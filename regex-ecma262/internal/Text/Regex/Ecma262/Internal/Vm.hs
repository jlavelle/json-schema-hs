module Text.Regex.Ecma262.Internal.Vm
  ( MemoStart (..)
  , matchProgram
  ) where

import Control.Monad qualified as Monad
import Control.Monad.ST qualified as ST
import Data.Bits qualified as Bits
import Data.Foldable qualified as Foldable
import Data.Primitive.Array qualified as Array
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Primitive.MutVar qualified as MutVar
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Word qualified as Word

import Text.Regex.Ecma262.Internal.Charset qualified as Charset
import Text.Regex.Ecma262.Internal.Compile qualified as Compile
import Text.Regex.Ecma262.Internal.Mode qualified as Mode
import Text.Regex.Ecma262.Internal.Utf8 qualified as Utf8

data MemoStart
  = MemoWhenWorthwhile
  | MemoFromTheStart
  deriving (Eq, Show)

matchProgram :: MemoStart -> Compile.Program -> Utf8.Bytes -> Int -> Int -> Bool
matchProgram memoStart program text lo hi =
  ST.runST $ do
    let
      groups = Compile.programGroups program
      loops = Compile.programLoops program
    regs <- PrimArray.newPrimArray registerCount
    PrimArray.setPrimArray regs 0 registerCount 0
    caps <- PrimArray.newPrimArray (2 * groups)
    PrimArray.setPrimArray caps 0 (2 * groups) (-1)
    tmp <- PrimArray.newPrimArray groups
    PrimArray.setPrimArray tmp 0 groups 0
    counts <- PrimArray.newPrimArray (2 * loops)
    PrimArray.setPrimArray counts 0 (2 * loops) 0
    initialStack <- PrimArray.newPrimArray 256
    stack <- MutVar.newMutVar initialStack
    emptyMemo <- ByteArray.newByteArray 0
    memo <- MutVar.newMutVar emptyMemo
    let
      env =
        Env
          { envInsts = Compile.programInsts program
          , envText = text
          , envLo = lo
          , envHi = hi
          , envUnits = Compile.programMode program == Mode.LegacyMode
          , envWidth = hi - lo + 1
          , envPoints = Compile.programPoints program
          , envMemoStart = memoStart
          , envRegs = regs
          , envCaps = caps
          , envTmp = tmp
          , envCounts = counts
          , envStack = stack
          , envMemo = memo
          }
    setReg env rSteps checkEvery
    setReg env rMemoOff (if Compile.programPoints program == 0 then 1 else 0)
    Monad.when (memoStart == MemoFromTheStart && Compile.programPoints program /= 0) (remember env)
    search env program

data Env s = Env
  { envInsts :: !(Array.Array Compile.Inst)
  , envText :: !Utf8.Bytes
  , envLo :: !Int
  , envHi :: !Int
  , envUnits :: !Bool
  , envWidth :: !Int
  , envPoints :: !Int
  , envMemoStart :: !MemoStart
  , envRegs :: !(PrimArray.MutablePrimArray s Int)
  , envCaps :: !(PrimArray.MutablePrimArray s Int)
  , envTmp :: !(PrimArray.MutablePrimArray s Int)
  , envCounts :: !(PrimArray.MutablePrimArray s Int)
  , envStack :: !(MutVar.MutVar s (PrimArray.MutablePrimArray s Int))
  , envMemo :: !(MutVar.MutVar s (ByteArray.MutableByteArray s))
  }

rSp, rSteps, rChecks, rMemoOn, rMemoOff, registerCount :: Int
rSp = 0
rSteps = 1
rChecks = 2
rMemoOn = 3
rMemoOff = 4
registerCount = 5

setReg :: Env s -> Int -> Int -> ST.ST s ()
setReg env =
  PrimArray.writePrimArray (envRegs env)
{-# INLINE setReg #-}

checkEvery :: Int
checkEvery = 4096

remember :: Env s -> ST.ST s ()
remember env = do
  checks <- reg env rChecks
  let
    size = memoWords env
  if size > maxMemoWords
    then setReg env rMemoOff 1
    else Monad.when
      ( checks * checkEvery >= size + memoStepsPerPosition * envWidth env
          || envMemoStart env == MemoFromTheStart
      )
      $ do
        memo <- ByteArray.newByteArray (8 * size)
        ByteArray.setByteArray memo 0 (8 * size) (0 :: Word.Word8)
        MutVar.writeMutVar (envMemo env) memo
        setReg env rMemoOn 1

reg :: Env s -> Int -> ST.ST s Int
reg env =
  PrimArray.readPrimArray (envRegs env)
{-# INLINE reg #-}

memoWords :: Env s -> Int
memoWords env =
  (envPoints env * Bits.shiftL (envWidth env) Compile.maxMemoLoops + 63) `div` 64

maxMemoWords :: Int
maxMemoWords = 1048576

memoStepsPerPosition :: Int
memoStepsPerPosition = 64

search :: Env s -> Compile.Program -> ST.ST s Bool
search env program
  | envHi env - envLo env < Compile.programNeed program = pure False
  | Compile.programAnchored program = run env (envLo env)
  | PrimArray.sizeofPrimArray (Compile.programPrefix program) > 0 = searchPrefix env program (envLo env)
  | otherwise = searchFirst env program (envLo env)

run :: Env s -> Int -> ST.ST s Bool
run env !start = do
  setReg env rSp 0
  step env 0 start

step :: Env s -> Int -> Int -> ST.ST s Bool
step env !pc !pos = do
  steps <- reg env rSteps
  let
    left = steps - 1
  setReg env rSteps left
  Monad.when (left < 0) (check env)
  exec env pc pos

check :: Env s -> ST.ST s ()
check env = do
  setReg env rSteps checkEvery
  on <- reg env rMemoOn
  off <- reg env rMemoOff
  Monad.when (on == 0 && off == 0) $ do
    checks <- reg env rChecks
    setReg env rChecks (checks + 1)
    remember env

exec :: Env s -> Int -> Int -> ST.ST s Bool
exec !env !pc !pos =
  let
    i = inst env pc
    hi = envHi env
    lo = envLo env
    next = step env (pc + 1)
    retry = backtrack env
    guarded condition = if condition then next pos else retry
   in
    case Compile.instOp i of
      Compile.OpChar ->
        if pos < hi
          then
            let
              p = charAt env pos
             in
              if Utf8.rune p == Compile.instR i then next (pos + Utf8.width p) else retry
          else retry
      Compile.OpSet ->
        if pos < hi
          then
            let
              c = byte env pos
             in
              if c < Utf8.runeSelf
                then
                  if Charset.asciiHas (Charset.charsetAscii (Compile.instSet i)) c
                    then next (pos + 1)
                    else retry
                else
                  let
                    p = decodeAt env pos
                   in
                    if Charset.contains (Compile.instSet i) (Utf8.rune p) then next (pos + Utf8.width p) else retry
          else retry
      Compile.OpLit -> do
        let
          lit = Compile.instLit i
          n = PrimArray.sizeofPrimArray lit
        addSteps env (Bits.shiftR n 3)
        if hi - pos >= n && equalLit env pos lit 0 then next (pos + n) else retry
      Compile.OpCharBack ->
        if pos > lo
          then
            let
              p = charBefore env pos
             in
              if Utf8.rune p == Compile.instR i then next (pos - Utf8.width p) else retry
          else retry
      Compile.OpSetBack ->
        if pos > lo
          then
            let
              p = charBefore env pos
             in
              if Charset.contains (Compile.instSet i) (Utf8.rune p) then next (pos - Utf8.width p) else retry
          else retry
      Compile.OpFail -> retry
      Compile.OpBol -> guarded (pos == lo)
      Compile.OpEol -> guarded (pos == hi)
      Compile.OpWordBoundary -> guarded (wordBefore env pos /= wordAt env pos)
      Compile.OpNotWordBoundary -> guarded (wordBefore env pos == wordAt env pos)
      Compile.OpLineStart -> guarded (pos == lo || lineTerminatorBefore env pos)
      Compile.OpLineEnd -> guarded (pos == hi || lineTerminatorAt env pos)
      Compile.OpWordBoundaryFold -> guarded (foldWordBefore env pos /= foldWordAt env pos)
      Compile.OpNotWordBoundaryFold -> guarded (foldWordBefore env pos == foldWordAt env pos)
      Compile.OpJmp -> step env (Compile.instX i) pos
      Compile.OpSplit -> do
        on <- memoOn env
        let
          pointed = on && Compile.instM i /= 0
        hasFailed <-
          if pointed
            then failedAt env i pos
            else pure False
        if hasFailed
          then retry
          else do
            Monad.when pointed $ do
              roomy <- markDepthAllows env
              Monad.when roomy $ do
                idx <- memoIndex env i pos
                push env eMark 0 idx 0
            let
              x = Compile.instX i
              y = Compile.instY i
              allowsX = allows env (Compile.instQuick (inst env x)) pos
              allowsY = allows env (Compile.instQuick (inst env y)) pos
            if not allowsY
              then if allowsX then step env x pos else retry
              else
                if not allowsX
                  then step env y pos
                  else do
                    push env eChoice y pos 0
                    step env x pos
      Compile.OpSaveTmp -> do
        let
          a = Compile.instA i
        old <- PrimArray.readPrimArray (envTmp env) a
        push env eTmp a old 0
        PrimArray.writePrimArray (envTmp env) a pos
        next pos
      Compile.OpCapture -> do
        let
          a = Compile.instA i
          caps = envCaps env
        start <- PrimArray.readPrimArray caps (2 * a)
        end <- PrimArray.readPrimArray caps (2 * a + 1)
        push env eCap a start end
        entered <- PrimArray.readPrimArray (envTmp env) a
        if Compile.instFlag i
          then do
            PrimArray.writePrimArray caps (2 * a) pos
            PrimArray.writePrimArray caps (2 * a + 1) entered
          else do
            PrimArray.writePrimArray caps (2 * a) entered
            PrimArray.writePrimArray caps (2 * a + 1) pos
        next pos
      Compile.OpClear -> do
        let
          caps = envCaps env
        addSteps env (Compile.instB i - Compile.instA i)
        Foldable.for_ [Compile.instA i .. Compile.instB i] $ \g -> do
          start <- PrimArray.readPrimArray caps (2 * g)
          Monad.when (start >= 0) $ do
            end <- PrimArray.readPrimArray caps (2 * g + 1)
            push env eCap g start end
            PrimArray.writePrimArray caps (2 * g) (-1)
            PrimArray.writePrimArray caps (2 * g + 1) (-1)
        next pos
      Compile.OpBackref -> do
        (from, upTo) <- groupSpan env i
        if from < 0
          then next pos
          else do
            let
              n = upTo - from
            addSteps env (Bits.shiftR n 3)
            if hi - pos >= n && equalSpans env pos from n then next (pos + n) else retry
      Compile.OpBackrefBack -> do
        (from, upTo) <- groupSpan env i
        if from < 0
          then next pos
          else do
            let
              n = upTo - from
            addSteps env (Bits.shiftR n 3)
            if pos - lo >= n && equalSpans env (pos - n) from n then next (pos - n) else retry
      Compile.OpBackrefFold -> backrefByChar env pc i pos Compile.Forward True
      Compile.OpBackrefUnits -> backrefByChar env pc i pos Compile.Forward False
      Compile.OpBackrefFoldBack -> backrefByChar env pc i pos Compile.Backward True
      Compile.OpBackrefUnitsBack -> backrefByChar env pc i pos Compile.Backward False
      Compile.OpLook ->
        if not (allows env (Compile.instQuick i) pos)
          then retry
          else do
            push env (if Compile.instFlag i then eLookNeg else eLook) (Compile.instX i) pos 0
            next pos
      Compile.OpLookOk -> do
        stack <- MutVar.readMutVar (envStack env)
        sp <- reg env rSp
        let
          findLook f = do
            kind <- PrimArray.readPrimArray stack f
            if kind == eLook then pure f else findLook (f - 4)
        f <- findLook (sp - 4)
        start <- PrimArray.readPrimArray stack (f + 2)
        let
          compact keep k
            | k >= sp = pure keep
            | otherwise = do
                kind <- PrimArray.readPrimArray stack k
                if kind >= eTmp
                  then do
                    PrimArray.copyMutablePrimArray stack keep stack k 4
                    compact (keep + 4) (k + 4)
                  else compact keep (k + 4)
        keep <- compact f (f + 4)
        setReg env rSp keep
        next start
      Compile.OpLookFail -> do
        stack <- MutVar.readMutVar (envStack env)
        sp <- reg env rSp
        let
          unwind e = do
            kind <- PrimArray.readPrimArray stack e
            if kind == eLookNeg
              then pure e
              else undo env stack e *> unwind (e - 4)
        e <- unwind (sp - 4)
        setReg env rSp e
        retry
      Compile.OpRep -> do
        reached <- rep env i pc pos 0 pos
        if reached >= 0 then next reached else retry
      Compile.OpRepLazy -> do
        let
          takeMin at n
            | n >= Compile.instMin i = (at, n)
            | otherwise =
                let
                  w = one env i at
                 in
                  if w == 0 then (at, n) else takeMin (at + w) (n + 1)
          (reached, taken) = takeMin pos 0
          more = taken /= Compile.instMax i
        addSteps env (Bits.shiftR taken 3)
        if taken /= Compile.instMin i
          then retry
          else do
            on <- memoOn env
            if on && Compile.instM i /= 0
              then do
                idx <- memoIndex env i reached
                hasFailed <- failed env idx
                if hasFailed
                  then do
                    Monad.when more (push env eRepLazy pc reached taken)
                    retry
                  else do
                    if more
                      then push env eRepLazy pc reached taken
                      else do
                        roomy <- markDepthAllows env
                        Monad.when roomy (push env eMark 0 idx 0)
                    next reached
              else do
                Monad.when more (push env eRepLazy pc reached taken)
                next reached
      Compile.OpLoopInit -> do
        let
          a = Compile.instA i
          counts = envCounts env
        count <- PrimArray.readPrimArray counts (2 * a)
        start <- PrimArray.readPrimArray counts (2 * a + 1)
        push env eCounter a count start
        PrimArray.writePrimArray counts (2 * a) 0
        PrimArray.writePrimArray counts (2 * a + 1) (-1)
        next pos
      Compile.OpLoop -> do
        let
          a = Compile.instA i
          low = Compile.instMin i
          high = Compile.instMax i
          left = hi - pos
          need = Compile.instNeed i
          iter = Compile.instIter i
        n <- PrimArray.readPrimArray (envCounts env) (2 * a)
        start <- PrimArray.readPrimArray (envCounts env) (2 * a + 1)
        if left < need
          || (n < low && iter /= 0 && left - need < (low - n) * iter)
          || (n > low && pos == start)
          then retry
          else
            if n < low
              then next pos
              else
                if high >= 0 && n >= high
                  then step env (Compile.instX i) pos
                  else do
                    on <- memoOn env
                    let
                      pointed = on && Compile.instM i /= 0
                    hasFailed <-
                      if pointed
                        then failedAt env i pos
                        else pure False
                    if hasFailed
                      then retry
                      else do
                        Monad.when pointed $ do
                          roomy <- markDepthAllows env
                          Monad.when roomy $ do
                            idx <- memoIndex env i pos
                            push env eMark 0 idx 0
                        let
                          x = Compile.instX i
                        if not (allows env (Compile.instQuick (inst env x)) pos)
                          then next pos
                          else
                            if not (allows env (Compile.instQuick (inst env (pc + 1))) pos)
                              then step env x pos
                              else
                                if Compile.instFlag i
                                  then do
                                    push env eChoice (pc + 1) pos 0
                                    step env x pos
                                  else do
                                    push env eChoice x pos 0
                                    next pos
      Compile.OpLoopIter -> do
        let
          a = Compile.instA i
          counts = envCounts env
        count <- PrimArray.readPrimArray counts (2 * a)
        start <- PrimArray.readPrimArray counts (2 * a + 1)
        push env eCounter a count start
        PrimArray.writePrimArray counts (2 * a) (count + 1)
        PrimArray.writePrimArray counts (2 * a + 1) pos
        next pos
      Compile.OpMatch -> pure True

inst :: Env s -> Int -> Compile.Inst
inst env =
  Array.indexArray (envInsts env)
{-# INLINE inst #-}

backtrack :: Env s -> ST.ST s Bool
backtrack env = do
  sp <- reg env rSp
  if sp == 0
    then pure False
    else do
      stack <- MutVar.readMutVar (envStack env)
      let
        e = sp - 4
        field k = PrimArray.readPrimArray stack (e + k)
        resume = do
          setReg env rSp e
          pc <- field 1
          pos <- field 2
          step env pc pos
        resumeEntry kind
          | kind == eChoice = resume
          | kind == eRep = backtrackRep env stack e
          | kind == eRepLazy = backtrackRepLazy env stack e
          | kind == eMark = do
              idx <- field 2
              setFailed env idx
              setReg env rSp e
              backtrack env
          | kind == eLook = setReg env rSp e *> backtrack env
          | kind == eLookNeg = resume
          | otherwise = do
              undo env stack e
              setReg env rSp e
              backtrack env
      kind <- field 0
      resumeEntry kind

eChoice, eRep, eRepLazy, eLook, eLookNeg, eMark, eTmp, eCap, eCounter :: Int
eChoice = 0
eRep = 1
eRepLazy = 2
eLook = 3
eLookNeg = 4
eMark = 5
eTmp = 6
eCap = 7
eCounter = 8

backtrackRep :: Env s -> PrimArray.MutablePrimArray s Int -> Int -> ST.ST s Bool
backtrackRep env stack !e = do
  pc <- PrimArray.readPrimArray stack (e + 1)
  stood <- PrimArray.readPrimArray stack (e + 2)
  floorPos <- PrimArray.readPrimArray stack (e + 3)
  let
    loop = inst env (pc - 1)
  on <- memoOn env
  let
    marked = on && Compile.instM loop /= 0
  Monad.when marked (setFailedAt env loop stood)
  let
    w
      | byte env (stood - 1) < Utf8.runeSelf = 1
      | envUnits env = Utf8.width (Utf8.unitBefore (envText env) (envLo env) stood (envHi env))
      | otherwise = Utf8.width (Utf8.decodeLast (envText env) floorPos stood)
    pos = stood - w
    lastOne = pos <= floorPos
    passOver = do
      Monad.when lastOne (setReg env rSp e)
      addSteps env 1
      backtrack env
  PrimArray.writePrimArray stack (e + 2) pos
  if not (allows env (Compile.instQuick (inst env pc)) pos)
    then passOver
    else do
      hasFailed <-
        if marked
          then failedAt env loop pos
          else pure False
      if hasFailed
        then passOver
        else do
          Monad.when lastOne
            $ if marked
              then do
                idx <- memoIndex env loop pos
                PrimArray.writePrimArray stack e eMark
                PrimArray.writePrimArray stack (e + 2) idx
              else setReg env rSp e
          step env pc pos

memoOn :: Env s -> ST.ST s Bool
memoOn env =
  fmap (/= 0) (reg env rMemoOn)
{-# INLINE memoOn #-}

setFailedAt :: Env s -> Compile.Inst -> Int -> ST.ST s ()
setFailedAt env i pos = do
  idx <- memoIndex env i pos
  setFailed env idx

memoIndex :: Env s -> Compile.Inst -> Int -> ST.ST s Int
memoIndex env i !pos = do
  let
    base = Bits.shiftL ((Compile.instM i - 1) * envWidth env + (pos - envLo env)) Compile.maxMemoLoops
    loopBit l least bit idx =
      if l < 0
        then pure idx
        else do
          count <- PrimArray.readPrimArray (envCounts env) (2 * l)
          start <- PrimArray.readPrimArray (envCounts env) (2 * l + 1)
          pure (if count > least && start == pos then idx Bits..|. bit else idx)
  withOuter <- loopBit (Compile.instMl0 i) (Compile.instMm0 i) 1 base
  loopBit (Compile.instMl1 i) (Compile.instMm1 i) 2 withOuter

setFailed :: Env s -> Int -> ST.ST s ()
setFailed env idx = do
  memo <- MutVar.readMutVar (envMemo env)
  b <- readMemoByte memo (Bits.shiftR idx 3)
  ByteArray.writeByteArray memo (Bits.shiftR idx 3) (Bits.setBit b (idx Bits..&. 7))

readMemoByte :: ByteArray.MutableByteArray s -> Int -> ST.ST s Word.Word8
readMemoByte =
  ByteArray.readByteArray

byte :: Env s -> Int -> Int
byte env =
  Utf8.byte (envText env)
{-# INLINE byte #-}

addSteps :: Env s -> Int -> ST.ST s ()
addSteps env n = do
  steps <- reg env rSteps
  setReg env rSteps (steps - n)
{-# INLINE addSteps #-}

allows :: Env s -> Compile.Quick -> Int -> Bool
allows env q pos
  | pos >= envHi env = Compile.quickAtEnd q
  | Compile.quickMid q = True
  | otherwise =
      let
        c = byte env pos
       in
        if c < Utf8.runeSelf then Charset.asciiHas (Compile.quickAscii q) c else Compile.quickOther q
{-# INLINE allows #-}

failedAt :: Env s -> Compile.Inst -> Int -> ST.ST s Bool
failedAt env i pos = do
  idx <- memoIndex env i pos
  failed env idx

failed :: Env s -> Int -> ST.ST s Bool
failed env idx = do
  memo <- MutVar.readMutVar (envMemo env)
  b <- readMemoByte memo (Bits.shiftR idx 3)
  pure (Bits.testBit b (idx Bits..&. 7))

backtrackRepLazy :: Env s -> PrimArray.MutablePrimArray s Int -> Int -> ST.ST s Bool
backtrackRepLazy env stack !e = do
  pc <- PrimArray.readPrimArray stack (e + 1)
  stood <- PrimArray.readPrimArray stack (e + 2)
  taken <- PrimArray.readPrimArray stack (e + 3)
  let
    loop = inst env pc
  on <- memoOn env
  let
    marked = on && Compile.instM loop /= 0
  Monad.when marked (setFailedAt env loop stood)
  let
    w = one env loop stood
  if w == 0
    then do
      setReg env rSp e
      backtrack env
    else do
      let
        pos = stood + w
        count = taken + 1
        lastOne = count == Compile.instMax loop
      PrimArray.writePrimArray stack (e + 2) pos
      PrimArray.writePrimArray stack (e + 3) count
      hasFailed <-
        if marked
          then failedAt env loop pos
          else pure False
      if hasFailed
        then do
          Monad.when lastOne (setReg env rSp e)
          addSteps env 1
          backtrack env
        else do
          Monad.when lastOne
            $ if marked
              then do
                idx <- memoIndex env loop pos
                PrimArray.writePrimArray stack e eMark
                PrimArray.writePrimArray stack (e + 2) idx
              else setReg env rSp e
          step env (pc + 1) pos

one :: Env s -> Compile.Inst -> Int -> Int
one env i !pos =
  if pos >= envHi env
    then 0
    else
      let
        p = charAt env pos
       in
        if instMatches i (Utf8.rune p) then Utf8.width p else 0

charAt :: Env s -> Int -> Int
charAt env pos =
  let
    c = byte env pos
   in
    if c < Utf8.runeSelf then Utf8.packed c 1 else decodeAt env pos
{-# INLINE charAt #-}

decodeAt :: Env s -> Int -> Int
decodeAt env pos =
  if envUnits env
    then Utf8.unitAt (envText env) (envLo env) pos (envHi env)
    else Utf8.decode (envText env) pos (envHi env)
{-# INLINE decodeAt #-}

instMatches :: Compile.Inst -> Int -> Bool
instMatches i r =
  if Compile.instHasSet i then Charset.contains (Compile.instSet i) r else r == Compile.instR i
{-# INLINE instMatches #-}

undo :: Env s -> PrimArray.MutablePrimArray s Int -> Int -> ST.ST s ()
undo env stack e = do
  kind <- PrimArray.readPrimArray stack e
  pc <- PrimArray.readPrimArray stack (e + 1)
  pos <- PrimArray.readPrimArray stack (e + 2)
  a <- PrimArray.readPrimArray stack (e + 3)
  let
    restorePair array = do
      PrimArray.writePrimArray array (2 * pc) pos
      PrimArray.writePrimArray array (2 * pc + 1) a
    restore
      | kind == eTmp = PrimArray.writePrimArray (envTmp env) pc pos
      | kind == eCap = restorePair (envCaps env)
      | kind == eCounter = restorePair (envCounts env)
      | otherwise = pure ()
  restore

equalLit :: Env s -> Int -> PrimArray.PrimArray Int -> Int -> Bool
equalLit env !from lit !k =
  k == PrimArray.sizeofPrimArray lit
    || (byte env (from + k) == PrimArray.indexPrimArray lit k && equalLit env from lit (k + 1))

charBefore :: Env s -> Int -> Int
charBefore env pos =
  let
    c = byte env (pos - 1)
   in
    if c < Utf8.runeSelf then Utf8.packed c 1 else decodeBefore env pos
{-# INLINE charBefore #-}

decodeBefore :: Env s -> Int -> Int
decodeBefore env pos =
  if envUnits env
    then Utf8.unitBefore (envText env) (envLo env) pos (envHi env)
    else Utf8.decodeLast (envText env) (envLo env) pos
{-# INLINE decodeBefore #-}

wordBefore :: Env s -> Int -> Bool
wordBefore env pos =
  pos > envLo env && Charset.isWordByte (byte env (pos - 1))

wordAt :: Env s -> Int -> Bool
wordAt env pos =
  pos < envHi env && Charset.isWordByte (byte env pos)

lineTerminatorBefore :: Env s -> Int -> Bool
lineTerminatorBefore env pos =
  let
    c = byte env (pos - 1)
   in
    c == 0x0A
      || c == 0x0D
      || ((c == 0xA8 || c == 0xA9) && pos - envLo env >= 3 && byte env (pos - 3) == 0xE2 && byte env (pos - 2) == 0x80)

lineTerminatorAt :: Env s -> Int -> Bool
lineTerminatorAt env pos =
  let
    c = byte env pos
   in
    c == 0x0A
      || c == 0x0D
      || ( c == 0xE2
             && pos + 2 < envHi env
             && byte env (pos + 1) == 0x80
             && (byte env (pos + 2) == 0xA8 || byte env (pos + 2) == 0xA9)
         )

foldWordBefore :: Env s -> Int -> Bool
foldWordBefore env pos =
  pos > envLo env && Charset.isFoldWordRune (Utf8.rune (Utf8.decodeLast (envText env) (envLo env) pos))

foldWordAt :: Env s -> Int -> Bool
foldWordAt env pos =
  pos < envHi env && Charset.isFoldWordRune (Utf8.rune (Utf8.decode (envText env) pos (envHi env)))

markDepthAllows :: Env s -> ST.ST s Bool
markDepthAllows env =
  fmap (< 4 * maxMarkDepth) (reg env rSp)

maxMarkDepth :: Int
maxMarkDepth = maxStack `div` 2

maxStack :: Int
maxStack = 1048576

push :: Env s -> Int -> Int -> Int -> Int -> ST.ST s ()
push env !kind !pc !pos !a = do
  sp <- reg env rSp
  current <- MutVar.readMutVar (envStack env)
  size <- PrimArray.getSizeofMutablePrimArray current
  stack <-
    if sp + 4 > size
      then do
        grown <- PrimArray.newPrimArray (2 * size + 256)
        PrimArray.copyMutablePrimArray grown 0 current 0 sp
        MutVar.writeMutVar (envStack env) grown
        pure grown
      else pure current
  PrimArray.writePrimArray stack sp kind
  PrimArray.writePrimArray stack (sp + 1) pc
  PrimArray.writePrimArray stack (sp + 2) pos
  PrimArray.writePrimArray stack (sp + 3) a
  setReg env rSp (sp + 4)

groupSpan :: Env s -> Compile.Inst -> ST.ST s (Int, Int)
groupSpan env i = do
  from <- PrimArray.readPrimArray (envCaps env) (2 * Compile.instA i)
  upTo <- PrimArray.readPrimArray (envCaps env) (2 * Compile.instA i + 1)
  pure (from, upTo)
{-# INLINE groupSpan #-}

equalSpans :: Env s -> Int -> Int -> Int -> Bool
equalSpans env !a !b !n =
  n == 0 || (byte env a == byte env b && equalSpans env (a + 1) (b + 1) (n - 1))

backrefByChar :: Env s -> Int -> Compile.Inst -> Int -> Compile.Direction -> Bool -> ST.ST s Bool
backrefByChar env !pc i !pos direction fold = do
  (from, upTo) <- groupSpan env i
  if from < 0
    then step env (pc + 1) pos
    else do
      addSteps env (Bits.shiftR (upTo - from) 3)
      let
        reached =
          case direction of
            Compile.Forward -> backrefChars env fold (Compile.instFlag i) from upTo pos
            Compile.Backward -> backrefCharsBack env fold (Compile.instFlag i) from upTo pos
      if reached >= 0 then step env (pc + 1) reached else backtrack env

backrefChars :: Env s -> Bool -> Bool -> Int -> Int -> Int -> Int
backrefChars env fold unicode !from !upTo !pos
  | from >= upTo = pos
  | pos >= envHi env = -1
  | otherwise =
      let
        want =
          if envUnits env
            then Utf8.unitAt (envText env) (envLo env) from (envHi env)
            else Utf8.decode (envText env) from upTo
        got = decodeAt env pos
        w = Utf8.rune want
        g = Utf8.rune got
       in
        if g == w || (fold && canonicalizeFor unicode g == canonicalizeFor unicode w)
          then backrefChars env fold unicode (from + Utf8.width want) upTo (pos + Utf8.width got)
          else -1

canonicalizeFor :: Bool -> Int -> Int
canonicalizeFor unicode =
  Charset.canonicalize (if unicode then Mode.UnicodeMode else Mode.LegacyMode)

backrefCharsBack :: Env s -> Bool -> Bool -> Int -> Int -> Int -> Int
backrefCharsBack env fold unicode !from !upTo !pos
  | from >= upTo = pos
  | pos <= envLo env = -1
  | otherwise =
      let
        want =
          if envUnits env
            then Utf8.unitBefore (envText env) (envLo env) upTo (envHi env)
            else Utf8.decodeLast (envText env) from upTo
        got = decodeBefore env pos
        w = Utf8.rune want
        g = Utf8.rune got
       in
        if g == w || (fold && canonicalizeFor unicode g == canonicalizeFor unicode w)
          then backrefCharsBack env fold unicode from (upTo - Utf8.width want) (pos - Utf8.width got)
          else -1

rep :: Env s -> Compile.Inst -> Int -> Int -> Int -> Int -> ST.ST s Int
rep env !i !pc !pos !n !floorPos =
  if n /= Compile.instMax i && pos < envHi env
    then
      let
        c = byte env pos
        w
          | c < Utf8.runeSelf =
              if (if Compile.instHasSet i then Charset.asciiHas (Charset.charsetAscii (Compile.instSet i)) c else c == Compile.instR i)
                then 1
                else 0
          | otherwise =
              let
                p = decodeAt env pos
               in
                if instMatches i (Utf8.rune p) then Utf8.width p else 0
       in
        if w == 0
          then repDone env i pc pos n floorPos
          else
            let
              next = pos + w
              taken = n + 1
             in
              rep env i pc next taken (if taken == Compile.instMin i then next else floorPos)
    else repDone env i pc pos n floorPos

repDone :: Env s -> Compile.Inst -> Int -> Int -> Int -> Int -> ST.ST s Int
repDone env !i !pc !pos !n !floorPos = do
  addSteps env (Bits.shiftR n 3)
  if n < Compile.instMin i
    then pure (-1)
    else do
      on <- memoOn env
      if on && Compile.instM i /= 0
        then do
          idx <- memoIndex env i pos
          hasFailed <- failed env idx
          if hasFailed
            then do
              Monad.when (pos > floorPos) (push env eRep (pc + 1) pos floorPos)
              pure (-1)
            else do
              if pos > floorPos
                then push env eRep (pc + 1) pos floorPos
                else do
                  roomy <- markDepthAllows env
                  Monad.when roomy (push env eMark 0 idx 0)
              pure pos
        else do
          Monad.when (pos > floorPos) (push env eRep (pc + 1) pos floorPos)
          pure pos

searchPrefix :: Env s -> Compile.Program -> Int -> ST.ST s Bool
searchPrefix env program !from =
  let
    prefix = Compile.programPrefix program
    n = PrimArray.sizeofPrimArray prefix
    hi = envHi env
    first = PrimArray.indexPrimArray prefix 0
    indexOf at
      | at > hi - n = -1
      | byte env at == first && equalLit env at prefix 1 = at
      | otherwise = indexOf (at + 1)
    found = indexOf from
   in
    if found < 0 || hi - found < Compile.programNeed program
      then pure False
      else do
        matched <- run env found
        if matched then pure True else searchPrefix env program (found + 1)

searchFirst :: Env s -> Compile.Program -> Int -> ST.ST s Bool
searchFirst env program !from
  | envHi env - from < Compile.programNeed program = pure False
  | from == envHi env =
      case Compile.programFirst program of
        Nothing -> run env from
        Just _ -> pure False
  | otherwise = do
      let
        p = charAt env from
        candidate = all (`Charset.contains` Utf8.rune p) (Compile.programFirst program)
      matched <- if candidate then run env from else pure False
      if matched then pure True else searchFirst env program (from + Utf8.width p)
