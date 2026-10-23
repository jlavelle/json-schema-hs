module Main (main) where

import Test.Tasty qualified as Tasty

import Test qualified

main :: IO ()
main = Tasty.defaultMain =<< Test.tree
