Canonical Draft 2020-12 metaschema and meta/* vocabulary documents.
Fetched 2026-07-24 with:

    for f in schema meta/core meta/applicator meta/unevaluated meta/validation meta/meta-data meta/format-annotation meta/content meta/format-assertion; do curl -fsSL "https://json-schema.org/draft/2020-12/$f" -o "$f.json"; done
