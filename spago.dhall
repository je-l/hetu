{ name = "hetu"
, dependencies =
  [ "datetime"
  , "effect"
  , "format"
  , "node-fs-aff"
  , "parsing"
  , "psci-support"
  , "spec"
  , "strings"
  , "stringutils"
  , "aff"
  , "arrays"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "node-buffer"
  , "prelude"
  , "transformers"
  , "partial"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/je-l/hetu"
}
