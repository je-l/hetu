{ name = "hetu"
, dependencies =
  [ "console"
  , "datetime"
  , "effect"
  , "exceptions"
  , "format"
  , "formatters"
  , "lists"
  , "node-fs"
  , "node-fs-aff"
  , "parsing"
  , "psci-support"
  , "spec"
  , "strings"
  , "stringutils"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/je-l/hetu"
}
