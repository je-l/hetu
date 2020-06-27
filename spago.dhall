{ name = "hetulib"
, dependencies =
  [ "console"
  , "effect"
  , "exceptions"
  , "lists"
  , "psci-support"
  , "spec"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
