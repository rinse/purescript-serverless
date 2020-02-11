{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-serverless"
, dependencies =
    [ "aff"
    , "aff-promise"
    , "console"
    , "effect"
    , "proxy"
    , "psci-support"
    , "spec"
    , "spec-discovery"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
