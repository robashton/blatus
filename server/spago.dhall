{-
-}
{ name = "demo"
, dependencies =
    [ "console"
    , "effect"
    , "sequences"
    , "erl-cowboy"
    , "erl-pinto"
    , "erl-stetson"
    , "erl-logger"
    , "psci-support"
    , "simple-json"
    , "erl-simplebus"
    , "random"
    , "st"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
