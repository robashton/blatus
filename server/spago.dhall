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
    , "debug"
    , "psci-support"
    , "simple-json"
    , "erl-simplebus"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs"   ] 
, backend = "purerl"
}
