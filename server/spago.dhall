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
    , "assert"
    , "erl-test-eunit" 
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs"   ] 
, backend = "purerl"
}
