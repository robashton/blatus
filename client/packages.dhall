let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.4-20191110/packages.dhall sha256:563a7f694e18e6399f7f6d01f5b7e3c3345781655d99945768f48e458feb93a4

let overrides = {=}

let additions =
      { apigen =
          { dependencies =
              [ "assert"
              , "console"
              , "debug"
              , "effect"
              , "generics-rep"
              , "ordered-collections"
              , "prelude"
              , "proxy"
              , "psci-support"
              , "record"
              , "simple-json"
              , "strings"
              ]
          , repo =
              "ssh://git@github.com/id3as/apigen.git"
          , version =
              "v0.0.3"
          }
      }

in  upstream // overrides // additions
