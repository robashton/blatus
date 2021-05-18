let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.14.1-20210426/packages.dhall sha256:19829ae168a3223e007cecfaaffcd5cf78e00e20974393ee1721a543872de75b

let overrides =
      { erl-cowboy =
        { repo = "https://github.com/purerl/purescript-erl-cowboy.git"
        , version = "3efdf080b8f1eece411411068b93951b851284cc"
        , dependencies = [ "lists", "prelude" ]
        },
        debug = 
        { repo = "https://github.com/robashton/purescript-debug.git"
        , version = "66e2e5e236ec0498d2ca0de304926d0a144cfddd"
        , dependencies = [ "prelude", "effect" ]
        } 
      }


let extras = {
       erl-simplebus =
          { dependencies =
              [ "erl-process"
              , "effect"
              ]
          , repo = "https://github.com/id3as/purescript-erl-simplebus.git"
          , version = "9e95648be2d75bc30268a3e214e1950c86680982"
          }
      , erl-logger =
        { dependencies = [ "prelude", "erl-atom", "erl-lists", "record" ]
        , repo = "https://github.com/id3as/purescript-erl-logger.git"
        , version = "200b15d498ce3b4d533cf5566edeaf7516730498"
        }
      , sequences =
        { dependencies =
          [ "prelude"
          , "unsafe-coerce"
          , "partial"
          , "unfoldable"
          , "lazy"
          , "arrays"
          , "profunctor"
          , "maybe"
          , "tuples"
          , "newtype"
          ]
        , repo = "https://github.com/id3as/purescript-sequences.git"
        , version = "73fdb04afa32be8a3e3d1d37203592118d4307bc"
        }
      , erl-stetson =
        { repo = "https://github.com/id3as/purescript-erl-stetson.git"
        , dependencies =
          [ "erl-atom"
          , "erl-binary"
          , "erl-lists"
          , "erl-maps"
          , "erl-tuples"
          , "erl-modules"
          , "erl-cowboy"
          , "erl-test-eunit"
          , "foreign"
          , "maybe"
          , "prelude"
          , "transformers"
          , "routing-duplex"
          ]
        , version = "a1cf52e4141b1d01de131860f63a2bbbf4b0f86e"
        }
      , erl-pinto =
        { repo = "https://github.com/id3as/purescript-erl-pinto.git"
        , dependencies =
          [ "erl-process"
          , "erl-lists"
          , "erl-atom"
          , "erl-tuples"
          , "erl-modules"
          , "foreign"
          ]
        , version = "41d424c8d39a6613099de86ac7c5b2fdd8df445d"
        }
      }

in  upstream ⫽ overrides⫽ extras
