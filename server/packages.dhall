let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.14.0-20210319/packages.dhall sha256:aa6d4bcef87080a6684464cfba5fb611b03c4a4372b1e08c77186de4a1f5b66f

let overrides =
      { erl-cowboy =
        { repo = "https://github.com/purerl/purescript-erl-cowboy.git"
        , version = "3efdf080b8f1eece411411068b93951b851284cc"
        , dependencies = [ "lists", "prelude" ]
        }
      }


let extras = {
       erl-simplebus =
          { dependencies =
              [ "erl-process"
              , "effect"
              ]
          , repo = "ssh://git@github.com/id3as/purescript-erl-simplebus.git"
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
        , version = "5a43ccd12e5ec60d3e1d68df6a981d31ed730f64"
        }
      }

in  upstream ⫽ overrides⫽ extras
