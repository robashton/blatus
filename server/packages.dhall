let upstream =
      https://raw.githubusercontent.com/purerl/package-sets/erl-0.13.2-20190808/src/packages.dhall sha256:9bf5fa72c656bde6888aeb33cf93047cda65ab53c7e8f29cc37c4dcade5a89b1

let overrides =
      { erl-pinto =
          { dependencies = [ "erl-cowboy", "erl-process" ]
          , repo = "ssh://git@github.com/id3as/purescript-erl-pinto.git"
          , version = "eabf6db6b2aa49d69e5917b4ebb4ceb66d5489f3"
          }
      , simple-json =
          { dependencies =
              [ "assert"
              , "effect"
              , "erl-lists"
              , "erl-maps"
              , "exceptions"
              , "foldable-traversable"
              , "foreign"
              , "functions"
              , "generics-rep"
              , "globals"
              , "lists"
              , "nullable"
              , "ordered-collections"
              , "partial"
              , "prelude"
              , "random"
              , "tuples"
              , "record"
              , "strings"
              , "transformers"
              , "typelevel-prelude"
              , "variant"
              ]
          , repo = "ssh://git@github.com/purerl/purescript-simple-json.git"
          , version = "7ba3d3f1bc9165ebdce948cbecc076d13f964e59"
          }
      }

in  upstream ⫽ overrides
