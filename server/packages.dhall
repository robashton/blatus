let upstream = https://github.com/purerl/package-sets/releases/download/erl-0.13.6-20200402/packages.dhall sha256:5442e50aa76c20bd60b2770ab41c68bae80f6ec96f2df1cfaea310673de567d1

let overrides =
      { erl-cowboy =
          { dependencies = [ "erl-modules" ]
          , repo = "https://github.com/robashton/purescript-erl-cowboy.git"
          , version = "2e095b7fb6f7bc3602269fb142c00c7a9e090659"
          },

      erl-pinto =
          { dependencies = [ "erl-process" ]
          , repo = "ssh://git@github.com/id3as/purescript-erl-pinto.git"
          , version = "d94d1220fe0fd6bc6840e62bd14920e9ab30c234"
          },

      erl-stetson =
          { dependencies = ["erl-atom" , "erl-binary" , "erl-lists" , "erl-maps" , "erl-tuples" , "erl-modules" , "foreign" , "maybe" , "prelude" , "transformers" , "routing-duplex"]
          , repo = "ssh://git@github.com/id3as/purescript-erl-stetson.git"
          , version = "d584315bb669cd2a6b190e5dbcc193522406f15f"
          }
      }

let extras = {
       erl-simplebus =
          { dependencies =
              [ "erl-process"
              , "effect"
              ]
          , repo = "ssh://git@github.com/id3as/purescript-erl-simplebus.git"
          , version = "499883e219c9d828ad67cb68726c8e8c4335ff7b"
          },
       erl-logger =
          { dependencies =
              [ "record"
              ]
          , repo = "ssh://git@github.com/id3as/purescript-erl-logger.git"
          , version = "4966aba0f7a3579c1ff8646f0fb6747d49c241a7"
          },
       sequences =
          { dependencies =
              [ "prelude"
              , "unsafe-coerce"
              , "unfoldable"
              , "lazy"
              , "arrays"
              , "profunctor"
              , "maybe"
              , "tuples"
              , "newtype"
              ]
          , repo = "ssh://git@github.com/hdgarrood/purescript-sequences.git"
          , version = "448919594e979cfd2a45ebcde619c22a9fb984fc"
          },
       st =
          { dependencies =
              [ "effect"
              ]
          , repo = "ssh://git@github.com/robashton/purescript-st.git"
          , version = "463085478446dacb91ede00ad8bcfbd3f76bfec9"
          }
  }

in  upstream ⫽ overrides⫽ extras
