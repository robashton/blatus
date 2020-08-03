let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.4-20191110/packages.dhall sha256:563a7f694e18e6399f7f6d01f5b7e3c3345781655d99945768f48e458feb93a4

let overrides = {=}

let additions =
      { sequences =
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
          }
      }

in  upstream // overrides // additions
