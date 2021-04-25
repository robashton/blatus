
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210329/packages.dhall sha256:32c90bbcd8c1018126be586097f05266b391f6aea9125cf10fba2292cb2b8c73

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
          , repo = "ssh://git@github.com/id3as/purescript-sequences.git"
          , version = "73fdb04afa32be8a3e3d1d37203592118d4307bc"
          }
      }

in  upstream // overrides // additions
