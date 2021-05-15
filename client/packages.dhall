let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210506/packages.dhall sha256:d199e142515f9cc15838d8e6d724a98cd0ca776ceb426b7b36e841311643e3ef

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
