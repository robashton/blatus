{ name = "pure-unit"
, backend = "purerl"
, dependencies =
    [
    "console",
    "erl-atom",
    "erl-binary",
    "erl-lists",
    "erl-tuples",
    "erl-maps",
    "erl-cowboy",
    "erl-process",
    "erl-stetson",
    "erl-pinto",
    "erl-lager",
    "datetime",
    "simple-json",
    "maybe",
    "prelude",
    "lists",
    "transformers",
    "tuples",
    "foldable-traversable",
    "exists"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

