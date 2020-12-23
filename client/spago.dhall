{ sources = [ "src/**/*.purs" ]
, name = "arqiva_control"
, packages = ./packages.dhall
, dependencies = [
  "debug",
  "console",
  "js-timers",
  "prelude",
  "simple-json",
  "web-socket",
  "signal",
  "filterable",
  "effect",
  "canvas",
  "web-html",
  "web-dom",
  "aff",
  "routing-duplex",
  "sequences",
  "debug",
  "st",
  "exists"    ]
}
