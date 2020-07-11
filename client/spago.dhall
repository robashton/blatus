{ sources = [ "src/**/*.purs" ]
, name = "arqiva_control"
, packages = ./packages.dhall
, dependencies = [
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
  "aff",
  "routing-duplex",
  "exists"    ]
}
