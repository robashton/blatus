{ sources = [ "src/**/*.purs", "src/*.purs" ]
, name = "blatus-client"
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
  "web-uievents",
  "aff",
  "routing-duplex",
  "random",
  "sequences",
  "debug",
  "st",
  "exists"    ]
}
