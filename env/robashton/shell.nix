let
  ashtonPackages =
    builtins.fetchGit {
      name = "ashton-packages";
      url = https://github.com/robashton/nixpkgs-ashton/;
      rev = "4a34f189b8d793cc89f578b82687491d9d511fe9";
    };

  nixPackages =
    import <nixpkgs> {
      overlays = [
        (import ashtonPackages)
      ];
    };

in

with nixPackages;

stdenv.mkDerivation {
  name = "robashton.pure_unit";
  buildInputs = [
    pkgs.devPackages.erlang-22-0-1.erlang
    pkgs.devPackages.erlang-22-0-1.rebar3-9
    pkgs.devPackages.purerl-0-13-2
    pkgs.psc-package
  ];
}

