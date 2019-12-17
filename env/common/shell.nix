
let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.2-devel.tar.gz;

  ashtonPackages =
    builtins.fetchGit {
      name = "ashton-packages";
      url = "git@github.com:robashton/nixpkgs-purerl-bits.git";
     rev = "48b50c4008c31946e64aec6b4cfa0e95e3b87d57";
    };
    
  pursPackages =
    builtins.fetchGit {
      name = "purerl-packages";
      url = "git@github.com:purerl/nixpkgs-purerl.git";
      rev = "73897ce89970ed125c09bbc6217d30a3f72d33a1";
    };

  nixpkgs =
    import <nixpkgs> {
      overlays = [
        (import erlangReleases)
        (import ashtonPackages)
        (import pursPackages)
      ];
    };

in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [

    nixerl.erlang-22-1-8.erlang
    nixerl.erlang-22-1-8.rebar3

    purerl.purerl-0-0-1
    psc-package

    ashton.purescript-0-13-5
    ashton.spago-0-12-1-0 
    ashton.dhall-json-1-5-0 
  ];
}

