
let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.18-devel.tar.gz;

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "5ce67d6fa06f459dde59b4e930e11b89e229a4b1";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "0f28e40f9942c1449bfa0d400dee49a517102047";
    };

  purerlSupport =
    builtins.fetchGit {
      name = "purerl-support-packages";
      url = "https://github.com/id3as/nixpkgs-purerl-support.git";
      rev = "1338c5b08480b4a3e67d60aaf33492d224a46bcf";
    };

  etwasPackages =
    builtins.fetchGit {
      name = "id3as-etwas-packages";
      url = "https://github.com/id3as/etwas";
      rev = "289d841fa2fffccca266407764dc619cfae6a2fb";
    };


  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
        (import purerlSupport)
        (import "${etwasPackages}/overlay.nix")
      ];
    };

  erlangChannel = nixpkgs.nixerl.erlang-23-2-1;

  pls = nixpkgs.nodePackages.purescript-language-server.override {
    version = "0.15.0";
    src = builtins.fetchurl {
      url = "https://registry.npmjs.org/purescript-language-server/-/purescript-language-server-0.15.0.tgz";
    };
  };

in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [

    erlangChannel.erlang
    erlangChannel.rebar3
    erlangChannel.erlang-ls

    purerl-support.purescript-0-14-1
    purerl-support.spago-0-16-0
    purerl-support.dhall-json-1-5-0
    purerl-support.purty-7-0-0
    purerl-support.psa-0-8-2

    purerl.purerl-0-0-9
    pls

    etwas
    act
   ];
}
