
let
  erlangReleases = builtins.fetchGit {
    name = "nixpkgs-nixerl";
    url = "https://github.com/nixerl/nixpkgs-nixerl";
    rev = "6321e5b8b6cfe4c13307a2d2093c4b6243e6ad53";
  }; #builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.15-devel.tar.gz;

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "e5f945b13b3f6a39ec9fbb66c9794b277dc32aa1";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "b3f10cd33107f220e4328f0222d3d026bf4f5f99";
    };

  purerlSupport =
    builtins.fetchGit {
      name = "purerl-support-packages";
      url = "git@github.com:id3as/nixpkgs-purerl-support.git";
      rev = "47a8bd6ff017dad2208f10dddf91f6f3258a09be";
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
    

in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [

    (nixerl.erlang-22-3.erlang.override { wxSupport = false; })
    (nixerl.erlang-22-3.rebar3.override { erlang = (nixpkgs.nixerl.erlang-22-3.erlang.override { wxSupport = false; }); })

    (purerl-support.erlang_ls-0-5-1.override { erlang = (nixpkgs.nixerl.erlang-22-3.erlang.override { wxSupport = false; }); })

    purerl-support.purescript-0-13-8
    purerl-support.spago-0-16-0
    purerl-support.dhall-json-1-5-0

    purerl.purerl-0-0-7

    etwas
   ];
}
