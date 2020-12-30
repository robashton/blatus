let
  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "e5f945b13b3f6a39ec9fbb66c9794b277dc32aa1";
    };

  nixpkgs =
    import pinnedNix {
      overlays = [];
    };

in

with nixpkgs;
stdenv.mkDerivation rec {
  pname = "pure_unit";
  version = "1.0";

  src = ./_build/default/rel/pure_unit;

  buildPhase = ''
    echo "Null"
  '';

  installPhase = ''
    if [ -n "$prefix" ]; then
        mkdir -p "$prefix"
    fi
    cp -r ./* $out/
  '';

  buildInputs = [
    # Erlang deps, apparently
    ncurses   
    openssl 
  ];
}
