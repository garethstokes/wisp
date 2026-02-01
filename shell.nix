{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Haskell
    ghc
    cabal-install
    haskell-language-server

    # Database
    postgresql
    goose

    # Native dependencies for Haskell libs
    zlib
    pkg-config
  ];

  shellHook = ''
    echo "Wisp development shell"
    echo "  ghc:   $(ghc --version)"
    echo "  cabal: $(cabal --version | head -1)"
    echo "  goose: $(goose --version)"
  '';

  # For postgresql-simple and other native deps
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
    pkgs.postgresql
    pkgs.zlib
  ];
}
