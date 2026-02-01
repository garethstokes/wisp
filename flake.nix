{
  description = "Wisp - Personal Assistant";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell
            ghc
            cabal-install
            haskell-language-server
            haskellPackages.hspec-discover

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

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.postgresql
            pkgs.zlib
          ];
        };
      }
    );
}
