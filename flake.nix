{
  # Haskell.nix project used by `.github/workflows/haskell-nix.yml`

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: _prev: {
            networkProject =
              final.haskell-nix.cabalProject {
                name = "network";
                src = ./.;
                compiler-nix-name = "ghc9122";
                # Include Windows cross compilation
                crossPlatforms = p: [ p.ucrt64 ];
                modules = [{
                  # Run autoreconf on the source
                  packages.network.src = final.lib.mkForce (final.stdenv.mkDerivation {
                    pname = "network-src";
                    version = "1.0";
                    src = ./.;
                    nativeBuildInputs = [ final.autoreconfHook ];
                    installPhase = ''
                      mkdir -p $out
                      cp -r . $out
                    '';
                  });
                }];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.networkProject.flake {};
      in flake // {
        legacyPackages = pkgs;
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io" "https://cache.zw3rk.com"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="];
    allow-import-from-derivation = "true";
  };
}
