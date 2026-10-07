{
  description = "Tools for obtaining optimal sugar cane farm layouts in Minecraft";

  inputs.nixpkgs.url = "https://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz";

  outputs =
    { nixpkgs, ... }:
    let
      inherit (nixpkgs) lib;
      eachDefaultSystem =
        function:
        lib.genAttrs [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ] (system: function nixpkgs.legacyPackages.${system});
    in
    {
      packages = eachDefaultSystem (pkgs: rec {
        default = sugarc;
        sugarc = pkgs.callPackage ./sugarc.nix { };
      });

      overlays.default = _final: prev: {
        sugarc = prev.callPackage ./sugarc.nix { };
      };

      devShells = eachDefaultSystem (pkgs: {
        default = pkgs.haskellPackages.shellFor {
          # Haskell Dependencies derived from the sugarc package
          packages = hpkgs: [ (hpkgs.callPackage ./sugarc.nix { }) ];

          # "Normal" packages
          nativeBuildInputs = builtins.attrValues {
            inherit (pkgs)
              ghc
              cabal-install
              ghcid
              haskell-language-server
              fourmolu
              hlint
              statix
              ;
          };
        };
      });
    };
}
