{
  pkgs ? import <nixpkgs> { },
  lib ? pkgs.lib,
}:

let
  inherit (pkgs) haskellPackages;

  # sugarc library and executable shared dependencies
  baseDependencies = builtins.attrValues {
    inherit (haskellPackages)
      base
      ;
  };

  /*
    cabalVersionString :: Path -> String
    Parses the version string out of a cabal file.
  */
  cabalVersionString =
    file:
    let
      versionRegex = "^[[:space:]]*version:[[:space:]]*([0-9.]+)[[:space:]]*$";
      parseVersion =
        line:
        let
          m = builtins.match versionRegex line;
        in
        if m == null then null else builtins.head m;

      version = lib.pipe (builtins.readFile file) [
        (lib.splitString "\n")
        (map parseVersion)
        (lib.findFirst (version: version != null) null)
      ];
    in
    if version == null then throw "could not parse version from ${toString file}" else version;
in
haskellPackages.mkDerivation {
  pname = "sugarc";
  version = cabalVersionString ./sugarc.cabal;

  src = lib.cleanSource ./.;

  isLibrary = true;
  isExecutable = true;

  executableHaskellDepends = baseDependencies;
  libraryHaskellDepends = baseDependencies;

  license = lib.licenses.gpl3Plus;
  mainProgram = "sugarc";
}
