with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, hdevtools, hlint, hasktags, hoogle, pandoc }:
             mkDerivation {
                pname = "bahug101";
                version = "0.0.0.0";
                buildTools = [ hdevtools hlint hasktags hoogle pandoc ];
                license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
