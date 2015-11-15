{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, directory
      , filepath, HUnit, mtl, QuickCheck, stdenv
      }:
      mkDerivation {
        pname = "leveldb-hs";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring containers directory filepath mtl
        ];
        testHaskellDepends = [ base directory filepath HUnit QuickCheck ];
        description = "Pure Haskell implementation of LevelDB";
        license = stdenv.lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
