{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, b-tree, base, bytestring, containers
      , directory, filelock, filepath, HUnit, mtl, pipes, QuickCheck
      , stdenv
      }:
      mkDerivation {
        pname = "leveldb-hs";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          b-tree base bytestring containers directory filelock filepath mtl
          pipes
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
