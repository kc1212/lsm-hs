{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, b-tree, base, bytestring, containers
      , directory, filelock, filepath, HUnit, mtl, pipes, QuickCheck
      , random, stdenv
      }:
      mkDerivation {
        pname = "lsm-hs";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          b-tree base bytestring containers directory filelock filepath mtl
          pipes random
        ];
        testHaskellDepends = [ base directory filepath HUnit QuickCheck ];
        description = "Log-Structured Merge-Tree in Haskell";
        license = stdenv.lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
