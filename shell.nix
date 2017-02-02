{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, Chart, Chart-diagrams, containers
      , diagrams, diagrams-contrib, diagrams-lib, diagrams-svg, fgl
      , interpolate, mtl, stdenv, SVGFonts
      }:
      mkDerivation {
        pname = "talks";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base Chart Chart-diagrams containers diagrams diagrams-contrib
          diagrams-lib diagrams-svg fgl interpolate mtl SVGFonts
        ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
