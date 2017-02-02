{ pkgs ? (import ./../pkgs.nix) {} }:

let
  hLib = import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; };
  haskellPackages = pkgs.haskell.packages.ghc801.override{
    overrides =
      let overrideAttrs = package: newAttrs: package.override (args: args // {
              mkDerivation = expr: args.mkDerivation (expr // newAttrs);
            });
      in self: super: {
          opaleye-gen = self.callPackage (
            haskellPackageGen { doFilter = false; } (
              pkgs.fetchFromGitHub{
                owner = "folsen";
                repo = "opaleye-gen";
                rev = "14938df0081187539f23f8547fb1b7762e286ac3";
                sha256 = "1xapgyhkn71m0arb06rv5b1cncz5gv9lybi3q4yavs8zh4jbkbn7";
              }
            )
          ) {};

          # TODO: remove after 6.0.0.1 is released
          elm-export = self.callPackage (
            haskellPackageGen { doFilter = false; } (
              pkgs.fetchFromGitHub{
                owner = "domenkozar";
                repo = "elm-export";
                rev = "227a8fea47b4eeb534095f6c23b72a71c9e1f617";
                sha256 = "05y134z1w6rsfw0xyl48z5z9j4ccdw7hgkl10fl1hjxy8vrsr2i8";
              }
            )
          ) {};
          # TODO remove once 0.4.0.0 is released
          servant-elm = hLib.dontCheck (self.callPackage (
            haskellPackageGen { doFilter = false; } (
              pkgs.fetchFromGitHub{
                owner = "domenkozar";
                repo = "servant-elm";
                rev = "86b3238441f8eb43c8a67919a62ca35a1c3d9405";
                sha256 = "1abqc0s8dp1wkc1p08ivilm7fc0nanqds1qvdjsy4zsy2s8cb7v9";
              }
            )
          ) {});

          #
          #
          # New versions for opaleye-gen
          #
          product-profunctors = overrideAttrs super.product-profunctors {
            src = pkgs.fetchFromGitHub {
              owner = "tomjaguarpaw";
              repo = "product-profunctors";
              rev = "1f14ce3f495cfaac292a342e9d67c3f2f753c914";
              sha256 = "0qchq0hky05w52wpz1b6rp3y7k6z3rs16kpab175j28nqf6h47f3";
            };
          };
          opaleye = overrideAttrs super.opaleye {
            editedCabalFile = null;
            revision = null;
            src = pkgs.fetchFromGitHub {
              owner = "tomjaguarpaw";
              repo = "haskell-opaleye";
              rev = "c068ba9d5da49735ade354717aa12e4e6d32ef9c";
              sha256 = "01xix0d3y1mcc291rdcsanby08964935nb8c8ahb33lsn4ch707h";
            };
          };
          countable-inflections = overrideAttrs super.countable-inflections {
            src = pkgs.fetchFromGitHub {
              owner = "folsen";
              repo = "countable-inflections";
              rev = "cb2f8285d3756e4a31d6c8130f07d265f706e23b";
              sha256 = "10s2nmyab3d0kdb12xmz904271lmcr25vn5h845hgqf6qy77bqhk";
            };
            libraryHaskellDepends = with self; [
              base bytestring exceptions pcre-light text pcre-utils regex-pcre-builtin
            ];
          };
          cases = overrideAttrs super.cases {
            jailbreak = true;
          };
        };
      };

  haskellPackageGen = { doHaddock ? false, doFilter ? true }: src:
    let filteredSrc = builtins.filterSource (n: t: t != "unknown") src;
        package = pkgs.runCommand "default.nix" {} ''
          ${pkgs.haskell.packages.ghc801.cabal2nix}/bin/cabal2nix \
            ${if doFilter then filteredSrc else src} \
            ${if doHaddock
                then ""
                else "--no-haddock"} \
            > $out
        '';
    in import package;
  f = haskellPackageGen {} ./.;

  drv = haskellPackages.callPackage f {};

  extraEnvPackages = with pkgs; with haskellPackages; [ opaleye-gen postgresql ];

  envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
    buildInputs = attrs.buildInputs ++ extraEnvPackages;
  });

in drv // { env = envWithExtras; }
