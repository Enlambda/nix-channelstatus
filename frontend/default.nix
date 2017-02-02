{ backend ? (import ./../backend {})
, pkgs ? (import ./../pkgs.nix) {}
, backendURL ? "http://localhost:8081" }:

with pkgs;

stdenv.mkDerivation {
 name = "nix-channelstatus";

 src = ./.;

 buildInputs = [ elmPackages.elm elmPackages.elm-format nodejs ];

 patchPhase = ''
   patchShebangs node_modules/webpack
 '';

 buildBackend = "${backend}/bin/gen-elm src ${backendURL} && sed -i \"s@'@@g\" src/API.elm";
 runBackend = "${backend}/bin/serve-channelstatus";

 BACKEND_URL = backendURL;

 buildPhase = ''
   npm run build
 '';

 installPhase = ''
   mkdir $out
   cp -R dist/* $out/
 '';
}
