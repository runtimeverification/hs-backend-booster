{ stdenv, hs-backend-booster, rpc-client, git }:
stdenv.mkDerivation {
  name = "rpc-integration";
  src = ./.;
  preferLocalBuild = true;
  buildInputs = [ git ];

  enableParallelBuilding = true;
  buildPhase = ''
    export SERVER=${hs-backend-booster}/bin/hs-backend-booster
    export CLIENT=${rpc-client}/bin/rpc-client
    patchShebangs runDirectoryTest.sh
    ./runDirectoryTest.sh test-a-to-f
  '';
  installPhase = ''
    runHook preInstall
    touch "$out"
    runHook postInstall
  '';
}
