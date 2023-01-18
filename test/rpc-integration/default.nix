{ stdenv, lib, hs-backend-booster, rpc-client, git, k }:

let
  mkIntegrationTest = { name, buildInputs ? [ ], buildFlags ? [], clientArgs ? [] }:

    stdenv.mkDerivation {
      name = "rpc-integration-${name}";
      src = ./.;
      preferLocalBuild = true;
      buildInputs = [ git ] ++ buildInputs;

      enableParallelBuilding = true;
      buildPhase = ''
        ${lib.strings.concatMapStrings (f: "export ${f}\n") buildFlags}
        export SERVER=${hs-backend-booster}/bin/hs-backend-booster
        export CLIENT=${rpc-client}/bin/rpc-client

        patchShebangs runDirectoryTest.sh
        ./runDirectoryTest.sh test-${name} ${lib.strings.concatStringsSep " " clientArgs}
      '';
      installPhase = ''
        runHook preInstall
        touch "$out"
        runHook postInstall
      '';
    };

in {
  a-to-f = mkIntegrationTest { name = "a-to-f"; };
  imp = mkIntegrationTest {
    name = "imp";
    buildInputs = [ k ];
    buildFlags = ["MODULE=IMP" "SERVER_OPTS='-l Rewrite'"];
    clientArgs = [ "-O 'terminal-rules=[IMP.stop]'" ];
  };
}
