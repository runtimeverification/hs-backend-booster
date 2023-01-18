{ inputs.nixpkgs.url =
    "github:NixOS/nixpkgs/e39a5efc4504099194032dfabdf60a0c4c78f181";

  outputs = { nixpkgs, ... }: {
    checks.aarch64-darwin.default =
      nixpkgs.legacyPackages.aarch64-darwin.nixosTest {
        name = "test";

        nodes.machine = {
          nixpkgs.pkgs = nixpkgs.legacyPackages.aarch64-linux.pkgs;

          virtualisation.host.pkgs =
            nixpkgs.legacyPackages.aarch64-darwin;
        };

        testScript = "";
      };
  };
}