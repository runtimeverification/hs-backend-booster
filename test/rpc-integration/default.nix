{nixosTest} : 
nixosTest {
  system = "x86_64-linux";
  name = "useless-test";
  # `foo` will be referenced in the test script
  nodes.foo = { ... }: {
    environment.systemPackages = [ pkgs.cowsay ];
  };
  # The test script, written in Python
  testScript = ''
    foo.start() # or start_all() for starting all nodes
    foo.succeed("cowthink $(whoami)")
  '';
}