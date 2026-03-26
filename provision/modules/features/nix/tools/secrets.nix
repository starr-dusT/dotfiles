{ inputs, ... }:
{
  flake.modules.nixos.secrets =
    { pkgs, ... }:
    {
      imports = [
        inputs.agenix.nixosModules.default
      ];
      environment.systemPackages = [ inputs.agenix.packages.${pkgs.stdenv.hostPlatform.system}.default ];

      # Add user age key to identity path
      age.identityPaths = [
        "/etc/ssh/ssh_host_ed25519_key"
        "/etc/ssh/ssh_host_rsa_key"
      ];
    };
}
