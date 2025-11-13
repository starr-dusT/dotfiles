{
  config,
  pkgs,
  user,
  ...
}:
{
  # Use normal kernel
  boot.kernelPackages = pkgs.linuxPackages;

  # Set networking options
  networking.firewall.enable = true;
  networking.firewall.checkReversePath = "loose";

  age.secrets."ssh/kestrel/id_ed25519.pub" = {
    file = ../../secrets/ssh/kestrel/id_ed25519.pub.age;
    owner = "${user}";
    group = "users";
  };

  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    config.age.secrets."ssh/kestrel/id_ed25519.pub".path
  ];

  # Modules
  modules = {
    core = {
      physical.enable = true;
    };
    optional = {
      development = {
        programming.enable = true;
      };
      programs = {
        k3s.enable = true;
      };
    };
  };
}
