{
  pkgs,
  user,
  ...
}:
{
  # Use normal kernel
  boot.kernelPackages = pkgs.linuxPackages_6_6;

  # Set networking options
  networking.firewall.enable = false;
  networking.firewall.checkReversePath = false;

  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    ../../secrets/ssh/pubs/kestrel.pub
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
    };
  };
}
