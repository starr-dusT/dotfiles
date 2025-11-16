{
  pkgs,
  user,
  ...
}:
{
  # Use normal kernel
  boot.kernelPackages = pkgs.linuxPackages;

  # Set networking options
  networking.firewall.enable = false;
  networking.firewall.checkReversePath = false;
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

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
      programs = {
        k3s.enable = true;
      };
    };
  };
}
