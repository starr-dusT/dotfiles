{
  pkgs,
  user,
  hostname,
  ...
}:
{
  # Use normal kernel
  boot.kernelPackages = pkgs.linuxPackages;

  # Set networking options
  networking.firewall.enable = false;
  networking.firewall.checkReversePath = false;

  networking = {
    useDHCP = false;
    defaultGateway = "69.69.1.1";
    nameservers = [
      "9.9.9.9"
      "8.8.8.8"
      "1.1.1.1"
    ];

    interfaces = {
      enp1s0f0.useDHCP = false;
      enp1s0f0 = {
        ipv4.addresses = [
          {
            address =
              if "${hostname}" == "vortex-1" then
                "69.69.1.11"
              else if "${hostname}" == "vortex-2" then
                "69.69.1.12"
              else
                "69.69.1.13";
            prefixLength = 24;
          }
        ];
      };
    };
  };

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
