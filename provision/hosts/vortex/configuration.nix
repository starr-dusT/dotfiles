{
  pkgs,
  user,
  hostname,
  ...
}:
{
  # Use normal kernel
  boot.kernelPackages = pkgs.linuxPackages_6_6;

  # Set networking options
  networking.firewall.enable = false;
  networking.firewall.checkReversePath = false;

  networking = {
    useDHCP = false;
    defaultGateway = {
      address = "69.69.1.1";
      interface = "enp1s0f0";
    };
    defaultGateway6 = {
      address = "fe80::1";
      interface = "enp1s0f0";
    };
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
        ipv6.addresses = [
          {
            address =
              if "${hostname}" == "vortex-1" then
                "2607:9b00:620a:9c00:ca4a:2454:50c7:4e2e"
              else if "${hostname}" == "vortex-2" then
                "2607:9b00:620a:9c00:a3c1:32df:32fe:6418"
              else
                "2607:9b00:620a:9c00:7385:15ff:6b2:d8de";
            prefixLength = 64;
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
