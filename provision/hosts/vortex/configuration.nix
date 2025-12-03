{
  pkgs,
  user,
  hostname,
  ...
}:
let
  interface = if "${hostname}" == "vortex-1" then "enp2s0" else "enp1s0f0";
in
{
  # Use normal kernel
  boot.kernelPackages = pkgs.linuxPackages_6_6;

  # Set networking options
  networking = {
    firewall.enable = false;
    useDHCP = false;
    defaultGateway = {
      address = "69.69.1.1";
      interface = "${interface}";
    };
    defaultGateway6 = {
      address = "fe80::1";
      interface = "${interface}";
    };
    nameservers = [
      "69.69.1.10"
    ];

    interfaces = {
      "${interface}" = {
        useDHCP = false;
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
