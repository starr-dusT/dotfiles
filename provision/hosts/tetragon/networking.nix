{ ... }:
{
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
  networking.firewall.allowedUDPPorts = [
    67
    68
  ];

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
            address = "69.69.1.10";
            prefixLength = 24;
          }
        ];
      };
    };
  };

  services.kea.dhcp4 = {
    enable = true;
    settings = {
      valid-lifetime = 3600;
      renew-timer = 900;
      rebind-timer = 1800;
      lease-database = {
        type = "memfile";
        persist = true;
        name = "/var/lib/kea/dhcp4.leases";
      };
      interfaces-config = {
        dhcp-socket-type = "raw";
        interfaces = [
          "enp1s0f0"
        ];
      };
      subnet4 = [
        {
          subnet = "69.69.1.1/24";
          pools = [
            {
              pool = "69.69.1.50 - 69.69.1.240";
            }
          ];
        }
      ];
      dhcp-range = [ "enp1s0f0,69.69.1.50,69.69.1.240,24h" ];
      interface = "enp1s0f0";
      dhcp-host = [ "69.69.1.10" ];
      port = 5353;

      local = "/lan/";
      domain = "lan";
      expand-hosts = true;
      no-hosts = true;
      address = [
        "/tetragon.lan/69.69.1.10"
        "/tv.lan/69.69.1.87"
        "/router.lan/69.69.1.1"
        "/v1.lan/69.69.1.11"
        "/v2.lan/69.69.1.12"
        "/v3.lan/69.69.1.13"
      ];
      dhcp-option = [ "option:router,69.69.1.1" ];
      dhcp-match = "set:efi64,60,PXEClient:Arch:00007";
      dhcp-boot = "tag:efi64,netboot.xyz.efi,,69.69.1.10";
    };
  };
}
