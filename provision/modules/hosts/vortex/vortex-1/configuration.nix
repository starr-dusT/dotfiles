{ inputs, self, ... }:
{
  flake.nixosConfigurations.vortex-1 = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.modules.nixos.vortex
      self.modules.nixos.vortex-1
    ];
  };

  flake.modules.nixos.vortex-1 =
    { ... }:
    let
      interface = "enp2s0";
      address4 = "69.69.1.11";
      address6 = "2607:9b00:620a:9c00:ca4a:2454:50c7:4e2e";
    in
    {
      imports = [

      ];

      preferences.hostname = "vortex-1";

      networking = {
        hostName = "vortex-1";
        defaultGateway = {
          interface = "${interface}";
        };
        defaultGateway6 = {
          interface = "${interface}";
        };
        interfaces = {
          "${interface}" = {
            useDHCP = false;
            ipv4.addresses = [
              {
                address = "${address4}";
                prefixLength = 24;
              }
            ];
            ipv6.addresses = [
              {
                address = "${address6}";
                prefixLength = 64;
              }
            ];
          };
        };
      };

      services.k3s = {
        clusterInit = true;
        serverAddr = "";
        nodeIP = "${address4},${address6}";
      };
    };
}
