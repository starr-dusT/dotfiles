{ inputs, self, ... }:
{
  flake.nixosConfigurations.vortex-3 = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.modules.nixos.vortex
      self.modules.nixos.vortex-3
    ];
  };

  flake.modules.nixos.vortex-3 =
    { ... }:
    let
      interface = "enp1s0f0";
      address4 = "69.69.1.13";
      address6 = "2607:9b00:620a:9c00:7385:15ff:6b2:d8de";
    in
    {
      imports = [

      ];

      preferences.hostname = "vortex-3";

      networking = {
        hostName = "vortex-3";
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
