{ inputs, self, ... }:
{
  flake.nixosConfigurations.vortex-2 = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.modules.nixos.vortex
      self.modules.nixos.vortex-2
    ];
  };

  flake.modules.nixos.vortex-2 =
    { ... }:
    let
      interface = "enp1s0f0";
      address4 = "69.69.1.12";
      address6 = "2607:9b00:620a:9c00:a3c1:32df:32fe:6418";
    in
    {
      imports = [

      ];

      preferences.hostname = "vortex-2";

      networking = {
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
