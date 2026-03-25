{ inputs, self, ... }:
{
  flake.nixosConfigurations.torus = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.modules.nixos.torus
    ];
  };

  flake.modules.nixos.torus =
    { pkgs, ... }:
    let
      interface = "enp3s0";
    in
    {
      imports = [
        self.modules.nixos.core
        self.modules.nixos.scripts

        self.modules.nixos.docker

        self.modules.nixos.syncthing
        self.modules.nixos.borgmatic
        self.modules.nixos.node-exporter
      ];

      preferences.hostname = "torus";

      # Use normal kernel
      boot.kernelPackages = pkgs.linuxPackages;

      # Set kernel modules
      boot.kernelModules = [ "sg" ];

      # Set networking options
      boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = true; # Needed for wireguard-server
      networking = {
        firewall.enable = false;
        firewall.checkReversePath = "loose";
        useDHCP = false;
        defaultGateway = {
          address = "69.69.1.1";
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
                address = "69.69.1.14";
                prefixLength = 24;
              }
            ];
          };
        };
      };

      services.xserver.videoDrivers = [ "nvidia" ];
      hardware.nvidia.open = false;
    };
}
