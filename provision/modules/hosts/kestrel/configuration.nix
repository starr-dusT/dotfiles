{ inputs, self, ... }:
{
  flake.nixosConfigurations.kestrel = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.modules.nixos.kestrel
    ];
  };

  flake.modules.nixos.kestrel =
    { config, ... }:
    {
      imports = [
        self.modules.nixos.core
        self.modules.nixos.scripts

        self.modules.nixos.gnome
        self.modules.nixos.kestrelGnome
        
        self.modules.nixos.engineering
        self.modules.nixos.notes
        self.modules.nixos.programming

        self.modules.nixos.emulation
        self.modules.nixos.pc

        self.modules.nixos.docker
        self.modules.nixos.virt-manager

        self.modules.nixos.syncthing
        self.modules.nixos.borgmatic
      ];

      preferences.hostname = "kestrel";

      # Use performance governor for sweet gaming performance!
      powerManagement.cpuFreqGovernor = "performance";

      # Set networking options
      networking.hostName = "kestrel";
      networking.firewall.checkReversePath = "loose";
      networking.firewall.enable = false;

      # Nvidia options
      services.xserver.videoDrivers = [ "nvidia" ];
      hardware.nvidia = {
        modesetting.enable = true;
        powerManagement.enable = false;
        nvidiaSettings = true;
        open = true;
        package = config.boot.kernelPackages.nvidiaPackages.stable;
      };

      # Wireguard clients
      age.secrets."wireguard/kestrel".file = ../../../secrets/wireguard/kestrel.age;
      environment.etc."NetworkManager/system-connections/kestrel.nmconnection" = {
        mode = "0600";
        source = config.age.secrets."wireguard/kestrel".path;
      };
    };
}
