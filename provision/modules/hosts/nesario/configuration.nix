{ inputs, self, ... }:
{
  flake.nixosConfigurations.nesario = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.modules.nixos.nesario
    ];
  };

  flake.modules.nixos.nesario =
    { ... }:
    {
      imports = [
        self.modules.nixos.core
        self.modules.nixos.scripts
        self.modules.nixos.gnome
      ];

      preferences.hostname = "nesario";

      # Use performance governor for sweet gaming performance!
      powerManagement.cpuFreqGovernor = "performance";

      # Set networking options
      networking.hostName = "nesario";
      networking.firewall.checkReversePath = "loose";
      networking.firewall.enable = false;
    };
}
