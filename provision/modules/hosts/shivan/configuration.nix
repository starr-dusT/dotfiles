{ inputs, self, ... }:
{
  flake.nixosConfigurations.shivan = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.modules.nixos.shivan
    ];
  };

  flake.modules.nixos.shivan =
    { ... }:
    {
      imports = [
        self.modules.nixos.core
        self.modules.nixos.scripts
        self.modules.nixos.gnome

        self.modules.nixos.notes
        self.modules.nixos.programming
      ];

      preferences.hostname = "shivan";

      # Use performance governor for sweet gaming performance!
      powerManagement.cpuFreqGovernor = "performance";

      # Set networking options
      networking.hostName = "shivan";
      networking.firewall.checkReversePath = "loose";
      networking.firewall.enable = false;
    };
}
