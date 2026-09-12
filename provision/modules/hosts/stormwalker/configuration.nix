{ inputs, self, ... }:
{
  flake.nixosConfigurations.stormwalker = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.modules.nixos.stormwalker
    ];
  };

  flake.modules.nixos.stormwalker =
    { config, ... }:
    let
      user = "${config.preferences.user}";
    in
    {
      imports = [
        self.modules.nixos.core
        self.modules.nixos.scripts

        self.modules.nixos.gnome

        self.modules.nixos.notes
        self.modules.nixos.programming

        self.modules.nixos.syncthing
      ];

      preferences.hostname = "stormwalker";

      # Use performance governor for sweet gaming performance!
      powerManagement.cpuFreqGovernor = "performance";

      # Set networking options
      networking.hostName = "stormwalker";
      networking.firewall.checkReversePath = "loose";
      networking.firewall.enable = false;

      # Needed for home-assistant commands to control stormwalker
      users.users."${user}".openssh.authorizedKeys.keyFiles = [
        ../../../secrets/ssh/pubs/hass.pub
      ];
    };
}
