{ ... }:
{
  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Modules
  modules = {
    core = {
      physical.enable = true;
      plus.enable = true;
    };

    optional = {
      desktop = {
        enable = true;
        gnome.enable = true;
      };
      development = {
        notes.enable = true;
        programming.enable = true;
      };
      programs = {
        _plus.enable = true;
      };
      scripts.enable = true;
    };
  };
}
