{ ... }:
{
  imports = [
    ../kestrel/gnome.nix
  ];

  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Modules
  modules = {
    core = {
      physical.enable = true;
    };

    optional = {
      desktop = {
        enable = true;
        gnome.enable = true;
        gnome-remote-desktop.enable = true;
      };
      development = {
        notes.enable = true;
      };
      scripts.enable = true;
      services = {
        syncthing = {
          enable = true;
          keyPath = ../../secrets/syncthing/stormwalker/key.pem.age;
          certPath = ../../secrets/syncthing/stormwalker/cert.pem.age;
          devices = {
            "kestrel" = {
              id = "5WWL4FE-ARZ4FHP-J33HQCH-CZKEXLN-2RAY4KW-PDI754F-3HVPZYI-VC3ESAF";
            };
            "kruos" = {
              id = "VROOKEG-H75SHUK-FK2LRAI-DU77ULS-WB6ZMQ2-7AIE5SQ-IDZLU7Q-Z762ZQU";
            };
            "torus" = {
              id = "ZVABUCA-3SA5QKR-OZSCIS5-RDAHR2V-D4R4NFK-ZBYOKDP-6HQUG2M-BNL3DAO";
            };
          };
        };
      };
    };
  };
}
