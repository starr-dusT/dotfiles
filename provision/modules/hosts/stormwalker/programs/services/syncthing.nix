{ ... }:
{
  flake.modules.nixos.stormwalker =
    { ... }:
    {
      age.secrets."syncthing/key.pem".file = ../../../../../secrets/syncthing/stormwalker/key.pem.age;
      age.secrets."syncthing/cert.pem".file = ../../../../../secrets/syncthing/stormwalker/cert.pem.age;

      services.syncthing = {
        settings.devices = {
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
}
