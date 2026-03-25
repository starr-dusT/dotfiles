{ ... }:
{
  flake.modules.nixos.kestrel =
    { ... }:
    {
      age.secrets."syncthing/key.pem".file = ../../../../../secrets/syncthing/kestrel/key.pem.age;
      age.secrets."syncthing/cert.pem".file = ../../../../../secrets/syncthing/kestrel/cert.pem.age;

      services.syncthing = {
        settings.devices = {
          "kruos" = {
            id = "VROOKEG-H75SHUK-FK2LRAI-DU77ULS-WB6ZMQ2-7AIE5SQ-IDZLU7Q-Z762ZQU";
          };
          "stormwalker" = {
            id = "OTPOWIB-MRGIDWA-SDEEHJF-OPYEK6M-3TWREYD-T4YAKI5-RXOOXLP-UHRGZAO";
          };
          "torus" = {
            id = "ZVABUCA-3SA5QKR-OZSCIS5-RDAHR2V-D4R4NFK-ZBYOKDP-6HQUG2M-BNL3DAO";
          };
        };
      };
    };
}
