{ ... }:
{
  flake.modules.nixos.torus =
    { ... }:
    {
      age.secrets."syncthing/key.pem".file = ../../../../../secrets/syncthing/torus/key.pem.age;
      age.secrets."syncthing/cert.pem".file = ../../../../../secrets/syncthing/torus/cert.pem.age;

      services.syncthing = {
        settings.devices = {
          "kestrel" = {
            id = "5WWL4FE-ARZ4FHP-J33HQCH-CZKEXLN-2RAY4KW-PDI754F-3HVPZYI-VC3ESAF";
          };
          "stormwalker" = {
            id = "OTPOWIB-MRGIDWA-SDEEHJF-OPYEK6M-3TWREYD-T4YAKI5-RXOOXLP-UHRGZAO";
          };
        };
      };
    };
}
