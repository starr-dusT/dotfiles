{
  config,
  lib,
  ...
}:

let
  cfg = config.modules.optional.services.node-exporter;
in
{
  options.modules.optional.services.node-exporter.enable = lib.mkEnableOption "node-exporter";

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 9002 ];

    services.prometheus = {
      exporters = {
        node = {
          enable = true;
          enabledCollectors = [ "systemd" ];
          port = 9002;
        };
      };
    };
  };
}
