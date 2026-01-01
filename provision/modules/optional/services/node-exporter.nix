{
  config,
  lib,
  hostname,
  ...
}:

let
  cfg = config.modules.optional.services.node-exporter;
in
{
  options.modules.optional.services.node-exporter.enable = lib.mkEnableOption "node-exporter";

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [
      9002
      3031
    ];

    services.prometheus = {
      exporters = {
        node = {
          enable = true;
          enabledCollectors = [ "systemd" ];
          port = 9002;
        };
      };
    };

    services.promtail = {
      enable = true;
      configuration = {
        server = {
          http_listen_port = 3031;
          grpc_listen_port = 0;
        };
        positions = {
          filename = "/tmp/positions.yaml";
        };
        clients = [
          {
            url = "http://69.69.1.10:3030/loki/api/v1/push";
          }
        ];
        scrape_configs = [
          {
            job_name = "journal";
            journal = {
              max_age = "12h";
              labels = {
                job = "systemd-journal";
                host = "${hostname}";
              };
            };
            relabel_configs = [
              {
                source_labels = [ "__journal__systemd_unit" ];
                target_label = "unit";
              }
            ];
          }
        ];
      };
      # extraFlags
    };
  };
}
