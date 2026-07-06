{ ... }:
{
  flake.modules.nixos.node-exporter =
    { config, ... }:
    {
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

      services.alloy.enable = true;
      environment.etc."alloy/config.alloy".text = ''
        loki.relabel "journal" {
          forward_to = []

          rule {
            source_labels = ["__journal__systemd_unit"]
            target_label  = "unit"
          }
        }

        loki.source.journal "read" {
          max_age       = "12h"
          relabel_rules = loki.relabel.journal.rules
          forward_to    = [loki.write.default.receiver]
          labels        = {
            job  = "systemd-journal",
            host = "${config.preferences.hostname}",
          }
        }

        loki.write "default" {
          endpoint {
            url = "http://69.69.1.10:3030/loki/api/v1/push"
          }
        }
      '';
    };
}
