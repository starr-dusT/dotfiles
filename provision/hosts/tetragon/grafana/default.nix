{ ... }:
let
  mkJob = name: target: {
    job_name = name;
    static_configs = [
      {
        targets = [ target ];
      }
    ];
  };
in
{
  networking.firewall.allowedTCPPorts = [
    2342
    9001
    3030
  ];

  services.grafana = {
    enable = true;
    settings = {
      server = {
        http_port = 2342;
        http_addr = "0.0.0.0";
      };
      analytics = {
        reporting_enabled = false;
        check_for_updates = false;
        feedback_links_enabled = false;
      };
      plugins = {
        check_for_plugin_updates = false;
      };
    };
  };

  services.loki = {
    enable = true;
    configFile = ./loki.yaml;
  };

  services.prometheus = {
    enable = true;
    port = 9001;
    scrapeConfigs = [
      (mkJob "tetragon" "127.0.0.1:9002")
      (mkJob "vortex-1" "69.69.1.11:9002")
      (mkJob "vortex-2" "69.69.1.12:9002")
      (mkJob "vortex-3" "69.69.1.13:9002")
      (mkJob "torus" "69.69.1.14:9002")
    ];
  };
}
