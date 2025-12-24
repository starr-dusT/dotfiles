{ ... }:
{
  networking.firewall.allowedTCPPorts = [
    2342
    9001
    3030
  ];

  services.grafana = {
    enable = true;
    port = 2342;
    addr = "0.0.0.0";
  };

  services.prometheus = {
    enable = true;
    port = 9001;
    scrapeConfigs = [
      {
        job_name = "tetragon";
        static_configs = [
          {
            targets = [ "127.0.0.1:9002" ];
          }
        ];
      }
      {
        job_name = "vortex-1";
        static_configs = [
          {
            targets = [ "69.69.1.11:9002" ];
          }
        ];
      }
      {
        job_name = "vortex-2";
        static_configs = [
          {
            targets = [ "69.69.1.12:9002" ];
          }
        ];
      }
      {
        job_name = "vortex-3";
        static_configs = [
          {
            targets = [ "69.69.1.13:9002" ];
          }
        ];
      }
      {
        job_name = "torus";
        static_configs = [
          {
            targets = [ "69.69.1.14:9002" ];
          }
        ];
      }
    ];
  };

  services.loki = {
    enable = true;
    configFile = ./loki.yaml;
  };
}
