{ config, lib, pkgs, user, ... }:
{
  imports = [
    ./mqtt.nix
    ./automations.nix
  ];

  environment.systemPackages = [ pkgs.python3Packages.pyvizio ];

  networking.firewall.allowedTCPPorts = [ 8123 ];
  networking.firewall.allowedUDPPorts = [ 8123 ];

  users.users.hass = {
    extraGroups = [ "docker" ];
  };

  services.home-assistant = {
    enable = true;
    extraComponents = [
      # Components required to complete the onboarding
      "esphome"
      "met"
      "radio_browser"
      "tasmota"
    ];
    config = {
      # Includes dependencies for a basic setup
      # https://www.home-assistant.io/integrations/default_config/
      default_config = {};
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [
          "127.0.0.1"
          "::1"
        ];
      };
      shell_command = {
        # Vizio TV controls
        living_room_tv_power_on = "${pkgs.python312Packages.pyvizio}/bin/pyvizio --ip=192.168.1.168 --auth=Zt93u2t4sq power on";
        living_room_tv_power_off = "${pkgs.python312Packages.pyvizio}/bin/pyvizio --ip=192.168.1.168 --auth=Zt93u2t4sq power off";
        living_room_tv_hdmi1 = "${pkgs.python312Packages.pyvizio}/bin/pyvizio --ip=192.168.1.168 --auth=Zt93u2t4sq input hdmi1";
        living_room_tv_hdmi2 = "${pkgs.python312Packages.pyvizio}/bin/pyvizio --ip=192.168.1.168 --auth=Zt93u2t4sq input hdmi2";

        # Switch video and audio for Kestrel and Stormwalker
        kestrel_monitor_bedroom = "${pkgs.docker}/bin/docker run --rm kestrel_ssh -o 'StrictHostKeyChecking=no' tstarr@192.168.1.86 '${pkgs.mutter}/bin/gdctl set --logical-monitor --primary --monitor DP-2 --mode 2560x1440@143.912 --scale 1 --logical-monitor --monitor DP-1 --mode 2560x1440@143.973 --scale 1 --left-of DP-2'";
        kestrel_audio_bedroom = "${pkgs.docker}/bin/docker run --rm kestrel_ssh -o 'StrictHostKeyChecking=no' tstarr@192.168.1.86 'ss Starship'";
        kestrel_monitor_living = "${pkgs.docker}/bin/docker run --rm kestrel_ssh -o 'StrictHostKeyChecking=no' tstarr@192.168.1.86 '${pkgs.mutter}/bin/gdctl set --logical-monitor --primary --monitor HDMI-1 --mode 2560x1440@59.951 --scale 1.5'";
        kestrel_audio_living = "${pkgs.docker}/bin/docker run --rm kestrel_ssh -o 'StrictHostKeyChecking=no' tstarr@192.168.1.86 'ss Dragon'";
      };
    };
  };

  services.nginx.virtualHosts."home.tstarr.us" = {
    forceSSL = true;
    enableACME = true;
    extraConfig = ''
      proxy_buffering off;
    '';
    locations."/".extraConfig = ''
      proxy_pass http://localhost:8123;
      proxy_set_header Host $host;
      proxy_redirect http:// https://;
      proxy_http_version 1.1;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header Upgrade $http_upgrade;
      proxy_set_header Connection $connection_upgrade;
    '';
  };
}
