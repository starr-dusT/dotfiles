{ pkgs, ... }:
{
  imports = [
    ./mqtt.nix
    ./automations.nix
  ];

  networking.firewall.allowedTCPPorts = [ 8123 ];

  environment.systemPackages = [ pkgs.python3Packages.pyvizio ];

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
      default_config = { };
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [
          "127.0.0.1"
          "69.69.1.10"
          "::1"
        ];
      };
      shell_command = {
        # Vizio TV controls
        living_room_tv_power_on = "${pkgs.python312Packages.pyvizio}/bin/pyvizio --ip=69.69.1.87 --auth=Zt93u2t4sq power on";
        living_room_tv_power_off = "${pkgs.python312Packages.pyvizio}/bin/pyvizio --ip=69.69.1.87 --auth=Zt93u2t4sq power off";
        living_room_tv_hdmi1 = "${pkgs.python312Packages.pyvizio}/bin/pyvizio --ip=69.69.1.87 --auth=Zt93u2t4sq input hdmi1";
        living_room_tv_hdmi2 = "${pkgs.python312Packages.pyvizio}/bin/pyvizio --ip=69.69.1.87 --auth=Zt93u2t4sq input hdmi2";

        # Switch video and audio for Kestrel
        kestrel_monitor_desk = "${pkgs.docker}/bin/docker run --rm kestrel_ssh -o 'StrictHostKeyChecking=no' tstarr@kestrel.lan 'display-switch.sh kestrel-desktop'";
        kestrel_audio_desk = "${pkgs.docker}/bin/docker run --rm kestrel_ssh -o 'StrictHostKeyChecking=no' tstarr@kestrel.lan 'sink-switch.sh Starship'";
        kestrel_monitor_living = "${pkgs.docker}/bin/docker run --rm kestrel_ssh -o 'StrictHostKeyChecking=no' tstarr@kestrel.lan 'display-switch.sh kestrel-living'";
        kestrel_audio_living = "${pkgs.docker}/bin/docker run --rm kestrel_ssh -o 'StrictHostKeyChecking=no' tstarr@kestrel.lan 'sink-switch.sh Dragon'";
      };
    };
  };
}
