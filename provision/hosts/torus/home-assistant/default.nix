{ config, lib, pkgs, user, ... }:
{
  imports = [
    ./mqtt.nix
    ./automations.nix
  ];

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
        kestrel_monitor_bedroom = "${pkgs.docker}/bin/docker run --rm kestrel_ssh -o 'StrictHostKeyChecking=no' tstarr@192.168.1.86 'smk'";
        kestrel_audio_bedroom = "${pkgs.docker}/bin/docker run --rm kestrel_ssh -o 'StrictHostKeyChecking=no' tstarr@192.168.1.86 sink-switch.sh Starship'";
        kestrel_monitor_living = "${pkgs.docker}/bin/docker run --rm kestrel_ssh -o 'StrictHostKeyChecking=no' tstarr@192.168.1.86 'sms'";
        kestrel_audio_living = "${pkgs.docker}/bin/docker run --rm kestrel_ssh -o 'StrictHostKeyChecking=no' tstarr@192.168.1.86 sink-switch.sh Dragon'";
      };
    };
  };
}
