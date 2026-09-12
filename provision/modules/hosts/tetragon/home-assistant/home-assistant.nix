{ ... }:
{
  flake.modules.nixos.tetragon =
    { config, pkgs, ... }:
    {
      networking.firewall.allowedTCPPorts = [ 8123 ];

      environment.systemPackages = [ pkgs.python3Packages.pyvizio ];

      # Hass ssh key for kestrel
      age.secrets."ssh/hass" = {
        file = ../../../../secrets/ssh/hass.age;
        owner = "hass";
        group = "hass";
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
          "automation ui" = "!include automations.yaml";
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
            living_room_tv_power_on = "${pkgs.python312Packages.pyvizio}/bin/pyvizio --ip=69.69.1.150 --auth=Zt93u2t4sq power on"; # IP for vizio TV SHOULD be static per dnsmasq on tetragon
            living_room_tv_power_off = "${pkgs.python312Packages.pyvizio}/bin/pyvizio --ip=69.69.1.150 --auth=Zt93u2t4sq power off";
            living_room_tv_hdmi1 = "${pkgs.python312Packages.pyvizio}/bin/pyvizio --ip=69.69.1.150 --auth=Zt93u2t4sq input hdmi1";
            living_room_tv_hdmi2 = "${pkgs.python312Packages.pyvizio}/bin/pyvizio --ip=69.69.1.150 --auth=Zt93u2t4sq input hdmi2";

            # Switch video and audio for Kestrel
            kestrel_monitor_desk = "${pkgs.openssh}/bin/ssh -i ${
              config.age.secrets."ssh/hass".path
            } -o StrictHostKeyChecking=accept-new tstarr@kestrel.lan 'display-switch.sh kestrel-desktop'";
            kestrel_audio_desk = "${pkgs.openssh}/bin/ssh -i ${
              config.age.secrets."ssh/hass".path
            } -o StrictHostKeyChecking=accept-new tstarr@kestrel.lan 'sink-switch.sh Starship'";
            kestrel_monitor_living = "${pkgs.openssh}/bin/ssh -i ${
              config.age.secrets."ssh/hass".path
            } -o StrictHostKeyChecking=accept-new tstarr@kestrel.lan 'display-switch.sh kestrel-living'";
            kestrel_audio_living = "${pkgs.openssh}/bin/ssh -i ${
              config.age.secrets."ssh/hass".path
            } -o StrictHostKeyChecking=accept-new tstarr@kestrel.lan 'sink-switch.sh Dragon'";

            # Adjust subwoofer volume
            stormwalker_sub_low = "${pkgs.openssh}/bin/ssh -i ${
              config.age.secrets."ssh/hass".path
            } -o stricthostkeychecking=accept-new tstarr@stormwalker.lan 'subwoofer-volume.sh -21'";
            stormwalker_sub_mid = "${pkgs.openssh}/bin/ssh -i ${
              config.age.secrets."ssh/hass".path
            } -o stricthostkeychecking=accept-new tstarr@stormwalker.lan 'subwoofer-volume.sh -15'";
            stormwalker_sub_mid_high = "${pkgs.openssh}/bin/ssh -i ${
              config.age.secrets."ssh/hass".path
            } -o stricthostkeychecking=accept-new tstarr@stormwalker.lan 'subwoofer-volume.sh -9'";
            stormwalker_sub_high = "${pkgs.openssh}/bin/ssh -i ${
              config.age.secrets."ssh/hass".path
            } -o stricthostkeychecking=accept-new tstarr@stormwalker.lan 'subwoofer-volume.sh -3'";

            # Play/pause media on stormwalker
            stormwalker_play = "${pkgs.openssh}/bin/ssh -i ${
              config.age.secrets."ssh/hass".path
            } -o stricthostkeychecking=accept-new tstarr@stormwalker.lan 'playerctl play'";
            stormwalker_pause = "${pkgs.openssh}/bin/ssh -i ${
              config.age.secrets."ssh/hass".path
            } -o stricthostkeychecking=accept-new tstarr@stormwalker.lan 'playerctl pause'";
          };
        };
      };
    };
}
