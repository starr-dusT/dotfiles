{ config, lib, pkgs, user, ... }:
{
  imports = [
    ./mqtt.nix
    ./automations.nix
  ];
  networking.firewall.allowedTCPPorts = [ 8123 ];
  networking.firewall.allowedUDPPorts = [ 8123 ];

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
    };
  };

}
