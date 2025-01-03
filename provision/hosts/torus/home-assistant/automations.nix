{ config, lib, pkgs, user, ... }:
{
  services.home-assistant.config = {
    "automation ui" = "!include automations.yaml";
  };
}
