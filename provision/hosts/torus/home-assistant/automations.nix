{ config, lib, pkgs, user, ... }:
{
  services.home-assistant.config = {
    "automation manual" = [
      {
        alias = "turn on christmas lights";
        trigger = {
          platform = "time";
          at = "16:45";
        };
        action = [
          {
            type = "turn_on";
            device_id = "32bf8baf1214391472566d4300c7d6fb";
            entity_id = "72c7694c3a3aed5d67499bfe0574346b";
            domain = "switch";
          }
          {
            type = "turn_on";
            device_id = "08db6bd9e7bf98d5bf1b08351b909c94";
            entity_id = "ecec82df2265c6b92c8474fe0c9fde6b";
            domain = "switch";
          }
        ];
      }
      {
        alias = "turn off christmas lights";
        trigger = {
          platform = "time";
          at = "23:59";
        };
        action = [
          {
            type = "turn_off";
            device_id = "32bf8baf1214391472566d4300c7d6fb";
            entity_id = "72c7694c3a3aed5d67499bfe0574346b";
            domain = "switch";
          }
          {
            type = "turn_off";
            device_id = "08db6bd9e7bf98d5bf1b08351b909c94";
            entity_id = "ecec82df2265c6b92c8474fe0c9fde6b";
            domain = "switch";
          }
        ];
      }
    ];
    "automation ui" = "!include automations.yaml";
  };
}
