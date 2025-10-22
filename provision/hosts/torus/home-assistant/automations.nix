{ ... }:
{
  services.home-assistant.config = {
    "automation ui" = "!include automations.yaml";
  };
}
