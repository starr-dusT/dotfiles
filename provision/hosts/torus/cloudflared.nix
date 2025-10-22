{ config, ... }:
{
  age.secrets."cloudflared/tstarr.json" = {
    file = ../../secrets/cloudflared/tstarr.json.age;
  };
  services.cloudflared = {
    enable = true;
    tunnels = {
      "tstarr" = {
        credentialsFile = config.age.secrets."cloudflared/tstarr.json".path;
        default = "http_status:404";
      };
    };
  };
}
