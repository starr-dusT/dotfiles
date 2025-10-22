{ user, ... }:
{
  services.jellyfin.enable = true;
  services.jellyfin.openFirewall = true;
  services.jellyfin.user = "${user}";
}
