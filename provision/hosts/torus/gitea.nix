{ config, lib, pkgs, user, ... }:
{
  services.gitea = {
    enable = true;
    lfs.enable = true;
    dump = {
      enable = true;
      interval = "23:05";
    };
    settings.service = {
        DISABLE_REGISTRATION = true;
    };
    settings.server = {
      DOMAIN = "git.tstarr.us";
      HTTP_PORT = 3001;
      ROOT_URL = "https://git.tstarr.us";
    };
    #settings.actions = {
    #  ENABLED = true;
    #};
  };
}
