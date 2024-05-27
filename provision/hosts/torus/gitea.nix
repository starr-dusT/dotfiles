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
  };

  sops.secrets = {
    "tokens/gitea-runner" = {
      sopsFile = ../../secrets/secrets.yaml;
      owner = "gitea-runner";
    };
  };
  services.gitea-actions-runner.instances = {
    native-runner = {
      enable = true;
      url = "https://git.tstarr.us";
      tokenFile = config.sops.secrets.gitea-runner.path;
      name = "native-runner";
      labels = [
        "native:host"
      ];
      hostPackages = with pkgs; [
        bash
        coreutils
        curl
        gawk
        gitMinimal
        gnused
        nodejs-18_x
        wget
      ];
    };
  };
  users.users.gitea-runner = {
    createHome = false;
    isSystemUser = true;
    group = "gitea-runner";
  };
  users.groups.gitea-runner = {};
}
