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

  # gitea runner secrets
  age.secrets."git/gitea-runner-1" = {
    file = ../../secrets/git/gitea-runner-1.age;
    owner = "gitea-runner";
    group = "gitea-runner";
  };

  services.gitea-actions-runner.instances = {
    runner1 = {
      enable = true;
      url = "https://git.tstarr.us";
      tokenFile = "/run/agenix/git/gitea-runner-1";
      name = "runner1";
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
        gnutar
        gzip
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
