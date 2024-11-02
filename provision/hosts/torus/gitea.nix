{ config, lib, pkgs, user, ... }:
let
  stateDir = "/var/lib/gitea";
  dumpFolder = "/engi/backup/dumps/gitea";
  domain = "git.tstarr.us";
in {

  # Main gitea service
  systemd.tmpfiles.rules = [
    "d ${dumpFolder} 0775 gitea gitea -"
  ];

  environment.systemPackages = [
    (pkgs.writeScriptBin "dump-gitea" ''
      #!/bin/sh
      cd ${dumpFolder}
      [ -e gitea-dump.zip ] && rm gitea-dump.zip
      exec ${pkgs.gitea}/bin/gitea dump --type zip -c ${stateDir}/custom/conf/app.ini --file "gitea-dump.zip"
    '')
  ];

  services.gitea = {
    enable = true;
    lfs.enable = true;
    stateDir = "${stateDir}";
    customDir = "${stateDir}/custom"; 
    settings.server = {
      DOMAIN = "${domain}";
      HTTP_PORT = 3001;
      ROOT_URL = "https://${domain}";
    };
    settings.service = {
        DISABLE_REGISTRATION = true;
    };
  };

  # Gitea runners
  users.users.gitea-runner = {
    createHome = false;
    isSystemUser = true;
    group = "gitea-runner";
  };
  users.groups.gitea-runner = {};

  age.secrets."git/gitea-runner-1" = {
    file = ../../secrets/git/gitea-runner-1.age;
    owner = "gitea-runner";
    group = "gitea-runner";
  };

  services.gitea-actions-runner.instances = {
    runner1 = {
      enable = true;
      url = "https://${domain}";
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
}
