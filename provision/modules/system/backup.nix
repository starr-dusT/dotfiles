{ config, pkgs, user, lib, ... }:

let cfg = config.modules.system.backup;
in {
  options.modules.system.backup.enable = lib.mkEnableOption "backup";
  config = lib.mkIf cfg.enable {
    services.borgmatic.enable = true;
    environment.systemPackages = with pkgs; [
      borgbackup # Deduplicating backup program 
      tree
      (pkgs.writeScriptBin "stop-docker-containers" ''
        #!/bin/sh
        [ -e /tmp/docker_images ] && rm /tmp/docker_images
        images=$(docker ps -a -q)
        echo "$images" > /tmp/docker_images
        docker stop $images
      '')
      (pkgs.writeScriptBin "restore-docker-containers" ''
        #!/bin/sh
        [ ! -e /tmp/docker_images ] && exit 0
        docker start $(cat /tmp/docker_images)
        rm /tmp/docker_images
      '')
    ];
  };
}
