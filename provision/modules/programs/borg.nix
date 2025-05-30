{ config, pkgs, user, lib, ... }:

let cfg = config.modules.programs.borg;
in {
  options.modules.programs.borg.enable = lib.mkEnableOption "borg";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      borgbackup # Deduplicating backup program 
      tree # Command to produce a depth indented directory listing
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

    services.borgmatic.enable = true;
  };
}
