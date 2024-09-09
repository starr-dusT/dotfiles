{ config, pkgs, user, lib, ... }:
{
  # Password-less logins for backup
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    config.age.secrets."ssh/kestrel/id_ed25519.pub".path
    config.age.secrets."ssh/torus/id_ed25519.pub".path
  ];

  services.borgmatic.enable = true;
  environment.systemPackages = with pkgs; [
    borgbackup # Deduplicating backup program 
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
}
          
