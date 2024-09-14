{ config, pkgs, user, lib, ... }:
{
  # Password-less logins for backup
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    config.age.secrets."ssh/torus/id_ed25519.pub".path
  ];
  systemd.tmpfiles.rules = [
    "d /store 0775 ${user} users -"
  ];
}
          
