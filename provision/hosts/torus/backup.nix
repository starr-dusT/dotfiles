{ config, pkgs, user, lib, ... }:
{
  # Password-less logins for backup
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    config.age.secrets."ssh/kestrel/id_ed25519.pub".path
  ];
}
          
