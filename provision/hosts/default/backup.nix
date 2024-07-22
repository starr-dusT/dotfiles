{ config, pkgs, user, lib, ... }:
{
  # Password-less logins for backup
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    config.age.secrets."ssh/kestrel/id_ed25519.pub".path
  ];

  environment.systemPackages = with pkgs; [
    restic # Fast and secure backup program
  ];
}
