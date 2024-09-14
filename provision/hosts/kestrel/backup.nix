{ config, pkgs, user, lib, ... }:
{
  age.secrets."ssh/torus/id_ed25519.pub" = {
    file = ../../secrets/ssh/torus/id_ed25519.pub.age;
    owner = "${user}";
    group = "users";
  };
  age.secrets."ssh/kestrel/id_ed25519" = {
    file = ../../secrets/ssh/kestrel/id_ed25519.age;
    owner = "${user}";
    group = "users";
  };
  age.secrets."ssh/kestrel/id_ed25519.pub" = {
    file = ../../secrets/ssh/kestrel/id_ed25519.pub.age;
    owner = "${user}";
    group = "users";
  };

  # Password-less logins for backup
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    config.age.secrets."ssh/torus/id_ed25519.pub".path
  ];

  # Password-less login for root
  programs.ssh.extraConfig = ''
    Host torus 
      AddKeysToAgent yes
      IdentityFile /run/agenix/ssh/kestrel/id_ed25519 
  '';

  systemd.tmpfiles.rules = [
    "d /store 0775 ${user} users -"
  ];
}
          
