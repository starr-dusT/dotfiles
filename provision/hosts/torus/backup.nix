{ config, user, ... }:
{
  age.secrets."ssh/kestrel/id_ed25519.pub" = {
    file = ../../secrets/ssh/kestrel/id_ed25519.pub.age;
    owner = "${user}";
    group = "users";
  };
  age.secrets."ssh/torus/id_ed25519" = {
    file = ../../secrets/ssh/torus/id_ed25519.age;
    owner = "${user}";
    group = "users";
  };
  age.secrets."ssh/torus/id_ed25519.pub" = {
    file = ../../secrets/ssh/torus/id_ed25519.pub.age;
    owner = "${user}";
    group = "users";
  };
  age.secrets."borg/torus/password" = {
    file = ../../secrets/borg/torus/password.age;
    owner = "${user}";
    group = "users";
  };
  age.secrets."borg/rsync/id_rsa" = {
    file = ../../secrets/borg/rsync/id_rsa.age;
    owner = "${user}";
    group = "users";
  };
  age.secrets."borg/rsync/id_rsa.pub" = {
    file = ../../secrets/borg/rsync/id_rsa.pub.age;
    owner = "${user}";
    group = "users";
  };

  # Password-less login for user
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    config.age.secrets."ssh/kestrel/id_ed25519.pub".path
  ];

  # Password-less login for root
  programs.ssh.extraConfig = ''
    Host kestrel
      AddKeysToAgent yes
      IdentityFile /run/agenix/ssh/torus/id_ed25519 

    Host fm2120.rsync.net
      AddKeysToAgent yes
      IdentityFile /run/agenix/borg/rsync/id_rsa
  '';
}
