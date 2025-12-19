{ user, ... }:
{
  age.secrets."ssh/torus" = {
    file = ../../secrets/ssh/torus.age;
    owner = "${user}";
    group = "users";
  };
  age.secrets."borg/torus/discord_webhook" = {
    file = ../../secrets/borg/torus/discord_webhook.age;
    owner = "${user}";
    group = "users";
  };
  age.secrets."borg/rsync/id_rsa" = {
    file = ../../secrets/borg/rsync/id_rsa.age;
    owner = "${user}";
    group = "users";
  };
  age.secrets."borg/torus/password" = {
    file = ../../secrets/borg/torus/password.age;
    owner = "${user}";
    group = "users";
  };

  # Password-less login for user
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    ../../secrets/ssh/pubs/kestrel.pub
    ../../secrets/ssh/pubs/vortex-1.pub
  ];

  # Password-less login for root
  programs.ssh.extraConfig = ''
    Host kestrel
      AddKeysToAgent yes
      IdentityFile /run/agenix/ssh/torus 

    Host fm2120.rsync.net
      AddKeysToAgent yes
      IdentityFile /run/agenix/borg/rsync/id_rsa
  '';
}
