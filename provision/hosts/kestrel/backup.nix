{ user, ... }:
{
  age.secrets."ssh/kestrel" = {
    file = ../../secrets/ssh/kestrel.age;
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

  # Password-less logins for backup
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    ../../secrets/ssh/pubs/torus.pub
  ];

  # Password-less login for root
  programs.ssh.extraConfig = ''
    Host torus 
      AddKeysToAgent yes
      IdentityFile /run/agenix/ssh/kestrel 

    Host fm2120.rsync.net
      AddKeysToAgent yes
      IdentityFile /run/agenix/borg/rsync/id_rsa
  '';

  systemd.tmpfiles.rules = [
    "d /store 0775 ${user} users -"
  ];
}
