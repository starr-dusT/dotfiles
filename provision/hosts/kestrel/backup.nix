{ user, ... }:
let
  name = "kestrel.borg-{now}";
  sources = [ ];
  excludes = [
    "/home/*/Documents/devel" # All code worth saving is version controlled
  ];
in
{
  # Password-less logins for other hosts
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    ../../secrets/ssh/pubs/torus.pub
  ];

  # Password-less login for root
  programs.ssh.extraConfig = ''
    Host fm2120.rsync.net
      AddKeysToAgent yes
      IdentityFile /run/agenix/borg/rsync/id_rsa

    Host *
      AddKeysToAgent yes
      IdentityFile /run/agenix/ssh/kestrel
  '';

  systemd.tmpfiles.rules = [
    "d /store 0775 ${user} users -" # Directory for backups of other hosts
  ];

  modules.optional.services.borgmatic = {
    enable = true;
    config = {
      "kestrel-torus" = {
        path = "ssh://tstarr@torus//engi/store/kestrel.borg";
        label = "torus";
        remote_path = "borg";
        archive_name_format = name;
        additional_sources = sources;
        additional_excludes = excludes;
      };
      "kestrel-rsync" = {
        path = "ssh://fm2120@fm2120.rsync.net//data1/home/fm2120/store/kestrel.borg";
        label = "rsync";
        remote_path = "borg1";
        archive_name_format = name;
        additional_sources = sources;
        additional_excludes = excludes;
      };
    };
  };
}
