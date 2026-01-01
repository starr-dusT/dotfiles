{ user, ... }:
let
  includes = [
    "/engi/backup"
  ];
in
{
  # Password-less login for user
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    ../../secrets/ssh/pubs/kestrel.pub
    ../../secrets/ssh/pubs/vortex-1.pub
  ];

  systemd.tmpfiles.rules = [
    "d /engi/store 0775 ${user} users -" # Directory for backups of other hosts
  ];

  modules.optional.services.borgmatic = {
    enable = true;
    config = {
      "torus-rsync" = {
        path = "ssh://fm2120@fm2120.rsync.net//data1/home/fm2120/store/torus.borg";
        label = "rsync";
        archiveName = "torus_bulk.borg-{now}";
        additionalSources = includes;
        moreOpts = {
          commands = [
            {
              before = "everything";
              when = [ "create" ];
              run = [ "tree /engi > /engi/backup/tree.txt" ];
            }
          ];
          remote_path = "borg1";
        };
      };
    };
  };
}
