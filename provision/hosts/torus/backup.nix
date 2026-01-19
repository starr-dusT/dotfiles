{ user, pkgs, ... }:
{
  # Password-less login for user
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    ../../secrets/ssh/pubs/kestrel.pub
    ../../secrets/ssh/pubs/vortex-1.pub
  ];

  systemd.tmpfiles.rules = [
    "d /engi/store 0775 ${user} users -" # Directory for backups of other hosts
  ];

  environment.systemPackages = with pkgs; [
    tree # Command to produce a depth indented directory listing
  ];

  modules.optional.services.borgmatic = {
    enable = true;
    config = {
      "torus-rsync" = {
        path = "ssh://fm2120@fm2120.rsync.net//data1/home/fm2120/store/torus.borg";
        label = "rsync";
        additionalSources = [
          "/engi/backup"
          "/home/${user}/.podman/databasus"
        ];
        moreOpts = {
          commands = [
            {
              before = "everything";
              when = [ "create" ];
              run = [ "${pkgs.tree}/bin/tree /engi > /engi/backup/tree.txt" ];
            }
          ];
          remote_path = "borg1";
        };
      };
      "torus-drive" = {
        path = "/media/clone/store/torus.borg";
        label = "drive";
        additionalSources = [ "/engi/media" ];
        moreOpts = {
          commands = [
            {
              before = "everything";
              when = [ "create" ];
              run = [ "${pkgs.tree}/bin/tree /engi > /engi/backup/tree.txt" ];
            }
            {
              before = "repository";
              run = [ "${pkgs.util-linux}/bin/findmnt /media/clone > /dev/null || exit 75" ];
            }
          ];
        };
      };
    };
  };
}
