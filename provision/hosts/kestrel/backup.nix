{ user, ... }:
let
  excludes = [
    "/home/*/Documents/devel" # All code worth saving is version controlled
  ];
in
{
  # Password-less logins for other hosts
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    ../../secrets/ssh/pubs/torus.pub
  ];

  systemd.tmpfiles.rules = [
    "d /store 0775 ${user} users -" # Directory for backups of other hosts
  ];

  modules.optional.services.borgmatic = {
    enable = true;
    config = {
      "kestrel-torus" = {
        path = "ssh://tstarr@torus//engi/store/kestrel.borg";
        label = "torus";
        additionalExcludes = excludes;
      };
      "kestrel-rsync" = {
        path = "ssh://fm2120@fm2120.rsync.net//data1/home/fm2120/store/kestrel.borg";
        label = "rsync";
        additionalExcludes = excludes;
        moreOpts = {
          remote_path = "borg1";
        };
      };
    };
  };
}
