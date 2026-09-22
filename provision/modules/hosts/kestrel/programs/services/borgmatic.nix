{ ... }:
{
  flake.modules.nixos.kestrel =
    { config, ... }:
    let
      user = "${config.preferences.user}";
      additions = [
        "/home/*/.config/sops"
      ];
      excludes = [
        "/home/*/Documents/devel" # All code worth saving is version controlled
        "/home/*/Documents/40-49_media/41_video-games/41.01_roms"
        "/home/*/Documents/40-49_media/41_video-games/41.03_retrodeck-files"
      ];
    in
    {
      # Password-less logins for other hosts
      users.users."${user}".openssh.authorizedKeys.keyFiles = [
        ../../../../../secrets/ssh/pubs/torus.pub
        ../../../../../secrets/ssh/pubs/kestrel.pub
      ];

      systemd.tmpfiles.rules = [
        "d /store 0775 ${user} users -" # Directory for backups of other hosts
      ];

      preferences.backup-config = {
        "kestrel-torus" = {
          path = "ssh://tstarr@torus//engi/store/kestrel.borg";
          remote_path = "borg";
          additionalsources = additions;
          label = "torus";
          additionalExcludes = excludes;
        };
        "kestrel-rsync" = {
          path = "ssh://fm2120@fm2120.rsync.net//data1/home/fm2120/store/kestrel.borg";
          remote_path = "borg1";
          additionalsources = additions;
          label = "rsync";
          additionalExcludes = excludes;
        };
      };
    };
}
