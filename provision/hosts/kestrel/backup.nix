{ user, pkgs, ... }:
let
  sources = [
    # Common sources
    "/home/${user}"

    # Host specific sources
  ];
  excludes = [
    # Common excludes
    "/home/*/.?*" # Exclude dotfiles
    "home/*/Downloads"
    "**/node_modules"
    "**/bower_components"
    "**/_build"
    "**/.tox"
    "**/venv"
    "**/.venv"

    # Host specific excludes
    "/home/*/Documents/devel" # All code worth saving is version controlled
  ];
in
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
  age.secrets."borg/password" = {
    file = ../../secrets/borg/password.age;
    owner = "${user}";
    group = "users";
  };

  # Password-less login for root
  programs.ssh.extraConfig = ''
    Host fm2120.rsync.net
          AddKeysToAgent yes
          IdentityFile /run/agenix/borg/rsync/id_rsa
    Host *
      AddKeysToAgent yes
      IdentityFile /run/agenix/ssh/kestrel 
  '';

  environment.systemPackages = with pkgs; [
    xxHash # Extremely fast hash algorithm
  ];
  services.borgmatic = {
    enable = true;
    configurations = {
      "kestrel-torus" = {
        repositories = [
          {
            path = "ssh://tstarr@torus//engi/store/kestrel.borg";
            label = "torus";
          }
        ];
        ssh_command = "ssh -i /run/agenix/borg/rsync/id_rsa -o StrictHostKeyChecking=no";
        encryption_passcommand = "cat /run/agenix/borg/password";

        source_directories = sources;
        exclude_patterns = excludes;

        archive_name_format = "kestrel.borg-{now}";
        keep_daily = 7;
        keep_weekly = 4;
        keep_monthly = 6;

        checks = [
          { name = "repository"; }
          {
            name = "spot";
            count_tolerance_percentage = 10;
            data_sample_percentage = 1;
            data_tolerance_percentage = 0.5;
            xxh64sum_command = "/run/current-system/sw/bin/xxhsum -H64";
          }
        ];
        relocated_repo_access_is_ok = true;
      };
      "kestrel-rsync" = {
        repositories = [
          {
            path = "ssh://fm2120@fm2120.rsync.net//data1/home/fm2120/store/kestrel.borg";
            label = "rsync";
          }
        ];
        remote_path = "borg1";
        ssh_command = "ssh -i /run/agenix/borg/rsync/id_rsa -o StrictHostKeyChecking=no";
        encryption_passcommand = "cat /run/agenix/borg/password";

        source_directories = sources;
        exclude_patterns = excludes;

        archive_name_format = "kestrel.borg-{now}";
        keep_daily = 7;
        keep_weekly = 4;
        keep_monthly = 6;

        checks = [
          { name = "repository"; }
          {
            name = "spot";
            count_tolerance_percentage = 10;
            data_sample_percentage = 1;
            data_tolerance_percentage = 0.5;
            xxh64sum_command = "/run/current-system/sw/bin/xxhsum -H64";
          }
        ];
        relocated_repo_access_is_ok = true;
      };
    };
  };

  systemd.timers.borgmatic = {
    timerConfig = {
      OnCalendar = "02:00";
      Persistent = true;
      RandomizedDelaySec = "5m";
    };
  };

  # Password-less logins for backup
  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    ../../secrets/ssh/pubs/torus.pub
  ];

  systemd.tmpfiles.rules = [
    "d /store 0775 ${user} users -" # Directory for backups of other hosts
  ];
}
