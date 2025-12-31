{
  config,
  lib,
  hostname,
  user,
  pkgs,
  ...
}:
let
  cfg = config.modules.optional.services.borgmatic;
  common_sources = [
    "/home/${user}"
  ];
  common_excludes = [
    "/home/*/.?*" # Exclude dotfiles
    "home/*/Downloads"
    # Common programming venvs
    "**/node_modules"
    "**/bower_components"
    "**/_build"
    "**/.tox"
    "**/venv"
    "**/.venv"
  ];
in
{
  options.modules.optional.services.borgmatic.enable = lib.mkEnableOption "borgmatic";
  options.modules.optional.services.borgmatic.config = lib.mkOption {
    type = lib.types.attrs;
    default = [ ];
    description = "Set of configs for backups.";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      xxHash # Extremely fast hash algorithm
    ];

    # Password-less login for root
    programs.ssh.extraConfig = ''
      Host fm2120.rsync.net
        AddKeysToAgent yes
        IdentityFile /run/agenix/borg/rsync/id_rsa

      Host *
        AddKeysToAgent yes
        IdentityFile /run/agenix/ssh/${hostname}
    '';

    age.secrets."ssh/${hostname}" = {
      file = ../../../secrets/ssh/${hostname}.age;
      owner = "${user}";
      group = "users";
    };
    age.secrets."borg/webhook" = {
      file = ../../../secrets/borg/webhook.age;
      owner = "${user}";
      group = "users";
    };
    age.secrets."borg/rsync/id_rsa" = {
      file = ../../../secrets/borg/rsync/id_rsa.age;
      owner = "${user}";
      group = "users";
    };
    age.secrets."borg/password" = {
      file = ../../../secrets/borg/password.age;
      owner = "${user}";
      group = "users";
    };

    services.borgmatic = {
      enable = true;
      configurations = lib.mapAttrs (repo: opts: {
        repositories = [
          {
            path = opts.path;
            label = opts.label;
          }
        ];
        remote_path = opts.remote_path;
        ssh_command = "ssh -i /run/agenix/borg/rsync/id_rsa -o StrictHostKeyChecking=no";
        encryption_passcommand = "cat /run/agenix/borg/password";

        source_directories = common_sources ++ opts.additional_sources;
        exclude_patterns = common_excludes ++ opts.additional_excludes;

        archive_name_format = "${hostname}.borg-{now}";
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
      }) cfg.config;
    };

    systemd.timers.borgmatic = {
      timerConfig = {
        OnCalendar = "02:00";
        Persistent = true;
        RandomizedDelaySec = "10m";
      };
    };
  };
}
