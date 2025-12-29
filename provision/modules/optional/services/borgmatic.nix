{
  config,
  lib,
  hostname,
  user,
  ...
}:

let
  cfg = config.modules.optional.services.borgmatic;
  common_sources = [
    "home/${user}"
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
  options.modules.optional.services.borgmatic.repositories = lib.mkOption {
    type = lib.types.listOf lib.types.attrs;
    default = [ ];
    description = "Set that defines borg repositories.";
  };
  options.modules.optional.services.borgmatic.source_directories = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = "List of directories/files to backup.";
  };
  options.modules.optional.services.borgmatic.exclude_patterns = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = "List of directories/files to exclude.";
  };

  config = lib.mkIf cfg.enable {
    services.borgmatic = {
      enable = true;
      configurations = {
        "${hostname}" = {
          repositories = cfg.repositories;
          source_directories = common_sources ++ cfg.source_directories;
          exclude_patterns = common_excludes ++ cfg.exclude_patterns;
          encryption_passcommand = "cat /run/agenix/borg/password";
          checks = [
            { name = "repository"; }
            {
              name = "spot";
              count_tolerance_percentage = 10;
              data_sample_percentage = 1;
              data_tolerance_percentage = 0.5;
            }
          ];
          archive_name_format = "${hostname}.borg-{now}";
          keep_daily = 7;
          keep_weekly = 4;
          keep_monthly = 6;
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
  };
}
