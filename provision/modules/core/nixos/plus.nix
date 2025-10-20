{ lib, config, ... }:

let cfg = config.modules.base-plus;
in {
  options.modules.base-plus.enable = lib.mkEnableOption "base-plus";

  config = {
    modules = {
        programs = {
          borg.enable = true;
          beancount.enable = true;
      };
    };
  };
}
