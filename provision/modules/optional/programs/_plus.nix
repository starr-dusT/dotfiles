{ lib, config, ... }:

let
  cfg = config.modules.optional.programs._plus;
in
{
  options.modules.optional.programs._plus.enable = lib.mkEnableOption "_plus";

  config = lib.mkIf cfg.enable {
    modules = {
      optional = {
        programs = {
          beancount.enable = true;
        };
      };
    };
  };
}
