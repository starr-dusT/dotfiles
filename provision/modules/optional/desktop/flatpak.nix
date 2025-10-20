{ config, lib, ... }:

let cfg = config.modules.optional.desktop;
in {
  config = lib.mkIf cfg.enable {
    services.flatpak.packages = [
      "com.github.tchx84.Flatseal"
    ];
  };
}
