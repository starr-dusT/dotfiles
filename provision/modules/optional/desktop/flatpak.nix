{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop;
in {
  config = lib.mkIf cfg.enable {
    services.flatpak.packages = [
      "com.github.tchx84.Flatseal"
    ];
  };
}
