{ config, lib, pkgs, user, ... }:

let cfg1 = config.modules.desktop;
    cfg2 = config.modules.programs.firefox;
in {
  options.modules.programs.firefox = with lib; {
    enable = lib.mkOption {
      type = types.bool;
      default = true;
    };
  };

  config = lib.mkIf (cfg1.enable && cfg2.enable) {
    home-manager.users.${user} = {
      programs.firefox = {
        enable = true;
        policies = {
          SecurityDevices = {
            Add = {
              "NIPR" = "${pkgs.opensc}/lib/opensc-pkcs11.so";
            };
          };
        };
        profiles.default = {
          isDefault = true;
          name = "default";
          settings = {
            "browser.bookmarks.addedImportButton" = false;
            "browser.toolbars.bookmarks.visibility" = "always";
            "browser.newtabpage.activity-stream.feeds.topsites" = false;
            "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
            "browser.newtabpage.pinned" = [];
            "browser.tabs.drawInTitlebar" = true;
            "browser.tabs.inTitlebar" = 1;
          };
        };
      };
    };
  };
}
