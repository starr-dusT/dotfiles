{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop.browser;
in {
  options.modules.desktop.browser.enable = lib.mkEnableOption "browser";
  config = lib.mkIf cfg.enable {
    # Install applications for CAC
    environment.systemPackages = with pkgs; [
      opensc
      pcsc-tools
      pkcs11helper
    ];

    home-manager.users.${user} = {
      programs.firefox = {
        enable = true;
        profiles.default = {
          bookmarks = import ./bookmarks/firefox.nix;
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
