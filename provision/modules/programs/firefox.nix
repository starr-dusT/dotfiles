{ config, lib, pkgs, user, ... }:

let cfg = config.modules.programs.firefox;
in {
  options.modules.programs.firefox.enable = lib.mkEnableOption "firefox";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      opensc # Open source smart card tools and middleware
      pcsc-tools # Tools are used to test a PC/SC drivers
      pkcs11helper # Library that simplifies the interaction with PKCS#11
    ];

    # Add DoD CA certs to trusted source
    security.pki.certificateFiles = [
      ../../../resources/dod_certs/DoDWCFInterCA1.crt
      ../../../resources/dod_certs/DoDRootCA3.crt
      ../../../resources/dod_certs/DoDRootCA4.crt
      ../../../resources/dod_certs/DoDRootCA5.crt
      ../../../resources/dod_certs/DoDRootCA6.crt
      ../../../resources/dod_certs/DoDInteroperabilityRootCA2.crt
      ../../../resources/dod_certs/USDoDCCEBInteroperabilityRootCA2.crt
    ];

    home-manager.users.${user} = {
      programs.firefox = {
        enable = true;
        policies = {
          SecurityDevices = {
            Add = {
              # 
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
