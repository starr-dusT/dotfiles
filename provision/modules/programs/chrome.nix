{ config, lib, pkgs, user, ... }:

let cfg = config.modules.programs.chrome;
in {
  options.modules.programs.chrome.enable = lib.mkEnableOption "chrome";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (chromium.override { enableWideVine = true; }) # Web browser developed by Google
    ];

    programs.chromium = {
      enable = true;
      extraOpts = {
        # Disable all manner of account-related things.
        "BrowserSignin" = 0; # disable
        "BrowserAddPersonEnabled" = false;
        "BrowserGuestModeEnabled" = false;
        "UserDisplayName" = "PolicyUser";
        "UserFeedbackAllowed" = false;
        "BackgroundModeEnabled" = false;
        "MetricsReportingEnabled" = false;
        "BlockExternalExtensions" = true;
        "AutofillAddressEnabled" = false;
        "AutofillCreditCardEnabled" = false;
        "PasswordManagerEnabled" = false;
        "PromptForDownloadLocation" = true;
        "SyncDisabled" = true;
        "SpellcheckEnabled" = true;
        "SpellcheckLanguage" = [ "en-US" ];
        "CloudPrintSubmitEnabled" = false;
        "EnableMediaRouter" = false;
        "ShowCastIconInToolbar" = false;

        # Install extensions
        "ExtensionInstallForcelist" = [
          "ihennfdbghdiflogeancnalflhgmanop" # Gruvbox theme
          "nngceckbapebfimnlniiiahkandclblb" # Bitwarden
          "hkgfoiooedgoejojocmhlaklaeopbecg" # Picture-in-Picture (by Google)
          "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
          "beakmhbijpdhipnjhnclmhgjlddhidpe" # Linkding
          "icpgjfneehieebagbmdbhnlpiopdcmna" # New Tab Redirect
        ];

        # Setup bookmarks
        "BookmarkBarEnabled" = false;
        "ShowAppsShortcutInBookmarkBar" = false;
        "HomepageLocation" = "https://link.tstarr.us";
        "HomepageIsNewTabPage" = false;
        "ShowHomeButton" = true;
      };
    };
  };
}
