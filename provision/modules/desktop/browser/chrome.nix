{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop.browser.chrome;
in {
  options.modules.desktop.browser.chrome.enable = lib.mkEnableOption "chrome";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      google-chrome # Web browser developed by Google.
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
          "fkeaekngjflipcockcnpobkpbbfbhmdn" # Copy as Markdown
          "pcmpcfapbekmbjjkdalcgopdkipoggdi" # MarkDownload
          "nngceckbapebfimnlniiiahkandclblb" # Bitwarden
          "icpgjfneehieebagbmdbhnlpiopdcmna" # New Tab Redirect
          "oahiolknhkbpcolgnpljehalnhblolkm" # Shorts Blocker
          "hkgfoiooedgoejojocmhlaklaeopbecg" # Picture-in-Picture (by Google)
          "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
        ];

        # Setup bookmarks
        "BookmarkBarEnabled" = true;
        "ShowAppsShortcutInBookmarkBar" = false;
        "ManagedBookmarks" = import ./chrome-bookmarks.nix; 
      };
    };

    networking.firewall.allowedTCPPorts = [ 8080 ];
    services.static-web-server = {
      enable = true;
      listen = "[::]:8080";
      root = ../../../../resources/blank;
    };
  };
}
