{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop.browser;

in {
  options.modules.desktop.browser.enable = lib.mkEnableOption "browser";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      chromium
      google-chrome
      play-with-mpv
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
          "gfapcejdoghpoidkfodoiiffaaibpaem" # Dracula Theme
          "fkeaekngjflipcockcnpobkpbbfbhmdn" # Copy as Markdown
          "nngceckbapebfimnlniiiahkandclblb" # Bitwarden
          "dbepggeogbaibhgnhhndojpepiihcmeb" # Viumium
          "icpgjfneehieebagbmdbhnlpiopdcmna" # New Tab Redirect
          "hahklcmnfgffdlchjigehabfbiigleji" # Play with MPV
          "oahiolknhkbpcolgnpljehalnhblolkm" # Shorts Blocker
        ];

        # Setup bookmarks
         "BookmarkBarEnabled" = true;
        "ShowAppsShortcutInBookmarkBar" = false;
        "ManagedBookmarks" = [
          { "toplevel_name" = "Bookmarks"; }
          { "name" = "Daily"; "children" = [
            { "url" = "https://www.youtube.com/feed/subscriptions"; name = "Youtube"; }
            { "url" = "https://gmail.com/"; name = "Mail"; }
            { "url" = "https://github.com/"; name = "GitHub"; }
            { "url" = "https://media.tstarr.us/web/index.html#!/home.html"; name = "Jellyfin"; }
            { "url" = "https://rss.tstarr.us"; name = "FreshRSS"; }
            { "url" = "https://share.tstarr.us"; name = "Share (dufs)"; }
            { "url" = "https://www.google.com/"; name = "Google"; }
            { "url" = "https://www.icloud.com/"; name = "iCloud"; }
          ]; }
          { "name" = "Games"; "children" = [
            { "url" = "https://www.protondb.com/"; name = "ProtonDB"; }
            { "url" = "https://vimm.net/"; name = "Vimm's Lair: Preserving the Classics"; }
          ]; }
          { "name" = "Homelab"; "children" = [
            { "url" = "http://localhost:8384"; name = "Syncthing"; }
            { "url" = "https://github.com/starr-dusT/dotfiles"; name = "Dotfiles"; }
            { "url" = "https://vault.tstarr.us"; name = "Vault (dufs)"; }
            { "url" = "https://rssbridge.tstarr.us"; name = "RSSBridge"; }
          ]; }
          { "name" = "Nix"; "children" = [
            { "url" = "https://nixos.org/nix/manual/"; name = "nix-manual"; }
            { "url" = "https://nixos.org/nixpkgs/manual/"; name = "nixpkgs-manual"; }
            { "url" = "https://nixos.org/nixos/manual/"; name = "nixos-manual"; }
            { "url" = "https://nixos.org/nixos/options.html"; name = "nixos-options"; }
            { "url" = "https://discourse.nixos.org/"; name = "nixos-discourse"; }
            { "url" = "https://old.reddit.com/r/nixos/"; name = "nixos-reddit"; }
            { "url" = "https://nixos.wiki/"; name = "nixos-wiki"; }
          ]; }
        ];
      };
    };

    # Start play-with-mpv as service
    systemd.user.services.play-with-mpv = {
      path = with pkgs; [ play-with-mpv mpv ];
      environment = {
          DISPLAY = ":0";
      };
      description = "play videos with mpv from chromium.";
      wantedBy = [ "default.target" ];
      restartIfChanged = true;
      
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        ExecStart = "${pkgs.play-with-mpv}/bin/play-with-mpv";
      };
    };
  };
}
