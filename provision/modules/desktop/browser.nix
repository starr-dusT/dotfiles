{ config, lib, pkgs, pkgs-unstable, user, ... }:

let cfg = config.modules.desktop.browser;

in {
  options.modules.desktop.browser.enable = lib.mkEnableOption "browser";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs-unstable; [
      chromium
      google-chrome
    ];
    programs.chromium = {
      enable = true;
      extraOpts = {
        "BrowserSignin" = 0;
        "SyncDisabled" = true;
        "PasswordManagerEnabled" = false;
        "MetricsReportingEnabled" = true;
        "SpellcheckEnabled" = true;
        "SpellcheckLanguage" = [ "en-US" ];
        "CloudPrintSubmitEnabled" = false;
        "EnableMediaRouter" = false;
        "ShowCastIconInToolbar" = false;
        "ExtensionInstallForcelist" = [
          "gfapcejdoghpoidkfodoiiffaaibpaem" # Dracula Theme
          "fkeaekngjflipcockcnpobkpbbfbhmdn" # Copy as Markdown
          "nngceckbapebfimnlniiiahkandclblb" # Bitwarden
          "hkgfoiooedgoejojocmhlaklaeopbecg" # Picture-in-Picture (by Google)
          "dbepggeogbaibhgnhhndojpepiihcmeb" # Viumium
        ];
      };
    };
  };
}
