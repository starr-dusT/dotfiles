self: super: {
  gnomeExtensions = super.gnomeExtensions // {
    focus-changer = super.gnomeExtensions.focus-changer.overrideAttrs (old: { 
      src = super.fetchFromGitHub { 
        owner = "martinhjartmyr"; 
        repo = "gnome-shell-extension-focus-changer"; 
        rev = "4de5bc334a1624fd8eb22203ab229a6045320923"; 
        sha256 = "sha256-hvLDsLQl7+Flrlg26HNIuwLnWqpB09ZO1xNHRms22g0=";
      }; 
    }); 
  };
}
