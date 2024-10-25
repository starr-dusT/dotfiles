self: super: {
  appgate-sdp = super.appgate-sdp.overrideAttrs (old: { 
    src = super.fetchurl { 
      url = "https://bin.appgate-sdp.com/6.4/client/appgate-sdp_6.4.0_amd64.deb";
      sha256 = "sha256-0h6Mz3B7fADGL5tGbrKNYpVIAvRu7Xx0n9OvjOeVCds=";
    }; 
  }); 
}
