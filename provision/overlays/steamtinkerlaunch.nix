self: super: {
  steamtinkerlaunch = super.steamtinkerlaunch.overrideAttrs (old: { 
    src = super.fetchFromGitHub { 
      owner = "sonic2kk"; 
      repo = "steamtinkerlaunch"; 
      rev = "6ff8371e9c8f8122ab4b532e4649e76509a38666"; 
      sha256 = "sha256-DsfUcAqSnrLvIbkIJ52OHlNfxaq+mOjimc9x/tOW1dA=";
    }; 
  }); 
}
