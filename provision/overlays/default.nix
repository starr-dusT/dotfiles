final: prev: {
  advcpmv = final.callPackage ../pkgs/advcpmv.nix {};
  ftw = final.callPackage ../pkgs/ftw.nix {};
  gnome-fullscreen-to-empty-workspace = final.callPackage ../pkgs/gnome-fullscreen-to-empty-workspace/default.nix {};
  gnome-set-panel-monitor = final.callPackage ../pkgs/gnome-set-panel-monitor.nix {};
  nx_tzdb = final.callPackage ../pkgs/nx_tzdb.nix {};
  pySVS = final.callPackage ../pkgs/pySVS.nix {};
  ryujinx = final.callPackage ../pkgs/ryujinx/default.nix {};
  sudachi = final.qt6.callPackage ../pkgs/sudachi/default.nix {};
  yuzu = final.libsForQt5.callPackage ../pkgs/yuzu/default.nix {};
}
