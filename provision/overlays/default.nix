/* This contains various packages we want to overlay. Note that the
 * other ".nix" files in this directory are automatically loaded.
 */
final: prev: {
  advcpmv = final.callPackage ../pkgs/advcpmv.nix {};
  nx_tzdb = final.callPackage ../pkgs/nx_tzdb.nix {};
  sudachi = final.qt6.callPackage ../pkgs/sudachi/default.nix {};
  yuzu = final.libsForQt5.callPackage ../pkgs/yuzu.nix {};
  gnome-set-panel-monitor = final.callPackage ../pkgs/gnome-set-panel-monitor.nix {};
  gnome-fullscreen-to-empty-workspace = final.callPackage ../pkgs/gnome-fullscreen-to-empty-workspace.nix {};
  ryujinx = final.callPackage ../pkgs/ryujinx/default.nix {};
  pySVS = final.callPackage ../pkgs/pySVS.nix {};
  ftw = final.callPackage ../pkgs/ftw.nix {};
}
