/* This contains various packages we want to overlay. Note that the
 * other ".nix" files in this directory are automatically loaded.
 */
final: prev: {
  advcpmv = final.callPackage ../pkgs/advcpmv.nix {};
  nx_tzdb = final.callPackage ../pkgs/nx_tzdb.nix {};
  sudachi = final.kdePackages.callPackage ../pkgs/sudachi.nix {};
  gnome-set-panel-monitor = final.callPackage ../pkgs/gnome-set-panel-monitor.nix {};
  gnome-fullscreen-to-empty-workspace = final.callPackage ../pkgs/gnome-fullscreen-to-empty-workspace.nix {};
  ryujinx-archive = final.callPackage ../pkgs/ryujinx-archive/default.nix {};
}
