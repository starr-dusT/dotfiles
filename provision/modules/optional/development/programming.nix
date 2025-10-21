{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.optional.development.programming;
in
{
  options.modules.optional.development.programming.enable = lib.mkEnableOption "programming";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      nodejs # JavaScript runtime built on Chrome's V8 JavaScript engine
      cargo # Package manager and build system for Rust
      docker-compose # Docker CLI plugin to define and run multi-container applications with Docker
      distrobox # Use any linux distribution inside your terminal
      just # Hand way to save and run project-specific commands
      nixd # Feature-rich Nix language server interoperating with C++ nix
      nixfmt # Official formatter for Nix code
      lua-language-server # Language server that offers Lua language support
    ];
  };
}
