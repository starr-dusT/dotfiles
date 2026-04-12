{ ... }:
{
  flake.modules.nixos.programming =
    { pkgs, config, ... }:
    {
      environment.systemPackages = with pkgs; [
        nodejs # JavaScript runtime built on Chrome's V8 JavaScript engine
        cargo # Package manager and build system for Rust
        docker-compose # Docker CLI plugin to define and run multi-container applications with Docker
        distrobox # Use any linux distribution inside your terminal
        just # Hand way to save and run project-specific commands
        nixd # Feature-rich Nix language server interoperating with C++ nix
        nixfmt # Official formatter for Nix code
        nixfmt-tree # Official Nix formatter zero-setup starter using treefmt
        lua-language-server # Language server that offers Lua language support
        devenv # Fast, Declarative, Reproducible, and Composable Developer Environments
        (python3.withPackages (
          ps: with ps; [
            python-lsp-server # Python implementation of the Language Server Protocol
            python-lsp-jsonrpc # Python server implementation of the JSON RPC 2.0 protocol
            python-lsp-black # Black plugin for the Python LSP Server
            python-lsp-ruff # Ruff linting plugin for pylsp
            pyls-isort # Isort plugin for python-lsp-server
            pyls-flake8 # Modular source code checker: pep8, pyflakes and co
            flake8 # Modular source code checker: pep8, pyflakes and co
            isort # Isort plugin for python-lsp-server
            black # Modular source code checker: pep8, pyflakes and co
          ]
        ))
      ];

      # Needed for devenv
      nix.settings.trusted-users = [
        "root"
        "${config.preferences.user}"
      ];
    };
}
