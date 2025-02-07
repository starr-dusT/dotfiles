[working-directory: 'provision']
build-wsl:
    cd ~/.local/share/chezmoi/provision
    sudo nix run .#nixosConfigurations.wsl.config.system.build.tarballBuilder
