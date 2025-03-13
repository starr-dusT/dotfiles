[working-directory: 'provision']
update:
    cd ~/.local/share/chezmoi/provision
    sudo nixos-rebuild switch --impure --flake .#$(hostname)

build-wsl:
    cd ~/.local/share/chezmoi/provision
    sudo nix run .#nixosConfigurations.wsl.config.system.build.tarballBuilder
