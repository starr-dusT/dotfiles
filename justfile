[working-directory: 'provision']
update:
    cd ~/.local/share/chezmoi/provision
    sudo nixos-rebuild switch --impure --flake .#$(hostname)

[working-directory: 'provision']
build-wsl:
    cd ~/.local/share/chezmoi/provision
    sudo nix run .#nixosConfigurations.wsl.config.system.build.tarballBuilder

[working-directory: 'provision/secrets']
rekey-secrets:
    cd ~/.local/share/chezmoi/provision/secrets
    sudo agenix -r -i /etc/ssh/ssh_host_ed25519_key
