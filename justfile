[working-directory: 'provision']
update:
    cd ~/.local/share/chezmoi/provision
    sudo nixos-rebuild switch --flake .#$(hostname)

[working-directory: 'provision']
ship ships user="tstarr":
    cd ~/.local/share/chezmoi/provision
    IFS=','; \
    ships={{ships}}; \
    for ship in $ships; do \
        nixos-rebuild switch --flake ".#${ship}" --target-host "{{user}}@${ship}.lan" --ask-sudo-password; \
    done

[working-directory: 'provision']
install hostname:
    cd ~/.local/share/chezmoi/provision
    sudo nixos-rebuild switch --flake .#{{hostname}}

[working-directory: 'provision']
format-nix:
    cd ~/.local/share/chezmoi/provision
    treefmt .

[working-directory: 'provision/secrets']
rekey-secrets:
    cd ~/.local/share/chezmoi/provision/secrets
    sudo agenix -r -i /etc/ssh/ssh_host_ed25519_key

[working-directory: 'resources']
ftw-wallpapers:
    cd ~/.local/share/chezmoi/resources
    ftw ./img/wallpapers "#1D1D20"
