[working-directory: 'provision']
update:
    cd ~/.local/share/chezmoi/provision
    sudo nixos-rebuild switch --flake .#$(hostname)

[working-directory: 'provision']
pull-update:
    git pull origin master
    cd ~/.local/share/chezmoi/provision
    sudo nixos-rebuild switch --flake .#$(hostname)

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
    ftw ./img/wallpapers "#282828"
