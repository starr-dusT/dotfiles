# dotfiles
> My dotfiles... What do you want? 

My personal dotfiles repo. Documentation is pretty spotty. Maybe I'll fix that someday, but I probably won't. 

## usage

The dotfiles are managed with stow and otherwise Ansible sets everything up. The Anisible scripts are built for Fedora Workstation.

### initial setup

- Install latest Fedora Workstation
- If using nvidia setup with [this](https://copr.fedorainfracloud.org/coprs/t0xic0der/nvidia-auto-installer-for-fedora/)
- Setup zen-like kernel with [this](https://copr.fedorainfracloud.org/coprs/sentry/kernel-fsync/) from [Nobara](https://nobaraproject.org/)
- Run the following commands:
```bash
sudo dnf install -y git stow
git clone https://github.com/starr-dusT/dotfiles ~/.dotfiles 
rm ~/.bashrc
cd ~/.dotfiles && stow .
bash
initial-setup
```

### update setup

```bash
update {tags to update seperated with commas}
```

## TODO

- Change syncthing to user .service file rather than ansible
- add wireguard
- add https://github.com/digint/btrbk