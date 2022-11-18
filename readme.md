# dotfiles
> My dotfiles... What do you want? 

My personal dotfiles repo. Documentation is pretty spotty. Maybe I'll fix that someday, but I probably won't. 

## usage

The dotfiles are managed with stow and otherwise Ansible sets everything up. The Anisible scripts are built for Fedora Workstation.

### initial setup

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

## random notes

Random notes about things to change on a new system:

- Brave browser plays better in Gnome with hardware acceleration turned off

## TODO

- Change syncthing to user .service file rather than ansible
- manage gnome extensions with ansible
- add wireguard
- remove things that require xOrg (and replace)