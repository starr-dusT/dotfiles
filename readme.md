# dotfiles
> My dotfiles... What do you want? 

My personal dotfiles repo. Documentation is pretty spotty. Maybe I'll fix that someday, but I probably won't. 

## usage

The dotfiles are managed with stow and otherwise Ansible sets everything up. The Anisible scripts are built for Fedora.

### initial setup

```bash
sudo dnf install -y git stow
https://github.com/starr-dusT/dotfiles ~/.dotfiles 
cd ~/.dotfiles && stow .
bash
initial-setup
```

### update setup

```bash
update {tags to update seperated with commas}
```
