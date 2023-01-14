# Dotfiles
> My dotfiles... What do you want? 

My personal dotfiles repo. Documentation is pretty spotty. Maybe I'll fix that someday, but I probably won't. 

## Usage

Dotfiles are managed with chezmoi and Ansible (built for Arch) is used for the bulk of configuration. 

## Initial Setup

Install Arch with BTRFS and run the following commands:

```bash
sudo pacman -Syu
sudo pacman -S vim git chezmoi
git clone https://github.com/starr-dusT/dotfiles ~/.local/share/chezmoi 
```

Copy `.chezmoidata.yaml.example` to `.chezmoidata.yaml` and edit with desired settings then run the following commands:

```bash
~/.local/share/chezmoi/jumpstart.sh
```

After reboot log into window manager and run `linux-monitor`, follow the prompts, reapply chezmoi config, and restart the window manager.

Perform additional setup found in [additional-setup](additional-setup.md)

## Update Setup

`arch-update` command updates the system with ansible. Run `arch-update -h` for information on usage.
