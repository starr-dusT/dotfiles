# Dotfiles
> My dotfiles... What do you want? 

My personal dotfiles repo. Documentation is pretty spotty. Maybe I'll fix that someday, but I probably won't. 

## Usage

Dotfiles are managed with chezmoi and Ansible (built for Void) is used for the bulk of configuration. 

## Initial Setup

Install Void with BTRFS and run the following commands:

```bash
sudo xbps-install -Syu -y
sudo xbps-install -S git chezmoi -y
git clone https://github.com/starr-dusT/dotfiles ~/.local/share/chezmoi 
```

Then edit the `.chezmoidata.yaml` file within `home` to desired settings and run the following commands:

```bash
~/.local/share/chezmoi/jumpstart.sh
```

After reboot log into window manager and run `linux-monitor`, follow the prompts, reapply chezmoi config, and restart the window manager.

Perform additional setup found in [additional-setup](additional-setup.md)

## Update Setup

`void-update` command updates the system with ansible. Run `void-update -h` for information on usage.

## TODO

- setup pavucontrol and bluetooth with scratchpads, polybar integration, etc.
- install taskopen with ansible?
- crontab for calendar and contact sync?
