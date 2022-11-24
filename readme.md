# Dotfiles
> My dotfiles... What do you want? 

My personal dotfiles repo. Documentation is pretty spotty. Maybe I'll fix that someday, but I probably won't. 

## Usage

Dotfiles are managed with stow and Ansible (built for Fedora) is used for the bulk of configuration. Setup that cannot be easily automated with Ansible is documentd in [addtional-setup](additional-setup.md).

## Initial Setup

Install Fedora Workstation and run the following commands:

```bash
sudo dnf install -y git stow
git clone https://github.com/starr-dusT/dotfiles ~/.dotfiles 
rm ~/.bashrc
cd ~/.dotfiles && stow .
bash
initial-setup
```

Perform additional setup found in [additional-setup](additional-setup.md)

## Update Setup

Run the following command with comma seperated tags:

```bash
update {tags} # valid tags: configs, updates, packages, services, once
```

## TODO

- make homesever use btrfs and fix "target" for home backups 