# Provision Fedora
> \*Tips Fedora\*

Jumpstart scripts to install Fedora with packages and configs I use.

## Usage

Install Fedora Workstation with BTRFS and default partitions.

Run the following commands:

```bash
sudo dnf install vim git -y
git clone https://github.com/starr-dusT/dotfiles ~/.local/share/chezmoi 
```

Copy `.chezmoidata.yaml.example` to `.chezmoidata.yaml` and edit with desired settings then run the following commands:

```bash
~/.local/share/chezmoi/provision/fedora/jumpstart.sh
```

Perform additional setup found in [additional-setup](additional-setup.md)

## Update Setup

`linux-update --fedora` command updates the system with ansible. Run `linux-update -h` for information on usage.
