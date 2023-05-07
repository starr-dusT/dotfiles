# Provision Arch 
> \*I use arch btw\*

Jumpstart scripts to install Arch with packages and configs I use.

## Usage

Install Arch with BTRFS and partition:

```
1. 512Mb EFI partition at /boot/EFI
2. BTRFS volume with subvolumes: 
       @ -> /
       @home -> /home
       @log -> /var/log
       @pkg -> /var/cache/pacman/pkg
       @.snapshots -> /.snapshots
       @home/.snapshots /home/.snapshots
```

Select pipwire for audio, networkmanager for network, and install.

Run the following commands:

```bash
pacman -S --needed git base-devel vim

# Install yay
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si

# Get jumpstart files
sudo dnf install vim git -y
git clone --recurse-submodules https://github.com/starr-dusT/dotfiles ~/.local/share/chezmoi 
```

Copy `.chezmoidata.yaml.example` to `.chezmoidata.yaml` and edit with desired settings then run the following commands:

```bash
~/.local/share/chezmoi/provision/arch/jumpstart.sh
```

Perform additional setup found in [additional-setup](additional-setup.md)

## Update Setup

`linux-update --arch` command updates the system with ansible. Run `linux-update -h` for information on usage.
