# Provision Arch
> \*Yes, I haven't showered in days.\*

Jumpstart scripts to install Arch with packages and configs I use.

## Usage

1. Install Arch with `archinstall` and the following settings:

   ![Install Options](https://github.com/starr-dusT/dotfiles/blob/master/provision/arch/img/install.png?raw=true)

2. Edit fstab for btrfs subvolumes to have `rw,noatime,compress=zstd`. 

3. Run the following commands:

   ```bash
   sudo pacman -S install vim git
   git clone https://github.com/starr-dusT/dotfiles ~/.local/share/chezmoi 
   ```
   
   Copy `.chezmoidata.yaml.example` to `.chezmoidata.yaml` and edit with desired settings then run the following commands:
   
   ```bash
   ~/.local/share/chezmoi/provision/arch/jumpstart.sh
   ```

Perform additional setup found in [additional-setup](additional-setup.md)

## Update

`linux-update --arch` command updates the system with ansible. Run `linux-update -h` for information on usage.
