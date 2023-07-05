# Provision Arch
> \*Yes, I haven't showered in days.\*

Jumpstart scripts to install Debian with packages and configs I use.

## Usage

1. Install Arch with `archinstall` and the following settings:

![Install Options](https://github.com/starr-dusT/dotfiles/blob/master/provision/arch/img/install.png?raw=true)

2. Complete installation until the "Install the base system" step.

3. Switch to a tty and run the following commands:
   ```bash
   umount /target/boot/efi
   umount /target
   mount /dev/sdaY /mnt
   cd /mnt && mv @rootfs @
   btrfs subvolume create @home
   btrfs subvolume create @snapshots
   btrfs subvolume create @home_snapshots
   btrfs subvolume create @var
   btrfs subvolume create @tmp
   mount -o rw,noatime,space_cache=v2,compress=zstd,ssd,discard=async,subvol=@ /dev/sdaY /target
   mkdir -p /target/boot/efi
   mkdir -p /target/home
   mkdir -p /target/snapshots
   mkdir -p /target/var
   mkdir -p /target/tmp
   mount /dev/sdaX /target/boot/efi
   mount -o rw,noatime,space_cache=v2,compress=zstd,ssd,discard=async,subvol=@home /dev/sdaY /target/home
   mount -o rw,noatime,space_cache=v2,compress=zstd,ssd,discard=async,subvol=@snapshots /dev/sdaY /target/snapshots
   mount -o rw,noatime,space_cache=v2,compress=zstd,ssd,discard=async,subvol=@var /dev/sdaY /target/var
   mount -o rw,noatime,space_cache=v2,compress=zstd,ssd,discard=async,subvol=@tmp /dev/sdaY /target/tmp
   mkdir -p /target/home/snapshots
   mount -o rw,noatime,space_cache=v2,compress=zstd,ssd,discard=async,subvol=@home_snapshots /dev/sdaY /target/home/snapshots
   ```

5. Edit fstab to mount items from step 4.

6. Switch back to gui install and complete install.

7. Reboot and edit `/etc/apt/sources.list` to have these sources:
   ```
   deb http://ftp.us.debian.org/debian/ sid main contrib non-free non-free-firmware
   deb-src http://ftp.us.debian.org/debian/ sid main contrib non-free non-free-firmware
   ```
   
   Then run the following commands:
   ```bash
   sudo apt-get update
   sudo apt-get dist-upgrade
   sudo apt-get autoremove
   sudo reboot
   ```

8. Install zram with the `sudo apt install zram-tools` and edit `/etc/defaults/zramswap` and uncomment `PERCENT=25`.

9. Run the following commands:

   ```bash
   sudo apt install vim git -y
   git clone https://github.com/starr-dusT/dotfiles ~/.local/share/chezmoi 
   ```
   
   Copy `.chezmoidata.yaml.example` to `.chezmoidata.yaml` and edit with desired settings then run the following commands:
   
   ```bash
   ~/.local/share/chezmoi/provision/debian/jumpstart.sh
   ```

Perform additional setup found in [additional-setup](additional-setup.md)

## Update

`linux-update --debian` command updates the system with ansible. Run `linux-update -h` for information on usage.
