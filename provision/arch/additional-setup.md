# Additional Setup

The following documents some Fedora setup that wasn't automated with ansible.

## Snapper

Snapper is used to create snapshots with the BTRFS filesystem for root and home
directories. I'd like to make these snapshots available at grub with 
[grub-btrfs](https://github.com/Antynea/grub-btrfs), but I've found that 
akmod-nvidia breaks it. Snapper is setup with:

```bash
sudo btrfs filesystem label / *FTL ship name*

# Make /var/log subvolume
sudo mv -v /var/log /var/log-old
sudo btrfs subvolume create /var/log
sudo cp -arv /var/log-old/. /var/log/
sudo restorecon -RFv /var/log
sudo rm -rvf /var/log-old

# Add /var/log to fstab
sudo vi /etc/fstab
# UUID=<drive uuid> /var/log  btrfs subvol=var/log,compress=zstd:1 0 0
sudo systemctl daemon-reload
sudo mount -va

# Create snapper configs
sudo snapper -c root create-config /
sudo snapper -c home create-config /home

# Allow users to perform snapshots
sudo snapper -c root set-config ALLOW_USERS=$USER SYNC_ACL=yes
sudo snapper -c home set-config ALLOW_USERS=$USER SYNC_ACL=yes
sudo chown -R :$USER /.snapshots
sudo chown -R :$USER /home/.snapshots

# Add / and /home to fstab
sudo vi /etc/fstab
# UUID=<drive uuid> /.snapshots      btrfs subvol=.snapshots,compress=zstd:1 0 0
# UUID=<drive uuid> /home/.snapshots btrfs subvol=home/.snapshots,compress=zstd:1 0 0
sudo systemctl daemon-reload
sudo mount -va

# Show resulting subvolume structure
sudo btrfs subvolume list /

# Enable and start snapper timeline and cleanup services
sudo systemctl enable snapper-timeline.timer
sudo systemctl start snapper-timeline.timer 
sudo systemctl enable snapper-cleanup.timer
sudo systemctl start snapper-cleanup.timer
```

## Wireguard Client

Wireguard is nice for a home vpn and [pivpn](https://pivpn.io/) makes it easy.

1. Create client on server and copy resulting `.conf` file to local machine
2. Import to networkmanager with:
```bash
nmcli connection import type wireguard file <conf file from pivpn>
```
3. Use `nm-connection-editor` to disable automatic connection

## Mount network drives

I find fstab messing about more troubule than it is worth. Credentials are 
stored in ~/.smb. Mount network drives when needed with the following command:

```bash
linux-mount-<network drive name>
```

## Taskopen for taskwarrior

[taskopen](https://github.com/jschlatow/taskopeni) is easier to install 
manually at this point since it isn't packaged and uses nim. Might get this 
automated in the future.

```bash
curl https://nim-lang.org/choosenim/init.sh -sSf | sh # install nim
git clone https://github.com/jschlatow/taskopen.git
cd taskopen
make PREFIX=/usr
sudo make PREFIX=/usr install
```

## Syncthing 

Syncthing is used to sync folders between various computers and android. The 
ansible script should setup and run the service, but shares must be setup
via the web gui. Currently four shares exists:
- `.warrior` - `.task` and `.timewarrior` folders to sync taskwarrior tasks.
These two folders are symlinked to the home folder where taskwarrior/timewarrior 
expects them.
- `warrior` - contains text files associated with taskwarrior (mostly from
taskopen).
- `phone photos` - personal photos synched from android.
- `phone screenshots` - personal screenshots synced from android.
- `ssh_keys` - contains ssh keys for git remotes (~/.ssh/keys)
- `vimwiki` - contains text files associate with my personal vimwiki.

## Lxappearance

My GTK theme is pulled down by chezmoi, but isn't active by default. This can
be fixed with the lxappearance gui (for X sessions).

## Git SSH for personal and work

- ~/.gitconfig - personal github configuration.
- ~/devel/work/.gitconfig - work gitlab configuration.

Gitconfig files for SSH git push/pull are automaitcally placed. The only
additional configuration required is the transfer of SSH keys (see Syncthing
section).

## Firefox

Transfer the `.mozilla` folder from install-to-install to maintain Firefox
settings and configurations.

## Dracula colorscheme for gnome terminal

[Dracula](https://draculatheme.com/gnome-terminal) is used for gnome-terminal. 
Run the following commands to install:

```bash
git clone https://github.com/dracula/gnome-terminal
cd gnome-terminal
./install.sh
```

## Bluetooth Audio

In addition to the `pipewire-codec-aptx` package being required (installed
with ansible) you need to set `ControllerMode = bredr` in
`/etc/bluetooth/main.conf` to get bluetooth to work properly with audio devices.

## Linux-tkg kernel

The [linux-tkg](https://github.com/Frogging-Family/linux-tkg) kernel provides
a more responsive desktop. Install with the following:

```bash
git clone https://github.com/Frogging-Family/linux-tkg.git
cd linux-tkg
```

Set `CONFIG_EFI_HANDOVER_PROTOCOL` to `y` in 
`linux-tkg-config/<kernel-version>/config.x86_64`.

```bash
./install.sh install
sudo dnf install akmod-nvidia
```

Reboot into new kernel.

## Single GPU Passthrough to windows

I use a windows virtual machine with gpu passthrough of the few games that
won't work on linux, Zwift, and Fusion360. [This](https://github.com/ilayna/Single-GPU-passthrough-amd-nvidia)
has scripts that make that process relatively easy and [this wiki](https://gitlab.com/risingprismtv/single-gpu-passthrough/-/wikis/home)
provides good information on setting up the virtual machine in virt-manager.
The `patch.rom` required for my GPU is included in my repo. The virt-manager 
setup should usually be avoided by transfering the VM between machines:

1. Copy the VM's disks from `/var/lib/libvirt/images` on src host to the same dir on destination host
2. On the source host run `virsh dumpxml VMNAME > domxml.xml` and copy this xml to the destination host
3. On the destination host run `virsh define domxml.xml`

## Google earth pro

[Google earth](https://www.google.com/earth/versions/) is nice for visualizing
my hikes and checking out snow levels. Download the RPM and install with yum.

## Emacs

```bash
git clone git://git.sv.gnu.org/emacs.git
sudo dnf install autoconf texinfo gtk3-devel libgccjit-devel gnutls-devel ncurses-devel jansson jansson-devel
cd emacs
./autogen.sh
./configure --with-native-compilation --with-json --with-pgtk
make -j16
sudo make install
```
