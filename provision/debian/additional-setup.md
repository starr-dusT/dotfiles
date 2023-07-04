# Additional Setup

The following documents some Fedora setup that wasn't automated with ansible.

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

## Git SSH for personal and work

- ~/.gitconfig - personal github configuration.
- ~/devel/work/.gitconfig - work gitlab configuration.

Gitconfig files for SSH git push/pull are automaitcally placed. The only
additional configuration required is the transfer of SSH keys (see Syncthing
section).

## Firefox

Transfer the `.mozilla` folder from install-to-install to maintain Firefox
settings and configurations.

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

## Google chrome

[Google chrome](https://www.google.com/chrome/) is gross, but I like to watch baseball. 
Download the RPM and install with yum.