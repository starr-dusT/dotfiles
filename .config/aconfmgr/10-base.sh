# Minimal system
AddPackage archlinux-keyring # Arch Linux PGP keyring
AddPackage base              # Minimal package set to define a basic Arch Linux installation
CreateFile /etc/ld.so.preload >/dev/null
CopyFile /etc/locale.conf
CopyFile /etc/locale.gen
CreateLink /etc/localtime /usr/share/zoneinfo/America/Los_Angeles

# Package management
AddPackageGroup base-devel        # Required for building AUR packages
AddPackage --foreign aconfmgr-git # A configuration manager for Arch Linux
AddPackage pacman-contrib         # Contributed scripts and tools for pacman systems
AddPackage --foreign paru         # Feature packed AUR helper
AddPackage pkgfile                # a pacman .files metadata explorer
AddPackage reflector              # A Python 3 module and script to retrieve and filter the latest Pacman mirror list.
CopyFile /etc/makepkg.conf
CopyFile /etc/pacman.conf


# Default kernel
AddPackage linux         # The Linux kernel and modules
AddPackage linux-headers # Headers and scripts for building modules for the Linux kernel

# Zen kernel
AddPackage linux-zen         # The Linux kernel and modules
AddPackage linux-zen-headers # Headers and scripts for building modules for the Linux kernel

# Backup kernel
AddPackage linux-lts         # The LTS Linux kernel and modules
AddPackage linux-lts-headers # Headers and scripts for building modules for the LTS Linux kernel

# Boot
AddPackage --foreign systemd-boot-pacman-hook # Pacman hook to upgrade systemd-boot after systemd upgrade.

# Firmware
AddPackage linux-firmware # Firmware files for Linux

# EFI filesystem support
AddPackage exfat-utils # Utilities for exFAT file system

# Main filesystem support
AddPackage e2fsprogs # Ext2/3/4 filesystem utilities

if ! DetectVM; then

		# Processor support
		DetectAMD && AddPackage amd-ucode     # Microcode update image for AMD CPUs
		DetectIntel && AddPackage intel-ucode # Microcode update files for Intel CPUs

		# Hardware support
		AddPackage fwupd                  # Simple daemon to allow session software to update firmware
		DetectNVME && AddPackage nvme-cli # NVM-Express user space tooling for Linux

		# Extra file system support... just in case
		AddPackage afpfs-ng       # A client for the Apple Filing Protocol (AFP)
		AddPackage btrfs-progs    # Btrfs filesystem utilities
		AddPackage dosfstools     # DOS filesystem utilities
		AddPackage f2fs-tools     # Tools for Flash-Friendly File System (F2FS)
		AddPackage jfsutils       # JFS filesystem utilities
		AddPackage kbfs           # The Keybase filesystem
		AddPackage mtools         # A collection of utilities to access MS-DOS disks
		AddPackage mtpfs          # A FUSE filesystem that supports reading and writing from any MTP device
		AddPackage nilfs-utils    # A log-structured file system supporting continuous snapshotting (userspace utils)
		AddPackage ntfs-3g        # NTFS filesystem driver and utilities
		AddPackage reiserfsprogs  # Reiserfs utilities
		AddPackage squashfs-tools # Tools for squashfs, a highly compressed read-only filesystem for Linux.
		AddPackage sysfsutils     # System Utilities Based on Sysfs
		AddPackage truecrypt      # Free open-source cross-platform disk encryption software
		AddPackage xfsprogs       # XFS filesystem utilities

		# Sensors
		AddPackage lm_sensors # Collection of user space tools for general SMBus access and hardware monitoring
		CreateLink /etc/systemd/system/multi-user.target.wants/lm_sensors.service /usr/lib/systemd/system/lm_sensors.service
	fi
fi

# Reduce timeouts to sane values
cat >>"$(GetPackageOriginalFile systemd /etc/systemd/system.conf)" <<-EOF
	RuntimeWatchdogSec=10min
	ShutdownWatchdogSec=10min
	DefaultTimeoutStartSec=30s
	DefaultTimeoutStopSec=30s
EOF

# Arch linux enables these GPG agent systemd user sockets by default
CreateLink /etc/systemd/user/sockets.target.wants/dirmngr.socket /usr/lib/systemd/user/dirmngr.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent-browser.socket /usr/lib/systemd/user/gpg-agent-browser.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent-extra.socket /usr/lib/systemd/user/gpg-agent-extra.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent.socket /usr/lib/systemd/user/gpg-agent.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent-ssh.socket /usr/lib/systemd/user/gpg-agent-ssh.socket

# Network Time Protocol
CreateLink /etc/systemd/system/dbus-org.freedesktop.timesync1.service /usr/lib/systemd/system/systemd-timesyncd.service
CreateLink /etc/systemd/system/sysinit.target.wants/systemd-timesyncd.service /usr/lib/systemd/system/systemd-timesyncd.service
