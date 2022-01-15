if ! DetectVM; then
	AddPackage libvirt            # API for controlling virtualization engines (openvz,kvm,qemu,virtualbox,xen,etc)
	AddPackage dmidecode          # OPTIONAL DEPENDENCY (libvirt)
	AddPackage dnsmasq            # OPTIONAL DEPENDENCY (libvirt)
	AddPackage --foreign ebtables # OPTIONAL DEPENDENCY (libvirt)
	AddPackage gettext            # OPTIONAL DEPENDENCY (libvirt)
	AddPackage lvm2               # OPTIONAL DEPENDENCY (libvirt)
	AddPackage openbsd-netcat     # OPTIONAL DEPENDENCY (libvirt)
	AddPackage qemu               # OPTIONAL DEPENDENCY (libvirt)

	AddPackage edk2-ovmf            # Firmware for Virtual Machines (x86_64, i686)
	AddPackage qemu-arch-extra      # QEMU for foreign architectures
	AddPackage swtpm                # Libtpms-based TPM emulator with socket, character device, and Linux CUSE interface
	AddPackage virt-manager         # Desktop user interface for managing virtual machines
	AddPackage virt-viewer          # A lightweight interface for interacting with the graphical display of virtualized guest OS.
	AddPackage --foreign virtio-win # virtio drivers for Windows (2000, XP, Vista, 7, 8, 10) guests and floppy images for Windows XP

	CreateLink /etc/systemd/system/multi-user.target.wants/libvirtd.service /usr/lib/systemd/system/libvirtd.service
	CreateLink /etc/systemd/system/sockets.target.wants/libvirtd-ro.socket /usr/lib/systemd/system/libvirtd-ro.socket
	CreateLink /etc/systemd/system/sockets.target.wants/libvirtd.socket /usr/lib/systemd/system/libvirtd.socket
	CreateLink /etc/systemd/system/sockets.target.wants/virtlockd.socket /usr/lib/systemd/system/virtlockd.socket
	CreateLink /etc/systemd/system/sockets.target.wants/virtlogd.socket /usr/lib/systemd/system/virtlogd.socket
fi
