if ! DetectVM; then        # No audio needed in VMs
	AddPackage alsa-utils     # Advanced Linux Sound Architecture - Utilities
	AddPackage pipewire       # Low-latency audio/video router and processor
	AddPackage pipewire-alsa  # Low-latency audio/video router and processor - ALSA configuration
	AddPackage pipewire-pulse # Low-latency audio/video router and processor - PulseAudio replacement
	AddPackage wireplumber    # Session / policy manager implementation for PipeWire

	CreateLink /etc/systemd/user/pipewire.service.wants/wireplumber.service /usr/lib/systemd/user/wireplumber.service
	CreateLink /etc/systemd/user/pipewire-session-manager.service /usr/lib/systemd/user/wireplumber.service
	CreateLink /etc/systemd/user/sockets.target.wants/pipewire.socket /usr/lib/systemd/user/pipewire.socket
	CreateLink /etc/systemd/user/sockets.target.wants/pipewire-pulse.socket /usr/lib/systemd/user/pipewire-pulse.socket
fi
