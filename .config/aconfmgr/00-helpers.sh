# Fail if not in Windows Subsystem for Linux
function DetectWSL() {
	grep -i -q microsoft /proc/version &>/dev/null
	return $?
}

# Fail if not in a container / virtual machine / WSL
function DetectVM() {
	# https://www.freedesktop.org/software/systemd/man/systemd-detect-virt.html
	systemd-detect-virt --quiet &>/dev/null
	return $?
}

# Fail if not in VirtualBox
function DetectVirtualBox() {
	systemd-detect-virt | grep -i -q oracle
	return $?
}

# Fail if not on an AMD processor
function DetectAMD() {
	grep -i -q AMD /proc/cpuinfo &>/dev/null
	return $?
}

# Fail if not on an Intel processor
function DetectIntel() {
	grep -i -q Intel /proc/cpuinfo &>/dev/null
	return $?
}

# Fail if no NVME drives are found
function DetectNVME() {
	find /dev -maxdepth 1 -name '*nvme*' | grep -i -q nvme &>/dev/null
	return $?
}

# Fail if no AMD GPU is found
function DetectAMDGPU() {
	lspci -v | grep -i 'amd/ati' | grep -i -q 'vga' &>/dev/null
	return $?
}

# Fail if no Intel GPU is found
function DetectIntelGPU() {
	lspci -v | grep -i 'intel' | grep -i -q 'vga' &>/dev/null
	return $?
}

# Fail if no Nvidia GPU is found
function DetectNvidia() {
	lspci -v | grep -i -q nvidia &>/dev/null
	return $?
}

# Fail if no encrypted devices are listed in /etc/fstab
function DetectCrypt() {
	grep -q '/dev/mapper/' /etc/fstab
	return $?
}
