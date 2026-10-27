#!/bin/bash
set -e

# Script to fix AMD Polaris (RX 470/480/570/580/590) GPU ring timeouts,
# SDMA crashes, VM protection faults, and Wayland desktop crashes.

if [ "$EUID" -ne 0 ]; then
  echo "[-] Please run this script with sudo: sudo bash $0"
  exit 1
fi

REAL_USER="${SUDO_USER:-$USER}"
REAL_HOME=$(getent passwd "$REAL_USER" | cut -d: -f6)
DOTFILES_DIR="$REAL_HOME/projects/dotfiles"

echo "=== [1/5] Backing up and updating GRUB configuration ==="
GRUB_FILE="/etc/default/grub"
cp "$GRUB_FILE" "${GRUB_FILE}.bak.$(date +%Y%m%d%H%M%S)"

# Target kernel parameters:
# amdgpu.vm_update_mode=3 -> Use CPU for GPUVM page table updates (fixes ring sdma0 timeout & VM fault)
# pcie_aspm=off           -> Disable PCIe Active State Power Management (prevents link latency timeouts)
# amdgpu.gpu_recovery=1   -> Enable GPU automatic recovery
# amdgpu.dpm=1            -> Enable dynamic power management
# amdgpu.ppfeaturemask=0xfffd3fff -> Disable unstable stutter mode & overdrive

NEW_CMDLINE="quiet splash amdgpu.gpu_recovery=1 amdgpu.dpm=1 amdgpu.ppfeaturemask=0xfffd3fff amdgpu.vm_update_mode=3 pcie_aspm=off"

if grep -q '^GRUB_CMDLINE_LINUX_DEFAULT=' "$GRUB_FILE"; then
  sed -i "s|^GRUB_CMDLINE_LINUX_DEFAULT=.*|GRUB_CMDLINE_LINUX_DEFAULT=\"$NEW_CMDLINE\"|" "$GRUB_FILE"
else
  echo "GRUB_CMDLINE_LINUX_DEFAULT=\"$NEW_CMDLINE\"" >> "$GRUB_FILE"
fi
echo "[+] Updated $GRUB_FILE with: $NEW_CMDLINE"

echo "=== [2/5] Creating /etc/modprobe.d/amdgpu.conf ==="
cat << 'EOF' > /etc/modprobe.d/amdgpu.conf
# Options for AMD Polaris GPUs to prevent SDMA timeouts, GPUVM faults, and ring hangs
options amdgpu vm_update_mode=3 gpu_recovery=1 dpm=1 ppfeaturemask=0xfffd3fff aspm=0
EOF
echo "[+] Created /etc/modprobe.d/amdgpu.conf"

echo "=== [3/5] Updating GRUB and initramfs ==="
update-grub
update-initramfs -u

echo "=== [4/5] Checking and installing mesa-va-drivers ==="
apt update
apt install -y mesa-va-drivers || true

echo "=== [5/5] Configuring Chrome user wrapper & desktop entry ==="
if [ -d "$DOTFILES_DIR" ]; then
  mkdir -p "$REAL_HOME/.local/bin" "$REAL_HOME/.local/share/applications"
  
  cp "$DOTFILES_DIR/ubuntu/applications/google-chrome.desktop" "$REAL_HOME/.local/share/applications/google-chrome.desktop"
  chown "$REAL_USER:$REAL_USER" "$REAL_HOME/.local/share/applications/google-chrome.desktop"

  mkdir -p "$DOTFILES_DIR/ubuntu/bin"
  chmod +x "$DOTFILES_DIR/ubuntu/bin/google-chrome-stable" 2>/dev/null || true
  ln -sf "$DOTFILES_DIR/ubuntu/bin/google-chrome-stable" "$REAL_HOME/.local/bin/google-chrome-stable"
  ln -sf "$REAL_HOME/.local/bin/google-chrome-stable" "$REAL_HOME/.local/bin/google-chrome"
  chown -h "$REAL_USER:$REAL_USER" "$REAL_HOME/.local/bin/google-chrome-stable" "$REAL_HOME/.local/bin/google-chrome"
  echo "[+] Configured Chrome video offload wrapper for user $REAL_USER"
fi

echo ""
echo "===================================================================="
echo "[SUCCESS] GPU stability fixes applied successfully!"
echo "Please REBOOT your computer for the kernel parameters to take effect:"
echo "    sudo reboot"
echo "===================================================================="
