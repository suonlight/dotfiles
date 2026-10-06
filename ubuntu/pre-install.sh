#!/bin/bash

echo "=== Installing system packages ==="
sudo apt update
sudo apt install -y \
  libncurses-dev libpq-dev libicu-dev cmake pkg-config \
  unzip bison liblzma-dev libenchant-2-dev \
  libfontconfig1-dev \
  curl \
  libsystemd-dev libjansson-dev gnupg \
  libcurl4-gnutls-dev \
  wl-clipboard \
  libtool libtool-bin \
  libyaml-dev \
  autojump \
  ripgrep \
  cargo

echo "=== Granting User without sudo ==="
sudo groupadd -f input
sudo usermod -aG input,input $USER
echo 'KERNEL=="uinput", GROUP="input", MODE="0660"' | sudo tee /etc/udev/rules.d/99-input.rules
sudo udevadm control --reload-rules && sudo udevadm trigger

echo "=== Installing Fonts ==="
sudo apt install -y fonts-firacode

echo "=== Installing ibus-unikey ==="
    sudo apt install ibus-unikey


gsettings set org.gnome.desktop.wm.keybindings panel-main-menu "['<Super>space']"
gsettings get org.gnome.desktop.wm.keybindings panel-main-menu
gsettings set org.gnome.mutter overlay-key ''
gsettings get org.gnome.mutter overlay-key
gsettings set org.gnome.shell.keybindings toggle-application-view "['<Super>space']"
gsettings get org.gnome.shell.keybindings toggle-application-view
gsettings set org.gnome.desktop.wm.keybindings switch-input-source "['<Shift><Super>period']"
gsettings get org.gnome.desktop.wm.keybindings switch-input-source

gsettings set org.gnome.shell.extensions.dash-to-dock hot-keys false
gsettings set org.gnome.shell.keybindings switch-to-application-1 "[]"
gsettings set org.gnome.shell.keybindings switch-to-application-2 "[]"
gsettings set org.gnome.shell.keybindings switch-to-application-3 "[]"
gsettings set org.gnome.shell.keybindings switch-to-application-4 "[]"
gsettings set org.gnome.shell.keybindings switch-to-application-5 "[]"
gsettings set org.gnome.shell.keybindings switch-to-application-6 "[]"
gsettings set org.gnome.shell.keybindings switch-to-application-7 "[]"
gsettings set org.gnome.shell.keybindings switch-to-application-8 "[]"
gsettings set org.gnome.shell.keybindings switch-to-application-9 "[]"
gsettings set org.gnome.shell.keybindings toggle-message-tray "[]"

# check: $ gsettings get org.gnome.shell.keybindings switch-to-application-1
# reset: $ gsettings reset-recursively org.gnome.shell.keybindings
# $ gsettings list-recursively org.gnome.desktop.wm.keybindings | grep Super
# $ gsettings list-recursively org.gnome.shell.keybindings | grep Super
# $ gsettings list-recursively org.gnome.settings-daemon.plugins.media-keys
