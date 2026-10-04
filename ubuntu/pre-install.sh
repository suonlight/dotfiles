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
  libtool \
  cargo

echo "=== Granting User without sudo ==="
sudo groupadd -f input
sudo usermod -aG input,input $USER
echo 'KERNEL=="uinput", GROUP="input", MODE="0660"' | sudo tee /etc/udev/rules.d/99-input.rules
sudo udevadm control --reload-rules && sudo udevadm trigger

echo "=== Installing Fonts ==="
sudo apt install -y fonts-firacode

