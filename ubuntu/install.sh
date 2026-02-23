#!/bin/bash

set -e

export WORKSPACE="$HOME/projects"

echo "=== Cloning dotfiles ==="
mkdir -p $WORKSPACE
cd $WORKSPACE
if [ ! -d "dotfiles" ]; then
  git clone git@github.com:suonlight/dotfiles.git
fi

echo "=== Installing system packages ==="
sudo apt update
sudo apt install -y \
  libncurses-dev libpq-dev libicu-dev cmake pkg-config \
  unzip bison liblzma-dev libenchant-2-dev \
  libsystemd-dev libjansson-dev gnupg \
  libcurl4-gnutls-dev

echo "=== Installing Neovim ==="
sudo apt install -y neovim

echo "=== Installing Oh My Zsh ==="
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

echo "=== Installing Zplug ==="
if [ ! -d "$HOME/.zplug" ]; then
  curl -sL https://raw.githubusercontent.com/zplug/installer/master/install.zsh | zsh
fi

echo "=== Installing asdf ==="
if [ ! -d "$HOME/.asdf" ]; then
  git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.18.0
fi

echo "=== Setting up asdf plugins ==="
source ~/.asdf/asdf.sh
asdf plugin add nodejs
asdf plugin add yarn
asdf plugin add ruby
asdf plugin add python
asdf plugin add postgres
asdf plugin add redis
asdf plugin add ripgrep
asdf plugin add fd
asdf plugin add fzf
asdf plugin add yq
asdf plugin add bat

echo "=== Installing latest versions of asdf tools ==="
asdf install ripgrep latest
asdf set ripgrep $(asdf list ripgrep | tail -1 | tr -d ' ')
asdf install fd latest
asdf set fd $(asdf list fd | tail -1 | tr -d ' ')
asdf install fzf latest
asdf set fzf $(asdf list fzf | tail -1 | tr -d ' ')
asdf install bat latest
asdf set bat $(asdf list bat | tail -1 | tr -d ' ')

echo "=== Installing Node.js ==="
asdf install nodejs latest
asdf set nodejs $(asdf list nodejs | tail -1 | tr -d ' ')

echo "=== Installing OpenCommit ==="
npm install -g @ddediu/opencommit

echo "=== Installing Ollama and tinyllama ==="
if [ ! -d "$HOME/ollama" ]; then
  curl -fsSL https://ollama.com/install.sh | sh
fi
ollama pull tinyllama:1.1b
oco config set OCO_AI_PROVIDER='ollama' OCO_MODEL='tinyllama:1.1b'

echo "=== Installing tmux plugins ==="
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

echo "=== Installing Doom Emacs ==="
if [ ! -d "$HOME/projects/doom-emacs" ]; then
  git clone --depth 1 git@github.com:doomemacs/doomemacs.git ~/projects/doom-emacs
fi
ln -sf ~/projects/doom-emacs ~/.config/emacs
ln -sf ~/projects/dotfiles/doom ~/.config/doom
rm -rf ~/.config/doom/snippets
ln -sf ~/.config/doom/private/snippets ~/.config/doom/snippets
cd ~/.config/emacs && bin/doom install

echo "=== Installing Fonts ==="
sudo apt install -y fonts-firacode fonts-source-code-pro

echo "=== Creating symlinks ==="
mkdir -p ~/.config/nvim
mkdir -p ~/.config/bat
rm -rf ~/.zshrc ~/.tmux.conf ~/.tmux.sp ~/.ctags ~/.editorconfig ~/.config/doom ~/.config/nvim ~/.config/alacritty
ln -sf $WORKSPACE/dotfiles/.zshrc ~/.zshrc
ln -sf $WORKSPACE/dotfiles/.tmux.conf ~/.tmux.conf
ln -sf $WORKSPACE/dotfiles/.tmux.sp ~/.tmux.sp
ln -sf $WORKSPACE/dotfiles/.editorconfig ~/.editorconfig
ln -sf $WORKSPACE/dotfiles/.ctags ~/.ctags
ln -sf $WORKSPACE/dotfiles/nvim ~/.config/nvim
ln -sf $WORKSPACE/dotfiles/doom ~/.config/doom

ln -sf $WORKSPACE/dotfiles/ubuntu/alacritty ~/.config/alacritty
ln -sf $WORKSPACE/dotfiles/bat.conf ~/.config/bat/config

ln -sf $WORKSPACE/dotfiles/xremap ~/.config/xremap
# ln -sf $WORKSPACE/dotfiles/systemd ~/.config/systemd

# rm -rf ~/.config/polybar
# ln -sf $WORKSPACE/dotfiles/polybar ~/.config/polybar

# rm -f ~/.xinitrc ~/.xprofile
# ln -sf $WORKSPACE/dotfiles/.xinitrc ~/.xinitrc
# ln -sf $WORKSPACE/dotfiles/.xprofile ~/.xprofile

echo "=== Ubuntu setup complete! ==="
