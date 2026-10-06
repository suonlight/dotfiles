#!/bin/bash

set -e

export WORKSPACE="$HOME/projects"

echo "=== Cloning dotfiles ==="
mkdir -p $WORKSPACE
cd $WORKSPACE
if [ ! -d "dotfiles" ]; then
  git clone git@github.com:suonlight/dotfiles.git
fi

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

echo "=== Installing Alacritty ==="
sudo snap install alacritty --classic

echo "=== Installing Neovim ==="
sudo apt install -y neovim

echo "=== Installing Zsh ==="
sudo apt install zsh

echo "=== Installing go ==="
if [ ! -d "$HOME/.go" ]; then
  curl https://raw.githubusercontent.com/canha/golang-tools-install-script/master/goinstall.sh | bash
  source ~/.zshrc
fi

echo "=== Installing Tmux ==="
sudo apt install tmux

echo "=== Installing tmux plugins ==="
if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

echo "=== Installing asdf ==="
if [ ! -d "$HOME/.asdf" ]; then
  go install github.com/asdf-vm/asdf/cmd/asdf@v0.20.2
fi

echo "=== Setting up asdf plugins ==="
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

echo "=== Installing Ruby ==="
asdf install ruby latest
asdf set ruby $(asdf list ruby | tail -1 | tr -d ' ')
# echo "=== Installing OpenCommit ==="
# npm install -g @ddediu/opencommit

# echo "=== Installing Ollama and tinyllama ==="
# if [ ! -d "$HOME/ollama" ]; then
#   curl -fsSL https://ollama.com/install.sh | sh
# fi
# ollama pull tinyllama:1.1b
# oco config set OCO_AI_PROVIDER='ollama' OCO_MODEL='tinyllama:1.1b'

echo "=== Installing Dropbox ==="
cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
ln -s ~/Dropbox/org-modes/roam ~/notes

echo "=== Installing Emacs ==="
sudo apt install emacs

echo "=== Installing Doom Emacs ==="
if [ ! -d "$HOME/projects/doom-emacs" ]; then
  git clone --depth 1 git@github.com:doomemacs/doomemacs.git ~/projects/doom-emacs
fi
ln -sf ~/projects/doom-emacs ~/.config/emacs
ln -sf ~/projects/dotfiles/doom ~/.config/doom
rm -rf ~/.config/doom/snippets
ln -sf ~/.config/doom/private/snippets ~/.config/doom/snippets
cd ~/.config/emacs && bin/doom install

echo "=== Ubuntu setup complete! ==="
