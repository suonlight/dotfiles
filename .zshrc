# zmodload zsh/zprof
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Install zinit if not installed
if [ ! -d "${HOME}/.local/share/zinit" ]; then
    bash -c "$(curl --fail --show-error --silent --location https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)"
fi

### Added by zinit's installer
source ~/.local/share/zinit/zinit.git/zinit.zsh

(( ${+_comps} )) && _comps[zinit]=_zinit

# Functions to make configuration less verbose
zt() { zinit ice wait"${1}" lucid               "${@:2}"; } # Turbo
z()  { [ -z $2 ] && zinit light "${@}" || zinit "${@}"; } # zinit
# If you come from bash you might have to change your $PATH.
### End of zinit's installer chunk

zinit ice lucid pick"async.zsh" src"pure.zsh"
zinit light sindresorhus/pure
# zinit ice from"gh-r" as"program" atclone'./starship init zsh > zhook.zsh' atpull'%atclone' src"zhook.zsh"
# zinit light starship/starship

zinit snippet OMZ::lib/history.zsh
zinit snippet OMZ::lib/completion.zsh

zt 0a; zinit snippet OMZ::lib/git.zsh
zt 0b; zinit snippet OMZ::plugins/git/git.plugin.zsh
zt 0b; zinit snippet OMZ::plugins/ruby/ruby.plugin.zsh
zt 0b; zinit snippet OMZ::plugins/rake/rake.plugin.zsh
zt 0b; zinit snippet OMZ::plugins/tmuxinator/tmuxinator.plugin.zsh
zt 1b; zinit snippet OMZ::plugins/rails/rails.plugin.zsh
zt 0b; zinit snippet OMZ::plugins/bundler/bundler.plugin.zsh
zt 0b; zinit snippet OMZ::plugins/yarn/yarn.plugin.zsh
zt 0b; zinit snippet OMZ::plugins/postgres/postgres.plugin.zsh
zt 0b; zinit snippet OMZ::plugins/docker-compose/docker-compose.plugin.zsh
zt 0b; zinit light joshskidmore/zsh-fzf-history-search
zt 1a; zinit light djui/alias-tips
zt 0b atload'unalias help; unalias cp'; zinit snippet OMZ::plugins/common-aliases/common-aliases.plugin.zsh

# zinit ice as"completion"; zinit snippet OMZ::plugins/docker/_docker
zinit ice as"completion"; zinit snippet OMZ::plugins/bundler/_bundler
# zinit ice as"completion"; zinit snippet OMZ::plugins/rust/_rust
zinit ice as"completion"; zinit snippet OMZ::plugins/terraform/_terraform

zt 0b compile'{src/*.zsh,src/strategies/*}' atload'_zsh_autosuggest_start'; zinit light zsh-users/zsh-autosuggestions
# zt 0b blockf atpull'zinit creinstall -q .'; zinit light zsh-users/zsh-completions
zi for \
    atload"zicompinit; zicdreplay" \
    blockf \
    lucid \
    wait \
    zsh-users/zsh-completions && \
    fpath=($HOME/.asdf/completions $fpath)

zt 1a atinit'zpcompinit; zpcdreplay'
zinit light zdharma/fast-syntax-highlighting

# faster plugin
# autojump
function j() {
  (( $+commands[brew] )) && {
    local pfx=$(brew --prefix)
    [[ -f "$pfx/etc/autojump.sh" ]] && . "$pfx/etc/autojump.sh"
    j "$@"
  }

  test -e /usr/share/autojump/autojump.zsh && {
    source /usr/share/autojump/autojump.zsh
    j "$@"
  }

  test -e ~/.nix-profile/etc/profile.d/autojump.sh && {
    source ~/.nix-profile/etc/profile.d/autojump.sh
    j "$@"
  }
}

cd() {
  builtin cd "$@" || return
  [ "$OLDPWD" = "$PWD" ] || echo -e "\e]51;A$(pwd)\e\\"
}

bindkey -e
bindkey jk vi-cmd-mode
# List all zle commands
# zle -al | fzf

# config editor
export ALTERNATE_EDITOR=""
export EDITOR=nvim
test -n "$TMUX" && export EDITOR=nvim
export BUNDLER_EDITOR=nvim
# alias emacsclient=/usr/local/bin/emacsclient
alias ek="ps -ef | grep Emacs | awk '{ print $2 }' | xargs kill -9"
alias vimdiff='nvim -d'
alias vim=nvim
alias v=nvim

export FZF_DEFAULT_OPTS="--height 20% --reverse --border"
export FZF_DEFAULT_COMMAND='fd --type file --follow --hidden --exclude .git'
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=2'
export TERM=xterm-256color

if [[ -n $INSIDE_EMACS ]]; then
  alias nvim='emacsclient -n $@'
  export BUNDLER_EDITOR='emacsclient -n $@'
  export EDITOR='emacsclient -n $@'

  # export FZF_DEFAULT_COMMAND='/usr/local/bin/rg --files --no-ignore --hidden --follow --glob "!{.git, node_modules}"'
  export FZF_DEFAULT_COMMAND='fd --type file --follow --hidden --exclude .git'
  alias clear='printf "\e]51;Evterm-clear-scrollback\e\\";tput clear'
  vterm_prompt_end() {
      printf "\e]51;A$(whoami)@$(hostname):$(pwd)\e\\";
  }
  PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
  vterm_cmd() {
      printf "\e]51;E"
      local r
      while [[ $# -gt 0 ]]; do
        r="${1//\\/\\\\}"
        r="${r//\"/\\\"}"
        printf '"%s" ' "$r"
        shift
      done
      printf "\e\\"
  }
  find_file() {
      vterm_cmd find-file "$(realpath "$@")"
  }
fi

if [[ -n $STY ]]; then
  export TERM=xterm-256color
fi

# aliases
alias cdu='cd "$(git rev-parse --show-cdup)"'
alias m=make
alias c=clear
alias find=fd
alias de='asdf current'

# customize pure
export PURE_PROMPT_SYMBOL='$'

# Need tmux 3.2 or later
if [[ "$TERM_PROGRAM" = "tmux" ]]; then
  zstyle :prompt:pure:path color yellow
fi

test -d ~/.asdf/plugins/java/set-java-home.zsh && . ~/.asdf/plugins/java/set-java-home.zsh

# . $HOME/.asdf/asdf.sh
# mkdir -p "$HOME/.asdf/completions"
# asdf completion zsh > "$HOME/.asdf/completions/_asdf"
### End of Zinit's installer chunk

export PATH="/usr/local/opt/texinfo/bin:$PATH"
export PATH="$HOME/.asdf/shims:$PATH"

setopt interactivecomments

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix
### End of Zinit's installer chunk

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk
