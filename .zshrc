# zmodload zsh/zprof
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Install zplugin if not installed
if [ ! -d "${HOME}/.zplugin" ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zplugin/master/doc/install.sh)"
fi

### Added by Zplugin's installer
source $HOME/.zplugin/bin/zplugin.zsh
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

# Functions to make configuration less verbose
zt() { zplugin ice wait"${1}" lucid               "${@:2}"; } # Turbo
zi() { zplugin ice lucid                            "${@}"; } # Regular Ice
z()  { [ -z $2 ] && zplugin light "${@}" || zplugin "${@}"; } # zplugin
# If you come from bash you might have to change your $PATH.
### End of Zplugin's installer chunk

zplugin ice lucid pick"async.zsh" src"pure.zsh"
zplugin light sindresorhus/pure

zplugin snippet OMZ::lib/history.zsh
zplugin snippet OMZ::lib/completion.zsh

zt 0a; zplugin snippet OMZ::lib/git.zsh
zt 0b; zplugin snippet OMZ::plugins/git/git.plugin.zsh
zt 0b; zplugin snippet OMZ::plugins/ruby/ruby.plugin.zsh
zt 0b; zplugin snippet OMZ::plugins/rake/rake.plugin.zsh
zt 0b; zplugin snippet OMZ::plugins/tmuxinator/tmuxinator.plugin.zsh
zt 1b; zplugin snippet OMZ::plugins/rails/rails.plugin.zsh
zt 0b; zplugin snippet OMZ::plugins/bundler/bundler.plugin.zsh
zt 0b; zplugin snippet OMZ::plugins/docker-compose/docker-compose.plugin.zsh
zt 1a; zplugin light djui/alias-tips
zt 0b atload'unalias help'; zplugin snippet OMZ::plugins/common-aliases/common-aliases.plugin.zsh

zplugin ice as"completion"; zplugin snippet OMZ::plugins/docker/_docker
zplugin ice as"completion"; zplugin snippet OMZ::plugins/bundler/_bundler
zplugin ice as"completion"; zplugin snippet OMZ::plugins/rust/_rust
zplugin ice as"completion"; zplugin snippet OMZ::plugins/cargo/_cargo

zt 0b compile'{src/*.zsh,src/strategies/*}' atload'_zsh_autosuggest_start'; zplugin light zsh-users/zsh-autosuggestions
zt 0b blockf atpull'zplugin creinstall -q .'; zplugin light zsh-users/zsh-completions

zt 1a atinit'zpcompinit; zpcdreplay'
zplugin light zdharma/fast-syntax-highlighting

# faster plugin
# autojump
function j() {
  (( $+commands[brew] )) && {
    local pfx=$(brew --prefix)
    [[ -f "$pfx/etc/autojump.sh" ]] && . "$pfx/etc/autojump.sh"
    j "$@"
  }
}

bindkey jk vi-cmd-mode
bindkey "\C-b" backward-char
bindkey "\C-f" forward-char
bindkey "\C-a" beginning-of-line
bindkey "\C-e" end-of-line
bindkey '\ef' emacs-forward-word
bindkey '\eb' emacs-backward-word

# config editor
export ALTERNATE_EDITOR=""
# export VISUAL=nvim
# export EDITOR='nvim'
test -n "$TMUX" && export EDITOR=nvim
export BUNDLER_EDITOR=nvim
alias emacsclient=/usr/local/bin/emacsclient
alias e='emacs -nw'
alias ec='node --version && ruby --version && emacsclient -a "" -c'
alias ecn="emacsclient -c -F '(quote (name . \"capture\"))' -e '(activate-capture-frame)' & osascript -e 'activate application \"Emacs\"'"
alias ek='emacsclient -e "(kill-emacs)"'
alias ect='emacsclient -a "" -t -c'
alias vimdiff='nvim -d'
alias vim=nvim
alias vi=vim
alias v=vim

export FZF_DEFAULT_OPTS="--height 20% --reverse"
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=2'
export TERM=xterm-256color

if [[ -n $INSIDE_EMACS ]]; then
  alias vim='/usr/local/bin/emacsclient -n $@'
  alias nvim=vim
  export BUNDLER_EDITOR='/usr/local/bin/emacsclient -n $@'
  export EDITOR='/usr/local/bin/emacsclient -n $@'
  # export VISUAL=$BUNDLER_EDITOR

  # export FZF_DEFAULT_COMMAND='/usr/local/bin/rg'
  export FZF_DEFAULT_COMMAND='/usr/local/bin/rg --files --no-ignore --hidden --follow --glob "!{.git, node_modules}"'

  function chpwd() {
      print -Pn "\e]51;$(whoami)@$(hostname):$(pwd)\e\\"
  }
fi

if [[ -n $STY ]]; then
  export TERM=xterm-256color
fi

# aliases
alias cdu='cd "$(git rev-parse --show-cdup)"'
alias m=make
alias c=clear

# other tools
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

. $HOME/.asdf/asdf.sh
# . $HOME/.asdf/completions/asdf.bash
