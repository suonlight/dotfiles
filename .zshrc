# Path to your oh-my-zsh installation.
# autoload -Uz compinit && compinit
# zmodload zsh/zprof

export ZSH=$HOME/.oh-my-zsh
# User configuration

capture() {
    sudo dtrace -p "$1" -qn '
        syscall::write*:entry
        /pid == $target && arg0 == 1/ {
            printf("%s", copyinstr(arg1, arg2));
        }
    '
}

# source ~/.fonts/*.sh
# POWERLEVEL9K_MODE='awesome-fontconfig'

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="powerlevel9k/powerlevel9k"
# ZSH_THEME="bullet-train"
# ZSH_THEME="amuse"
# ZSH_THEME="blinks"
# ZSH_THEME="ys"
# ZSH_THEME=""
# ZSH_THEME="junkfood"
# ZSH_THEME="agnoster"
# ZSH_THEME="spaceship"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    rake
    docker
    ruby
    bundler
    rails
    # vi-mode
    tmuxinator
    tmux
    # git-open
    # golang
    # vundle
    # rvm
    # kubectl
    # colored-man-pages
    # alias-tips
    zsh-autosuggestions
    # zsh-syntax-highlighting
    # yarn
    # docker-compose
)

if [[ -n $INSIDE_EMACS ]]; then
    plugins=(
        git
        rake
        docker
        ruby
        rails
        bundler
	zsh-autosuggestions
	alias-tips
	rust
	cargo
	# vi-mode
        # git-open
        # golang
        # rvm
        # kubectl
        # colored-man-pages
        # zsh-syntax-highlighting
        # yarn
        # docker-compose
    )
fi

# faster plugin
# autojump
function j() {
  (( $+commands[brew] )) && {
    local pfx=$(brew --prefix)
    [[ -f "$pfx/etc/autojump.sh" ]] && . "$pfx/etc/autojump.sh"
    j "$@"
  }
}

# source ~/.zplug/init.zsh

# zplug "mafredri/zsh-async", from:"github", use:"async.zsh"
# zplug "sindresorhus/pure", use:pure.zsh, as:theme
# zplug "djui/alias-tips", defer:1
# zplug "zsh-users/zsh-autosuggestions", defer:3
# zplug "zsh-users/zsh-completions", defer:3

# Check for uninstalled plugins.
# if ! zplug check --verbose; then
#   printf "Install? [y/N]: "
#  if read -q; then
#    echo; zplug install
#  fi
# fi

# zplug load --verbose

autoload -Uz compinit
if [ $(date +'%j') != $(/usr/bin/stat -f '%Sm' -t '%j' ${ZDOTDIR:-$HOME}/.zcompdump) ]; then
    compinit
else
    compinit -C
fi

source $ZSH/oh-my-zsh.sh

bindkey jk vi-cmd-mode
bindkey "\C-b" backward-char
bindkey "\C-f" forward-char
bindkey '\ef' emacs-forward-word
bindkey '\eb' emacs-backward-word

# emacs
export ALTERNATE_EDITOR=""
export VISUAL=nvim
alias m=make
alias emacsclient=/usr/local/bin/emacsclient
alias e='emacs -nw'
# alias ecw='emacsclient -a "" -c -e "(user-config-gui)"'
alias ec='node --version && ruby --version && emacsclient -a "" -c'
alias ecn="emacsclient -c -F '(quote (name . \"capture\"))' -e '(activate-capture-frame)' & osascript -e 'activate application \"Emacs\"'"
alias ek='emacsclient -e "(kill-emacs)"'
alias ect='emacsclient -a "" -t -c'
alias vimdiff='nvim -d'
alias vim=nvim
alias vi=vim
alias v=vim
alias cdu='cd "$(git rev-parse --show-cdup)"'
alias c=clear
export FZF_DEFAULT_OPTS="--height 20% --reverse"
# alias ssh='TERM=xterm-256color ssh'
export EDITOR='nvim'
export BUNDLER_EDITOR='nvim'

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=2'
# export TERM=xterm
# export TERM=screen-256color
# export TERM=eterm-256color
# export TERM=xterm-24bit
export TERM=xterm-256color

if [[ -n $INSIDE_EMACS ]]; then
  alias vim='/usr/local/bin/emacsclient  -n $@'
  alias nvim=$vim
  export BUNDLER_EDITOR='/usr/local/bin/emacsclient  -n $@'
  export VISUAL=$BUNDLER_EDITOR

  export FZF_DEFAULT_COMMAND='/usr/local/bin/rg'
  export FZF_DEFAULT_OPTS="--height 20% --reverse"

  function chpwd() {
      # print -Pn "\e]51A;$(whoami)@$(hostname):$(pwd)\e\\"
      print -Pn "\e]51;$(whoami)@$(hostname):$(pwd)\e\\"
  }
fi

if [[ -n $STY ]]; then
  export TERM=xterm-256color
fi

# export GTAGSLABEL=ctags
# export GTAGSCONF=~/.globalrc

export KEYTIMEOUT=0
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export RPROMPT=""

ZSH_THEME=""
autoload -U promptinit; promptinit
prompt pure

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# source ~/projects/sandboxd/sandboxd

. $HOME/.asdf/asdf.sh

. $HOME/.asdf/completions/asdf.bash
