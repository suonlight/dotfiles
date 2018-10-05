# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
# User configuration

function kong-start () {
  cd ~/projects/kong && docker-compose up
}

function spacemacs-upgrade() {
  cd ~/.emacs.d && git pull -r
}

function txsa() {
  tmuxinator start emacs &
  tmuxinator start services
}

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
ZSH_THEME="ys"
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
    git-open
    autojump
    rake
    docker
    ruby
    rails
    bundler
    # golang
    rvm
    kubectl
    zsh-autosuggestions
    colored-man-pages
    zsh-syntax-highlighting
    yarn
    docker-compose
    vi-mode
    tmuxinator
    tmux
    vundle
)

if [[ -n $INSIDE_EMACS ]]; then
    plugins=(
        git
        git-open
        autojump
        rake
        docker
        ruby
        rails
        bundler
        # golang
        rvm
        kubectl
        zsh-autosuggestions
        colored-man-pages
        zsh-syntax-highlighting
        yarn
        docker-compose
        vi-mode
    )
fi

source $ZSH/oh-my-zsh.sh

bindkey jk vi-cmd-mode
bindkey "\C-b" backward-char
bindkey "\C-f" forward-char
bindkey '\ef' emacs-forward-word
bindkey '\eb' emacs-backward-word

# if ! [[ -n $INSIDE_EMACS ]]; then
#     bindkey '^[f' vi-forward-word
#     bindkey '^[b' backward-word
#     bindkey '^e' end-of-line
#     bindkey '^a' beginning-of-line
#     bindkey '^k' forward-kill-word
#     bindkey '^r' history-incremental-search-backward
# fi

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias m=make

# emacs
export ALTERNATE_EDITOR=""
export VISUAL=nvim
# export EDITOR=vim
# export EDITOR="emacsclient -t"                  # $EDITOR should open in terminal
#export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI with non-daemon as alternate
# alias emacs-plus=~/projects/emacs/src/emacs
alias e='emacs -nw'
alias ec='emacsclient -a "" -t -c'
alias ecw='emacsclient -a "" -c'
alias ek='emacsclient -e "(kill-emacs)"'
alias vim=nvim
alias vimdiff='nvim -d'
alias vi=vim
alias k=kubectl
alias cdu='cd "$(git rev-parse --show-cdup)"'
alias c=clear
alias gop='git open'
alias ls='ls --color=auto'
# alias ssh='TERM=xterm-256color ssh'
export EDITOR='nvim'
export BUNDLER_EDITOR='nvim'

export TERM=xterm-24bit

if [[ -n $INSIDE_EMACS ]]; then
    # alias nvim='emacsclient $@'
    # alias vim='emacsclient $@'
    export EDITOR=emacsclient
    export VISUAL=$EDITOR
    # export TERM=dumb
    export TERM=eterm-256color
fi

# export GTAGSLABEL=ctags
# export GTAGSCONF=~/.globalrc

export KEYTIMEOUT=0
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export RPROMPT=""
# test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH="$PATH:$HOME/.editor/bin"
export PATH="$PATH:$HOME/Library/Python/3.6/bin"
export PATH="$PATH:/usr/local/bin"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.bin"
# # Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
