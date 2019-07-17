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
  tmuxinator start ehe &
  tmuxinator start services
}

function active-byebug() {
    last_active=$1
    for s in $(tmux list-sessions -F "#{session_name}")
    do
	for w in $(tmux list-windows -F "#{window_id}" -t $s)
	do
	    win=$s:$w
	    for p in $(tmux list-panes -F "#{pane_id}" -t $win)
	    do
		x=$(tmux capture-pane -p -t "$win.$p" | tail -n 1 | grep byebug)
		if [[ -n $x ]]; then
		    tmux select-pane -t "$win.$p"
		    tmux select-window -t "$win"
		    tmux attach-session -t "$s"
		    echo "$win.$p"
		    if [[ "$last_active" != "$win.$p" ]]; then
			continue
		    fi
		    return
		fi
	    done
	done
    done

    return ""
}

function watch-byebug() {
    last_active=""
    for (( ; ; ))
    do
	last_active=$(active-byebug $last_active)
	echo "xxx: " $last_active
	sleep 2
    done
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
ZSH_THEME="blinks"
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
    # zsh-autosuggestions
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

source ~/.zplug/init.zsh
zplug "djui/alias-tips", defer:3
zplug "zsh-users/zsh-autosuggestions", defer:3

zplug load

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
# alias ecw='emacsclient -a "" -c -e "(user-config-gui)"'
alias ec='node --version && ruby --version && emacsclient -a "" -c'
alias ecn="emacsclient -c -F '(quote (name . \"capture\"))' -e '(activate-capture-frame)' & osascript -e 'activate application \"Emacs\"'"
alias ek='emacsclient -e "(kill-emacs)"'
alias ect='emacsclient -a "" -t -c'
alias vim=nvim
alias vimdiff='nvim -d'
alias vi=vim
alias cdu='cd "$(git rev-parse --show-cdup)"'
alias c=clear
# alias gop='git open'
# export FZF_DEFAULT_COMMAND='/usr/local/bin/rg'
export FZF_DEFAULT_OPTS="--height 20% --reverse"
# alias ls='ls --color=auto'
# alias ssh='TERM=xterm-256color ssh'
export EDITOR='nvim'
export BUNDLER_EDITOR='nvim'

# export TERM=xterm-24bit
export TERM=xterm-256color
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=2'
# export TERM=xterm
# export TERM=screen-256color
# export TERM=eterm-256color
if [[ -n $INSIDE_EMACS ]]; then
  alias nvim='emacsclient $@'
  alias vim='emacsclient $@'
  export EDITOR=emacsclient
  export BUNDLER_EDITOR='emacsclient $@'
  export VISUAL=$EDITOR
  # export TERM=xterm-24bit
  # export TERM=xterm
  # export TERM=screen-256color
  # export TERM=eterm-256color
  # export TERM=xterm-256color
  if [[ "vterm" == "$INSIDE_EMACS" ]]; then
    # export TERM=xterm
    ZSH_THEME=""
    autoload -U promptinit; promptinit
    prompt pure
  fi
  # export TERM=dumb
  export FZF_DEFAULT_COMMAND='/usr/local/bin/rg'
  export FZF_DEFAULT_OPTS="--height 20% --reverse"
  #     export FZF_DEFAULT_OPTS="--height 20% --reverse --bind=\
  # ctrl-j:accept,ctrl-k:kill-line,tab:toggle-up,btab:toggle-down,\
  # ctrl-y:execute('echo {} | xclip -selection clipboard')+abort"
  # alias gst="emacsclient -e '(magit-status)'"
else
  ZSH_THEME=""
  autoload -U promptinit; promptinit
  prompt pure
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
# test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source ~/projects/sandboxd/sandboxd
# export PATH="$HOME/.rbenv/bin:$PATH"
# eval "$(rbenv init -)"

# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" # This loads nvm
