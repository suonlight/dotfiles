#####################
### DESIGN CHANGES ###
######################

# loud or quiet?
set-option -g visual-activity off
set-option -g visual-bell off
set-option -g visual-silence off
set-window-option -g monitor-activity off
set-option -g bell-action none

set -g default-terminal "screen-256color"

# The modes {

setw -g clock-mode-colour colour135
setw -g mode-style fg=colour196,bg=colour238,bold

# }

# The panes {

set -g pane-border-style bg=colour235,fg=colour238
set -g pane-active-border-style bg=colour236,fg=colour51

# }

# The statusbar {

set -g status-justify centre
set -g status-interval 2
set -g status-position top
set -g status-style bg=colour234,fg=colour137,dim
set -g status-left "#[fg=colour16,bg=colour254,bold] #S #[fg=colour254,bg=colour234,nobold,nounderscore,noitalics]"
set -g status-right '#[fg=colour233,bg=colour241,bold] %Y-%m-%d #[fg=colour233,bg=colour245,bold] %H:%M:%S '
set -g status-right-length 50
set -g status-left-length 20

# }

# window status {

setw -g window-status-current-style fg=colour81,bg=colour238,bold
setw -g window-status-current-format ' #I#[fg=colour250]:#[fg=colour255]#W#[fg=colour50]#F '
setw -g window-status-style fg=colour138,bg=colour235,none
setw -g window-status-format ' #I#[fg=colour237]:#[fg=colour250]#W#[fg=colour244]#F '
setw -g window-status-bell-style fg=colour255,bg=colour1,bold

# }

# The messages {

set -g message-style fg=colour232,bg=colour166,bold
set -g message-command-style fg=blue,bg=black

# }
