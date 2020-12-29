app.name: Firefox
-
# key_wait increases the delay when pressing keys (milliseconds)
# this is useful if an app seems to jumble or drop keys
settings():
    key_wait = 6.0
    speech.timeout = 0.200

# activate the global tag "browser"
tag(): browser

# define some voice commands
switch tab: key(ctrl-tab)
tab next: key(cmd-k)
tab previous: key(cmd-option-arrow-left)
tab close: key(cmd-w)
