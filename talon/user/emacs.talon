app.name: Emacs
-
# key_wait increases the delay when pressing keys (milliseconds)
# this is useful if an app seems to jumble or drop keys
settings():
    key_wait = 6.0
    speech.timeout = 0.200

tag(): editor

# define some voice commands
find file: key(ctrl-p)
switch buffer: key(cmd-b)

open jira: key("g o j")
open log: key("g o l")
open tracing: key("g o t")
open monitor: key("g o m")
open ci: key("g o c")

paste: "p"

# line
copy this line: "yy"
delete this line: "dd"

# word
copy word: "yw"
delete word: "dw"
select word: "vw"

# search
search here: " ss"
search project: " *"

# org-mode
note today: " nrdt"
note yesterday: " nrdy"
note tomorrow: " nrdm"

# project
switch project: " pp"
workspace one: key(cmd-1)
workspace next: "gt"
workspace previous: "gT"

quit: key(ctrl-g)

