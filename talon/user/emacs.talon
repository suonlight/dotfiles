app.name: Emacs
-
# key_wait increases the delay when pressing keys (milliseconds)
# this is useful if an app seems to jumble or drop keys
settings():
    key_wait = 6.0
    speech.timeout = 0.300

tag(): editor

# define some voice commands
find file: key(ctrl-p)
switch buffer: key(cmd-b)

open jira: key(escape g o j)
open log: key(escape g o l)
open tracing: key(escape g o t)
open monitor: key(escape g o m)
open ci: key(escape g o c)
open sentry: key(escape g o s)

# org-mode
note today: " nrdt"
note yesterday: " nrdy"
note tomorrow: " nrdm"

# projects" pp"
switch project: key(escape space p p)
project shell: key(f12)
shell: key(cmd-t)

tab one: key(cmd-1)
tab next:
      key(escape)
      insert("gt")
tab last:
      key(escape)
      insert("gT")
tab close: key(cmd-w)

# buffers
buffer last:
      insert(" ")
      key(tab)

# files
file save: key(ctrl-x ctrl-s)

# window
window: key(ctrl-w)
window close: key(cmd-w)

split window below: key(ctrl-w s)
split window right: key(ctrl-w v)

# magit
magit status: key(space g s)
magit commit: key(c c)
magit stage: key(s)
magit unstage: key(u)
magit push: key(p p)

# terminal
bundle install: key(b i enter)

postrges start:
      insert("pg_ctl start")
      key(enter)

update code:
      key(g s t a)
      key(enter)
      key(g u p)
      key(enter)
      key(g s t p)
      key(enter)

db migrate: key(r d m enter)

enter: key(enter)
clear: key(c enter)

quit: key(ctrl-g)
escape: key(escape)
