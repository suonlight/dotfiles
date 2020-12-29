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
hub open file: key(escape space g o o)
hub open pr: key(escape g o g)

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
file copy: key(escape space f C)
file rename: key(escape space f R)
file delete: key(escape space f D)

# window
window: key(ctrl-w)
window close: key(cmd-w)

split window below: key(ctrl-w s)
split window right: key(ctrl-w v)

# magit
magit status: key(space g s)
commit: key(c c)
stage: key(s)
unstage: key(u)
push: key(p p)
log: key(l l)
create pull request: key(@ c p)

abort: key(ctrl-c ctrl-k)
confirm: key(ctrl-c ctrl-c)

# terminal
clear: key(c enter)
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
quit: key(q)
break: key(ctrl-g)
escape: key(escape)

quit emacs: key(escape space q q)
