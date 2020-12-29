app.name: Emacs
-
# key_wait increases the delay when pressing keys (milliseconds)
# this is useful if an app seems to jumble or drop keys
settings():
    key_wait = 6.0
    speech.timeout = 0.200

tag(): editor

left: "h"
right: "l"
up: "k"
down: "j"

# actions
append: "a"
append line: "A"

insert: "i"
insert line down: "o"
insert line up: "O"

delete: "d"
delete line: "dd"
opposite: "o"

undo: "u"
redo: key(ctrl-r)

yank: "y"
yank line: "yy"
paste: "p"
repeat: "."
again: ";"

# nouns
word: "w"
big word: "W"

# adj
inner: "i"
arround: "a"

# nav
lend: "$"
bend: "^"
