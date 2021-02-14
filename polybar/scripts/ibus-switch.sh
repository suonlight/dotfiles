#!/bin/bash

if [[ `ibus engine | awk -F":" '{ print $2 }'` == "us" ]]; then
  ibus engine 'Bamboo'
else
  ibus engine 'xkb:us:dvorak:eng'
fi

polybar-msg hook ibus 1
