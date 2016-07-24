#!/bin/bash

temp="$(emacsclient -e "(sudo-askpass \"$1\")")"
temp="${temp%\"}"
temp="${temp#\"}"
echo "$temp"
