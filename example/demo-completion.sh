#!/usr/bin/env bash

# Temporary usage:
#   Run: source ./demo-completion.sh
#
# Permanent usage:
#   Run: echo "source $(readlink -f .)/demo-completion.sh" >> ~/.bash_completion

_demo ()
{
    IFS=$'\n'
    COMPREPLY=($(${COMP_WORDS[0]} --complete "${COMP_WORDS[@]:1:COMP_CWORD}"))
}

complete -o default -F _demo demo.exe demo_compat.exe
