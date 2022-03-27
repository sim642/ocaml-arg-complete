#!/usr/bin/env bash
_demo ()
{
    IFS=$'\n'
    COMPREPLY=($(${COMP_WORDS[0]} --complete "${COMP_WORDS[@]:1:COMP_CWORD}"))
}

complete -o default -F _demo demo.exe
