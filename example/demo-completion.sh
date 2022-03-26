#!/usr/bin/env bash
_demo ()
{
    COMPREPLY=()
    IFS=$'\n'
    COMPREPLY=($(./_build/default/example/demo.exe --complete "${COMP_WORDS[@]:1:COMP_CWORD}"))
}

complete -F _demo demo.exe
