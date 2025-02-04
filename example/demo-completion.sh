#!/usr/bin/env bash

# Temporary usage:
#   Run: source ./demo-completion.sh
#
# Permanent usage:
#   Run: echo "source $(readlink -f .)/demo-completion.sh" >> ~/.bash_completion

# Bypass = in COMP_WORDBREAKS (https://stackoverflow.com/a/57437406/854540)
# Copied & modified from standard __ltrim_colon_completions
__ltrim_equal_completions()
{
    if [[ $1 == *=* && $COMP_WORDBREAKS == *=* ]]; then
        # Remove equal-word prefix from COMPREPLY items
        local equal_word=${1%"${1##*=}"}
        local i=${#COMPREPLY[*]}
        while ((i-- > 0)); do
            COMPREPLY[i]=${COMPREPLY[i]#"$equal_word"}
        done
    fi
}

_demo ()
{
    IFS=$'\n'
    local words cword cur
    _get_comp_words_by_ref -n = cur words cword # Bypass = in COMP_WORDBREAKS (https://stackoverflow.com/a/57437406/854540)
    COMPREPLY=($(${words[0]} --complete "${words[@]:1:cword}"))
    __ltrim_equal_completions "$cur" # Bypass = in COMP_WORDBREAKS (https://stackoverflow.com/a/57437406/854540)
}

complete -o default -F _demo demo.exe demo_compat.exe
