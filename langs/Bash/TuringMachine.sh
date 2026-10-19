#!/usr/bin/env bash

# The canonical machine: a binary incrementer.  In q0 a 1 is rewritten to 0 and
# the head moves left (carry); a 0 becomes 1 and the machine halts; a blank
# becomes 1 (a new leading digit) and the machine halts.
IFS= read -r line || line=""

case "$line" in
    *[!01]*)
        printf '%s\n' "Invalid input: expected a binary string."
        exit 0
        ;;
esac

tape="$line"
head=$(( ${#tape} - 1 ))

while true; do
    if [ "$head" -lt 0 ]; then
        tape="1$tape"
        break
    fi
    symbol="${tape:$head:1}"
    if [ "$symbol" = "1" ]; then
        tape="${tape:0:$head}0${tape:$((head + 1))}"
        head=$(( head - 1 ))
    else
        tape="${tape:0:$head}1${tape:$((head + 1))}"
        break
    fi
done

printf 'Initial tape: %s\n' "$line"
printf 'Final tape: %s\n' "$tape"
