#!/bin/bash

#ROGUE_VERBOSE_TEST=1

comment() {
    if [ -z "${ROGUE_VERBOSE_TEST+x}" ]; then
        sed 's,/\*\*DEBUG\*\*/,/\*\*DEBUG,g' -i "$1"
        sed 's,/\*\*OVER\*\*/,OVER\*\*/,g' -i "$1"
    else
        echo "debug off in $1"
    fi
}

uncomment() {
    comment "$1"
    if [ -z "${ROGUE_VERBOSE_TEST+x}" ]; then
        sed 's,/\*\*DEBUG,/\*\*DEBUG\*\*/,g' -i "$1"
        sed 's,OVER\*\*/,/\*\*OVER\*\*/,g' -i "$1"
    else
        echo "debug on in $1"
    fi
}

list() {
    find . -name '*.scala'
}

foreach() {
    for file in $( list ); do
        "$1" "$file"
    done
}

print_help() {
    echo "Verbosity toggle"
    echo "   verbose.sh on     -> uncomment all debug println"
    echo "   verbose.sh off    -> comment all debug println"
}

main() {
    if [ $# -gt 1 ]; then
        echo "Too many arguments"
        print_help
        exit 2
    fi
    case "$1" in
        on) foreach uncomment;;
        off) foreach comment;;
        "")
            print_help
            exit 0
            ;;
        *)
            echo "Invalid argument $1"
            print_help
            exit 1
            ;;
    esac
}

main "$@"
