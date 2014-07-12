#!/bin/bash
# This hook is run after every virtualenv is activated.
if [ -f "$VIRTUAL_ENV/.project" ]
then
    PROJECT_ROOT="$(cat "$VIRTUAL_ENV/.project")"
    for filename in setup.sh .venv-setup.sh
    do
        if [ -f "$PROJECT_ROOT/$filename" ]
        then
            . "$PROJECT_ROOT/$filename"
        fi
    done
    if [ -f "$PROJECT_ROOT/.env" ]
    then
        export $(cat "$PROJECT_ROOT/.env" | grep '^[^#]')
    fi
fi
