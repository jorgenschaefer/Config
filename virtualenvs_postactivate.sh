#!/bin/bash
# This hook is run after every virtualenv is activated.
if [ -f "$VIRTUAL_ENV/.project" ] && [ -f "$(cat "$VIRTUAL_ENV/.project")/setup.sh" ]
then
    . "$(cat "$VIRTUAL_ENV/.project")/setup.sh"
fi
