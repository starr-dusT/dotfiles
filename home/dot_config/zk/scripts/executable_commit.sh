#!/usr/bin/env bash
# Usage: zk-commit <directory of zk>

something_changed=`git status --porcelain`
if [ -n "$something_changed" ]; then
    git -C "$1" add .
    message=$(git -c color.status=false status | sed -n -r -e '1,/Changes to be committed:/ d' \
                                                           -e '1,3 d' \
                                                           -e '/^Untracked files:/,$ d' \
                                                           -e 's/^\s*//' \
                                                           -e '/./p')
    git -C "$1" commit -m "$message"
    exit 0
fi

# If there are no changes exit with failure 
exit 1
