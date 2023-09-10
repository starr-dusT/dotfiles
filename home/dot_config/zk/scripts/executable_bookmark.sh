#!/usr/bin/env bash

cd "$1"
echo "Input bookmark name: "
read name
link=$(wl-paste)
echo "- [${name}](${link})" >> "$(zk list --match-strategy exact --match "title: 'Bookmarks'" --format "{{path}}")"
cd -
