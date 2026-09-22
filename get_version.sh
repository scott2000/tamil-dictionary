#!/bin/sh

set -e

if [ -d .jj ]; then
  jj --no-pager --color=never log -G -T 'commit_id.short(12)' -r 'coalesce(@ ~ empty(), first_parent(@))'
elif [ -z "$(git status --porcelain)" ]; then
  git rev-parse --short=12 HEAD
else
  echo "dirty"
fi
