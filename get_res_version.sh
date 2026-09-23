#!/bin/sh

set -e

get_commit() {
  if [ -d .jj ]; then
    jj --no-pager --color=never log -G -T 'commit_id.short(12)' -r 'coalesce(@ ~ empty(), first_parent(@))' || exit 1
  elif [ -z "$(git status --porcelain)" ]; then
    git rev-parse --short=12 HEAD || exit 1
  else
    echo "dirty"
  fi
}

git rev-parse --short=12 "$(get_commit):res"

