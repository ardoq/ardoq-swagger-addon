#!/bin/bash

# This short scipt is used to conditionally runs its first (and only)
# argument as a script, unless the commit message at the tip of the
# branch being built contains the string [ci skip tests]

function skip() {
    return 0;
}
if cat ".git/refs/heads/$CIRCLE_BRANCH" | git show -s --format=%B | egrep -q '\[ci skip tests?\]'
then
    echo "Skipped command $1";
    skip;
else
    eval $1;
fi
