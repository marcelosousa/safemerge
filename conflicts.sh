#!/bin/bash

# This script goes through the git log of a repository 
# and for every merge commit, it finds out the common ancestor
# and checks the result of the git merge

# Hack to read entire line
OLDIFS="$IFS"
IFS=$'\n'

# Counters for number of merges
# and number of conflicts
NM=0
NC=0

# Determines all the merge branches
for line in `git log --merges --pretty=format:%P`; do
  # Reverse the hack
  IFS=$OLDIFS
  # Find base
  base=`git merge-base --octopus $line`
  
  echo "Merging base=$base with branches=$line..."
  # Dry run
  result=`git merge-tree $base $line`
  # If the result contains the string changed in both
  # its very likely that there is a conflict 
  if [[ $result == *"changed in both"* ]]
  then 
    echo "Potential conflict!";
    NC=$((NC+1))
  fi 
  NM=$((NM+1))
done

echo "Number of merges=$NM, number of conflicts=$NC"
echo "Done"
