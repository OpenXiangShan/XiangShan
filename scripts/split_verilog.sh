#!/bin/bash

path=$1
origin=$2
current=$origin

mkdir -p $path/tmp

while :; do
	res=$(grep -noPm 1 '(?<=// ----- 8< ----- FILE ").*(?=" ----- 8< -----)' "$path/$origin")
	if [ -z "$res" ]; then break; fi
	lastline=$(echo "$res" | grep -o '[0-9]*' | head -1)
	directory=$(echo "$current" | grep -o '.*/')
	if [ -n "$directory" ]; then mkdir -p "$path/tmp/$directory"; fi
	head -n $(($lastline - 2)) "$path/$origin" >"$path/tmp/$current"
	sed -i "1,$(($lastline + 1))d" "$path/$origin"
	current=$(echo "$res" | grep -oP "(?<=$lastline:).*")
done

mv "$path/$origin" "$path/$current" 2>/dev/null
mv "$path/tmp"/* "$path/" 2>/dev/null
rm -rf "$path/tmp"
