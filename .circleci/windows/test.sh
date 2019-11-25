#!/usr/bin/env bash

# Build project
# $PROJECT_ROOT - project dir or run the scripts from project root

set -e

export MSYS=winsymlinks:nativestrict

PROJECT_ROOT=${PROJECT_ROOT:-`pwd`}

bold="\\e[1;33m"
reset="\\e[0m"

echo -e "${bold}Start test${reset}"
cd ${PROJECT_ROOT}
time ./rebar3 eunit -v

echo -e "${bold}Done.${reset}"
