#!/bin/bash
set -o nounset -o pipefail -o errexit

stack build --pedantic --copy-bins
stack exec zfoh build
