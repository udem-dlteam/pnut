#!/bin/bash

shell=$1

# Function to get shell version
get_shell_version() {
  case $shell in
    bash)
      $shell --version | head -n 1
      ;;
    ksh)
      $shell --version 2>&1 | head -n 1
      ;;
    ash)
      busybox | grep "BusyBox" 2>&1 | head -n 1
      ;;
    yash)
      $shell --version 2>&1 | head -n 1
      ;;
    zsh)
      $shell --version | head -n 1
      ;;
    dash)
      dpkg -s dash | grep Version
      ;;
    mksh)
      $shell --version | head -n 1
      ;;
    pdksh)
      $shell --version 2>&1 | head -n 1
      ;;
    *)
      echo "Unknown shell"
      ;;
  esac
}

# Check if shell is installed
if ! command -v $shell &> /dev/null; then
  echo "$shell not found"
else
  version=$(get_shell_version)
  echo "$shell version: $version"
fi
