#!/bin/bash

shells=("bash" "ksh" "ash" "yash" "zsh" "dash" "mksh" "pdksh") # List of shells to benchmark
output_dir="benchmark_results" # Output directory
benchmark_script="benchmark-bootstrap.sh"

# Function to get shell version
get_shell_version() {
  case $1 in
    bash)
      $1 --version | head -n 1
      ;;
    ksh)
      $1 --version 2>&1 | head -n 1
      ;;
    ash)
      busybox | grep "BusyBox" 2>&1 | head -n 1
      ;;
    yash)
      $1 --version 2>&1 | head -n 1
      ;;
    zsh)
      $1 --version | head -n 1
      ;;
    dash)
      dpkg -s dash | grep Version
      ;;
    mksh)
      $1 --version | head -n 1
      ;;
    pdksh)
      $1 --version 2>&1 | head -n 1
      ;;
    *)
      echo "Unknown shell"
      ;;
  esac
}

mkdir -p $output_dir # Create output directory if it doesn't exist

for shell in "${shells[@]}"; do # Loop through each shell
  if command -v $shell &> /dev/null; then
    echo "Running benchmark for $shell..."
    version=$(get_shell_version $shell)
    {
      echo "Shell: $shell"
      echo "Version: $version"
      echo ""
      ./$benchmark_script $shell
    } > "$output_dir/${shell}_results.txt" # Redirect output to a file
  else
    echo "$shell not found, skipping..."
  fi
done
