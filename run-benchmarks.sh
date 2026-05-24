#!/bin/bash

shells=("bash" "ksh" "ash" "yash" "zsh" "dash") # List of shells to benchmark
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
    *)
      echo "Unknown shell"
      ;;
  esac
}

mkdir -p $output_dir # Create output directory if it doesn't exist

# Loop through each shell and run the bootstrap benchmark script
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

#output end message
echo "Benchmarking complete. Results saved in $output_dir"

# Call gen_bar_chart.py to generate bar charts
python3 gen_bar_chart.py

# output end message
echo "Bar charts generated in $output_dir/charts"