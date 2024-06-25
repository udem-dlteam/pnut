#!/bin/bash

shell=$1
output_file="shell_version_results.tex"

# Initialize the LaTeX document if it does not exist
if [ ! -f "$output_file" ]; then
  echo "\\documentclass{article}" > $output_file
  echo "\\usepackage{geometry}" >> $output_file
  echo "\\geometry{a4paper, margin=1in}" >> $output_file
  echo "\\begin{document}" >> $output_file
  echo "\\begin{table}[h!]" >> $output_file
  echo "\\centering" >> $output_file
  echo "\\begin{tabular}{|c|c|}" >> $output_file
  echo "\\hline" >> $output_file
  echo "Shell & Version \\\\" >> $output_file
  echo "\\hline" >> $output_file
fi

# Function to escape LaTeX special characters
escape_latex() {
  echo "$1" | sed -e 's/_/\\_/g' -e 's/%/\\%/g' -e 's/&/\\&/g' -e 's/#/\\#/g' -e 's/\$/\\\$/g' -e 's/{/\\{/g' -e 's/}/\\}/g' -e 's/\~/\\~/g' -e 's/\^/\\^/g'
}

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
  version="$shell not found"
else
  version=$(get_shell_version)
  version=$(escape_latex "$version")
fi

# Append results to the LaTeX table
echo "$shell & $version \\\\" >> $output_file
echo "\\hline" >> $output_file

# Finalize the LaTeX table and document if this is the last shell
if [ "$shell" == "yash" ]; then
  echo "\\end{tabular}" >> $output_file
  echo "\\caption{Shell Versions}" >> $output_file
  echo "\\label{table:shell_versions}" >> $output_file
  echo "\\end{table}" >> $output_file
  echo "\\end{document}" >> $output_file
fi
