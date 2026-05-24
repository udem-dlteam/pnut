#!/bin/bash

# Define the top-level benchmark directory

# Create the general-benchmark-results directory if it doesn't exist
mkdir -p general-benchmark-results

# Find the output.txt files in the benchmark-results subdirectories
find benchmarks -name "output.txt" | while read -r OUTPUT_FILE; do
    # Get the directory containing the output.txt file
    RESULTS_DIR=$(dirname "$OUTPUT_FILE")

    # Get the benchmark name from the directory path
    BENCHMARK_NAME=$(basename "$(dirname "$RESULTS_DIR")")

    # Copy the output.txt file to the general-benchmark-results directory with the benchmark name as the prefix
    cp "$OUTPUT_FILE" "general-benchmark-results/${BENCHMARK_NAME}_output.txt"
done

# End message
echo "Benchmark results copied to general-benchmark-results"