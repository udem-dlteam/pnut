#!/bin/bash

# Define the top-level benchmark directory
BENCHMARK_DIR="benchmarks"

# Clean up function for benchmark results in each benchmark directory
cleanup_results() {
    # Find all benchmark-results directories in the benchmarks directory and its subdirectories
    find "$BENCHMARK_DIR" -name "benchmark-results" | while read -r RESULTS_DIR; do
        echo "Cleaning up $RESULTS_DIR"
        rm -rf "$RESULTS_DIR"
    done
    # Find all compiled directories in the benchmarks directory and its subdirectories
    find "$BENCHMARK_DIR" -name "compiled" | while read -r COMP_DIR; do
        echo "Cleaning up $COMP_DIR"
        rm -rf "$COMP_DIR"
    done
}

# Determine if cleanup is needed
if [ "$1" == "cleanup" ]; then
    echo "Cleaning up benchmark results"
    cleanup_results
    exit 0
fi


echo "Benchmarking all scripts in $BENCHMARK_DIR"

# Find all run.sh files in the benchmarks directory and its subdirectories
find "$BENCHMARK_DIR" -name "run.sh" | while read -r RUN_SCRIPT; do
    echo "Executing $RUN_SCRIPT"

    # Get the directory containing the run.sh file
    SCRIPT_DIR=$(dirname "$RUN_SCRIPT")

    # Define the results directory within the script directory
    RESULTS_DIR="$SCRIPT_DIR/benchmark-results"

    # Create the benchmark-results directory if it doesn't exist
    mkdir -p "$RESULTS_DIR"

    # Define the output file path
    OUTPUT_FILE="$RESULTS_DIR/output.txt"

    # Execute the run.sh script and save the output to the output file
    bash "$RUN_SCRIPT" > "$OUTPUT_FILE" 2>&1

    echo "Executed $RUN_SCRIPT, output saved to $OUTPUT_FILE"
done

echo "Benchmarking complete"
