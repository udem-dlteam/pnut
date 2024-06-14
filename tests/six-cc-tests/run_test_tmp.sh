#!/bin/bash
# Temporary script to run tests for six-cc in local environment
# NOTE: getchar tests expect input from stdin, so they will hang until input if run in this script

TEST_DIR="six-cc-tests"

# Counters
passed32=0
failed32=0
passed64=0
failed64=0

# Array to keep track of failed tests
declare -a failed_tests32
declare -a failed_tests64

# Timeout duration in seconds
TIMEOUT_DURATION=20  # Corrected syntax: no spaces around "="

# Loop through all .c files in six-cc-tests
for test_file in $TEST_DIR/*.c; do

    # Get base name
    base_name=$(basename -- "$test_file" .c)

    # Test x86 version
    echo "Testing x86: $base_name"
    gcc pnut.c -Di386 -o pnut.exe
    timeout $TIMEOUT_DURATION ./pnut.exe < "$test_file" > "${TEST_DIR}/${base_name}.exe"
    exit_code=$?
    if [ $exit_code -eq 124 ]; then
        echo "Test timed out (32-bit)"
        let "failed32++"
        failed_tests32+=("$base_name (timed out)")
    else
        ./six-cc-tests/${base_name}.exe
        exit_code=$?
        echo "Exit code 32-bit: $exit_code"
        if [ $exit_code -eq 0 ]; then
            let "passed32++"
        else
            let "failed32++"
            failed_tests32+=("$base_name")
        fi
    fi
    rm "${TEST_DIR}/${base_name}.exe"

    # Test x64 version
    echo "Testing x64: $base_name"
    gcc pnut.c -Dx86_64 -o pnut.exe
    timeout $TIMEOUT_DURATION ./pnut.exe < "$test_file" > "${TEST_DIR}/${base_name}.exe"
    exit_code=$?
    if [ $exit_code -eq 124 ]; then
        echo "Test timed out (64-bit)"
        let "failed64++"
        failed_tests64+=("$base_name (timed out)")
    else
        ./six-cc-tests/${base_name}.exe
        exit_code=$?
        echo "Exit code 64-bit: $exit_code"
        if [ $exit_code -eq 0 ]; then
            let "passed64++"
        else
            let "failed64++"
            failed_tests64+=("$base_name")
        fi
    fi
    rm "${TEST_DIR}/${base_name}.exe"

done

# Test summary
echo "Summary:"
echo "32-bit tests: $passed32 passed, $failed32 failed"
echo "64-bit tests: $passed64 passed, $failed64 failed"

if [ $failed32 -gt 0 ]; then
    echo "Failed 32-bit tests:"
    printf ' - %s\n' "${failed_tests32[@]}"
fi

if [ $failed64 -gt 0 ]; then
    echo "Failed 64-bit tests:"
    printf ' - %s\n' "${failed_tests64[@]}"
fi

# Final cleanup
echo "Cleaning up all executables..."
find "${TEST_DIR}" -name '*.exe' -delete
