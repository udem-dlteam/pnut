#!/bin/bash

# List of test files and their corresponding expected output files
declare -a tests=(
    "global-simple.c:global-simple.txt"
    "global-simple-init.c:global-simple-init.txt"
    "local-global-complex.c:local-global-complex.txt"
    "local-global-init-interaction-simple.c:local-global-init-interaction-simple.txt"
    "local-global-interaction-simple.c:local-global-interaction-simple.txt"
    "local-simple.c:local-simple.txt"
    "local-simple-init.c:local-simple-init.txt"
    "shadowing-globals.c:shadowing-globals.txt"
)

# Function to compile and run a test
run_test() {
    test_file=$1
    expected_output_file=$2
    test_executable="tests/variable-declaration-tests/${test_file%.c}.exe"
    actual_output_file="tests/variable-declaration-tests/${test_file%.c}.out"

    # Compile the C file with the specified command
    gcc pnut.c -Di386 -o pnut.exe && ./pnut.exe < "tests/variable-declaration-tests/$test_file" > "$test_executable"
    if [ $? -ne 0 ]; then
        echo "Failed to compile $test_file"
        return 1
    fi

    # Run the executable and capture its output
    ./"$test_executable" > "$actual_output_file"
    if [ $? -ne 0 ]; then
        echo "Failed to run $test_executable"
        return 1
    fi

    # Compare the actual output to the expected output
    if diff -w "$actual_output_file" "tests/variable-declaration-tests/$expected_output_file" > /dev/null; then
        echo "Test $test_file passed."
    else
        echo "Test $test_file failed."
        echo "Expected output:"
        cat "tests/variable-declaration-tests/$expected_output_file"
        echo "Actual output:"
        cat "$actual_output_file"
        return 1
    fi

    return 0
}

# Run all tests
all_passed=1
for test in "${tests[@]}"; do
    IFS=":" read -r test_file expected_output_file <<< "$test"
    run_test "$test_file" "$expected_output_file"
    if [ $? -ne 0 ]; then
        all_passed=0
    fi
done

# Cleanup generated files
cleanup() {
    rm -f pnut.exe
    for test in "${tests[@]}"; do
        IFS=":" read -r test_file expected_output_file <<< "$test"
        test_executable="tests/variable-declaration-tests/${test_file%.c}.exe"
        actual_output_file="tests/variable-declaration-tests/${test_file%.c}.out"
        rm -f "$test_executable" "$actual_output_file"
    done
}

cleanup

if [ $all_passed -eq 1 ]; then
    echo "All tests passed."
else
    echo "Some tests failed."
fi
