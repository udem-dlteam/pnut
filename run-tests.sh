#!/bin/sh
#
# Run tests for pnut using different backends
# Usage: ./run-tests.sh <backend> --shell <shell> --match <pattern> --bootstrap
# The backend can be one of the following: sh, i386_linux, x86_64_linux, x86_64_mac
# The --shell flag is used to specify the shell to use for running tests with the sh backend
# The --match flag is used to run tests that match the given pattern, useful for re-running failed tests
# The --bootstrap flag compiles the tests using pnut compiled with pnut, useful for catching bootstrap errors

trap "exit 1" INT

fail() { echo "$1"; exit $2; }

if [ $# -lt 1 ]; then
  fail "Usage: $0 <backend> --shell shell -m pattern --bootstrap" 1
fi

# Parse the arguments
: ${PNUT_OPTIONS:=} # Default to empty options
backend=$1; shift
bootstrap=0
shell="$SHELL" # Use current shell as the default
pattern=".*"
while [ $# -gt 0 ]; do
  case $1 in
    --shell)         shell="$2";         shift 2;;
    --match)         pattern="$2";       shift 2;;
    --bootstrap)     bootstrap=1;        shift 1;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

# Determine the file extension based on the backend
case "$backend" in
  sh)
    ext="exe" # The extension doesn't matter for sh
    PNUT_EXE_OPTIONS="$PNUT_OPTIONS -Dsh -DRT_NO_INIT_GLOBALS"
    ;;
  i386_linux | x86_64_linux | x86_64_mac)
    ext="exe"
    PNUT_EXE_OPTIONS="$PNUT_OPTIONS -Dtarget_$backend"
    ;;
  *)
    echo "Unknown backend: $backend"
    exit 1
    ;;
esac

# Compile pnut, either using gcc or with pnut itself
# pnut_comp is set to the compiled pnut executable
compile_pnut() {
  pnut_source="pnut.c"
  pnut_exe="./tests/pnut-by-gcc.exe"
  pnut_exe_backend="./tests/pnut.$ext"

  echo "Compiling $pnut_source with $backend backend..."
  gcc "$pnut_source" $PNUT_EXE_OPTIONS -o "$pnut_exe" 2> /dev/null || fail "Error: Failed to compile $pnut_source with $backend"
  if [ "$bootstrap" -eq 1 ]; then
    "$pnut_exe" $PNUT_EXE_OPTIONS "$pnut_source" > "$pnut_exe_backend" || fail "Error: Failed to compile $pnut_source with $pnut_exe (bootstrap)"
    chmod +x "$pnut_exe_backend"
    pnut_comp="$pnut_exe_backend"
  else
    pnut_comp="$pnut_exe"
  fi
}

shell_version() {
  case "$1" in
    bash) bash -c 'echo $BASH_VERSION' ;;
    ksh)  ksh  -c 'echo $KSH_VERSION' ;;
    mksh) mksh -c 'echo $KSH_VERSION' ;;
    yash) yash --version | head -n 1 ;;
    zsh)  zsh  --version ;;
    # dash doesn't support --version or DASH_VERSION and we'd have to query the package manager
    dash) echo "dash" ;;
    *)    echo "Unknown shell: $1" ;;
  esac
}

# Some tests specify command line arguments in the source file meant to be passed to the compiler.
# This function extracts the arguments from the source file.
# To specify arguments, add a comment in the source file like this:
# // pnut_opt: arg1 arg2 arg3
test_args_comp() {
  # echo "test_args_comp file $1" >&2
  echo `sed -n -e "/\/\/ pnut_opt: /p" "$1" | sed -e "s/^\/\/ pnut_opt: //" |  tr '\n' ',' | sed -e 's/,$//'`
}

# Some tests specify command line arguments in the source file
# This function extracts the arguments from the source file
# To specify arguments, add a comment in the source file like this:
# // args: arg1 arg2 arg3
test_args() {
  echo `sed -n -e "/\/\/ args: /p" "$1" | sed -e "s/^\/\/ args: //" |  tr '\n' ',' | sed -e 's/,$//'`
}

# Some shells don't support certain features which mean some tests will fail.
# While we often can work around the bugs and non-standard behavior of certain
# shells, it can be easier to just disable the tests, especially if the test is
# not relevant to the bootstrap process.
# // expect_failure_for: bash-2*
# // expect_failure_for: yash
test_expect_falure_for_shells() {
  echo `sed -n -e "/\/\/ expect_failure_for: /p" "$1" | sed -e "s/^\/\/ expect_failure_for: //"`
}

# Some tests take a long time to run, so we set a timeout to prevent infinite
# loops However, we don't want to set a high timeout for all tests, so we have
# an option to set a specific timeout.
test_timeout() {
  echo `sed -n -e "/\/\/ timeout: /p" "$1" | sed -e "s/^\/\/ timeout: //"`
}

test_expect_failure_for_shell() { # file: $1
  failing_shells=$(test_expect_falure_for_shells "$1")
  for failing_shell in $failing_shells; do
    failing_shell_name=$(echo "$failing_shell" | sed 's/-.*//')
    failing_shell_version=$(echo "$failing_shell" | sed 's/.*-//')
    if [ "$failing_shell_name" = "$shell" ]; then # First match on the shell name, then on the version if any
      if [ -z "$failing_shell_version" ] || [ "$failing_shell_version" = "$failing_shell" ]; then
        return 0 # No version specified, match!
      elif shell_version "$shell" | grep -q -E "$failing_shell_version"; then
        return 0 # version matched!
      else
        return 1 # version didn't match!
      fi
    fi
  done
  return 1 # No match
}

execute_test() { # executable: $1, timeout: $2, args: $3
  if [ "$backend" = "sh" ]; then
    # Use a 30s timeout to prevent infinite loops
    timeout ${2:-30} $shell "./$1" $3
  else
    # Native code is much faster, it should never take more than a few seconds
    timeout ${2:-5} "./$1" $3
  fi
}

compile_test() { # c_file: $1
  # 5s timeout to prevent infinite loops in pnut
  timeout 5 "$pnut_comp" "$1" $2
}

run_test() { # file_to_test: $1
  file="$1"
  filename=$(basename "$file" .c) # Get the filename without extension
  dir=$(dirname "$file") # Get the directory of the test file

  golden_file="$dir/$filename.golden"

  # Print file name before generating golden file so we know it's getting processed
  printf "$file: "

  # Generate golden file if it doesn't exist
  if [ ! -f "$golden_file" ]; then
    compile_test "$file" "$(test_args_comp $file)" > "$dir/$filename.$ext" 2> "$dir/$filename.err"
    if [ $? -eq 0 ]; then
      chmod +x "$dir/$filename.$ext"
      execute_test "$dir/$filename.$ext" "$(test_timeout $file)" "$(test_args $file)" > "$golden_file"
      echo "🟡 Golden file generated by pnut"
    else
      echo "❌ Failed to compile with pnut"
    fi
    return 1
  fi

  # Compile the test file with pnut.exe
  compile_test "$file" "$(test_args_comp $file)" > "$dir/$filename.$ext" 2> "$dir/$filename.err"

  if [ $? -eq 0 ]; then # If compilation was successful
    chmod +x "$dir/$filename.$ext"
    execute_test "$dir/$filename.$ext" "$(test_timeout $file)" "$(test_args $file)" > "$dir/$filename.output" 2> "$dir/$filename.err"
    if [ $? -eq 0 ]; then # If the executable ran successfully
      diff_out=$(diff "$dir/$filename.output" "$dir/$filename.golden")
      if [ $? -eq 0 ]; then # If the output matches the golden file
        echo "✅ Test passed"
        return 0
      elif test_expect_failure_for_shell "$file"; then
        echo "⚠️ Test disabled for $shell"
        return 0
      else
        echo "❌ Test failed"
        echo "diff (output vs expected)"
        echo "$diff_out"
        return 1
      fi
    elif test_expect_failure_for_shell "$file"; then
      echo "⚠️ Test disabled for $shell"
      return 0
    else
      echo "❌ Failed to run: $(cat "$dir/$filename.err")"
      return 1
    fi
  else
    echo "❌ Failed to compile with pnut: $(cat "$dir/$filename.err")"
    return 1
  fi
}

run_tests_in_folder() {
  folder="$1"
  for file in $(find "$folder" -type f -name "*.c" | sort | grep -E "$pattern"); do
    if run_test "$file"; then
      passed_tests="$passed_tests\n$file"
    else
      failed_tests="$failed_tests\n$file"
    fi
  done
}

# Function to run tests
run_tests() {
  passed_tests="" # List of passed tests separated by newline
  failed_tests="" # List of failed tests separated by newline

  echo "Running tests..."

  run_tests_in_folder "tests/_all"
  if [ "$backend" = "sh" ]; then
    run_tests_in_folder "tests/_sh"
  else # Run all tests for other backends
    run_tests_in_folder "tests/_exe"
  fi

  echo "Summary:"
  echo "===================="
  echo "Passed: $(printf "$passed_tests" | wc -l)"
  echo "Failed: $(printf "$failed_tests" | wc -l)"

  if [ -n "$failed_tests" ]; then
    for file in $(printf "$failed_tests"); do
      printf " - %s\n" "$file"
    done
    # Return the number of failed tests, assuming it's less than 256
    exit $(printf "$failed_tests" | wc -l)
  else
    exit 0
  fi
}

compile_pnut
run_tests "$pattern"
