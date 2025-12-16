#!/bin/sh
#
# Run tests for pnut using different backends
# Usage: ./run-tests.sh <backend> --shell <shell> --match <pattern> --bootstrap
# The backend can be one of the following: sh, i386_linux, x86_64_linux, x86_64_mac
# The --shell flag is used to specify the shell to use for running tests with the sh backend
# The --match flag is used to run tests that match the given pattern, useful for re-running failed tests
# The --bootstrap flag compiles the tests using pnut compiled with pnut, useful for catching bootstrap errors

trap "exit 1" INT

fail() { echo "❌ $1"; exit 1; }

if [ $# -lt 1 ]; then
  fail "Usage: $0 <backend> --shell shell -m pattern --bootstrap" 1
fi

# Parse the arguments
: ${PNUT_OPTIONS:=} # Default to empty options
backend=$1; shift
bootstrap=0
safe=0
fast=0
one_pass=0
compile_only=0
create_golden_file=1 # Create golden files by default
shell="$SHELL" # Use current shell as the default
pattern=".*"
while [ $# -gt 0 ]; do
  case $1 in
    --shell)                  shell="$2";         shift 2;;
    --match)                  pattern="$2";       shift 2;;
    --bootstrap)              bootstrap=1;        shift 1;;
    --safe)                   safe=1;             shift 1;;
    --fast)                   fast=1;             shift 1;;
    --one-pass-generator)     one_pass=1;         shift 1;;
    --compile-only)           compile_only=1;     shift 1;;
    --no-create-golden-file)  create_golden_file=0;     shift 1;;
    *) echo "Unknown option: $1"; exit 1;;
  esac
done

# Determine the file extension based on the backend
case "$backend" in
  sh)
    ext="exe" # The extension doesn't matter for sh
    PNUT_EXE_OPTIONS="$PNUT_OPTIONS -Dtarget_sh"
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

if [ "$safe" -eq 1 ]; then
  # Enable safe mode which checks get_child accesses
  PNUT_EXE_OPTIONS="$PNUT_EXE_OPTIONS -DSAFE_MODE"
fi

if [ "$fast" -eq 1 ]; then
  if [ "$backend" != "sh" ]; then
    fail "Fast mode is not supported for the sh backend"
  fi
  # Enable fast mode which uses a faster implementation for local variables
  PNUT_EXE_OPTIONS="$PNUT_EXE_OPTIONS -DSH_SAVE_VARS_WITH_SET"
fi

if [ "$one_pass" -eq 1 ]; then
  # Enable the one pass mode for pnut-exe's code generator
  PNUT_EXE_OPTIONS="$PNUT_EXE_OPTIONS -DONE_PASS_GENERATOR"
fi

# Compile pnut, either using gcc or with pnut itself. Set pnut_comp to the compiled pnut executable
# The compiled pnut executable is cached in the tests folder to speed up the process
compile_pnut() { # extra pnut compilation options: $1, expect_failed_compilation?: $2
  pnut_source="pnut.c"
  extra_opts="$1"
  expect_failed_compilation="${2:-0}"
  if [ "$safe" -eq 0 ] && [ "$expect_failed_compilation" -eq 1 ]; then
    extra_opts="$extra_opts -DSAFE_MODE"
  fi

  # Disable ONE_PASS_GENERATOR since it interferes with the error message
  if [ "$expect_failed_compilation" -eq 1 ]; then
    # Add a flag to force compilation failure
    extra_opts="$extra_opts -UONE_PASS_GENERATOR"
  fi

  if [ -z "$extra_opts" ]; then
    extra_opts_id="base"
  else
    extra_opts_id=$(printf "%s" "$extra_opts" | md5sum | cut -c 1-16) # 16 characters should be enough
  fi
  extra_opts_suffix=${extra_opts_id:+"-"}$extra_opts_id               # Add a dash if there are extra options
  pnut_exe="./tests/pnut-by-gcc${extra_opts_suffix}.exe"
  pnut_exe_backend="./tests/pnut$extra_opts_suffix.$ext"

  if [ ! -f "$pnut_exe" ]; then
    gcc "$pnut_source" $PNUT_EXE_OPTIONS $extra_opts -o "$pnut_exe" \
      || fail "Error: gcc failed to compile $pnut_source with $backend"
  fi

  if [ "$bootstrap" -eq 1 ]; then
    if [ ! -f "$pnut_exe_backend" ]; then
      $pnut_exe $PNUT_EXE_OPTIONS $extra_opts "$pnut_source" > "$pnut_exe_backend" \
        || fail "Error: Failed to compile $pnut_source with $pnut_exe (bootstrap)"
      chmod +x "$pnut_exe_backend"
    fi
    pnut_comp="$pnut_exe_backend"
  else
    pnut_comp="$pnut_exe"
  fi
}

shell_version() {
  case "$1" in
    bash) bash -c 'echo $BASH_VERSION' ;;
    ksh)  ksh  -c 'echo $KSH_VERSION'  ;;
    mksh) mksh -c 'echo $KSH_VERSION'  ;;
    yash) yash -c 'echo $YASH_VERSION' ;;
    zsh)  zsh -c  'echo $ZSH_VERSION'  ;;
    # dash doesn't support --version or DASH_VERSION and we'd have to query the package manager
    dash) dash -c 'echo unknown-possibly-0.5.12' ;;
    *)    echo "Unknown shell: $1" ;;
  esac
}

# Some tests require specific command line options to be compiled properly.
# This function extracts those options from the source file.
# // comp_opt: arg1 arg2 arg3
test_comp_options() {
  echo `sed -n -e "/\/\/ comp_opt: /p" "$1" | sed -e "s/^\/\/ comp_opt: //" |  tr '\n' ',' | sed -e 's/,$//'`
}

# Some tests must be compiled by pnut compiled with specific options.
# This function extracts those options from the source file.
# // comp_pnut_opt: arg1 arg2 arg3
test_pnut_comp_options() {
  echo `sed -n -e "/\/\/ comp_pnut_opt:/p" "$1" | sed -e "s/^\/\/ comp_pnut_opt://" |  tr '\n' ',' | sed -e 's/,$//'`
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
test_expect_failure_for_shells() {
  echo `sed -n -e "/\/\/ expect_failure_for: /p" "$1" | sed -e "s/^\/\/ expect_failure_for: //"`
}

# Pnut has a bug that causes it to miscompile when a certain feature is used.
# // expect_failure
test_expect_failure() {
  if grep -q "// expect_failure$" "$1"; then
    return 0
  else
    return 1
  fi
}

# Some tests are expected to fail with a compilation error
# // expect_comp_failure
test_expect_comp_failure() {
  if grep -q "// expect_comp_failure" "$1"; then
    return 0
  else
    return 1
  fi
}

# Some tests take a long time to run, so we set a timeout to prevent infinite
# loops However, we don't want to set a high timeout for all tests, so we have
# an option to set a specific timeout.
test_timeout() {
  echo `sed -n -e "/\/\/ timeout: /p" "$1" | sed -e "s/^\/\/ timeout: //"`
}

test_failure_is_expected() { # file: $1
  if test_expect_failure "$1"; then
    reason="Expected to fail"
    return 0
  fi

  failing_shells=$(test_expect_failure_for_shells "$1")
  for failing_shell in $failing_shells; do
    failing_shell_name=$(echo "$failing_shell" | sed 's/-.*//')
    failing_shell_version=$(echo "$failing_shell" | sed 's/.*-//')
    if [ "$failing_shell_name" = "$shell" ]; then # First match on the shell name, then on the version if any
      if [ -z "$failing_shell_version" ] || [ "$failing_shell_version" = "$failing_shell" ]; then
        reason="Expected to fail on $failing_shell_name"
        return 0 # No version specified, match!
      elif shell_version "$shell" | grep -q -E "$failing_shell_version"; then
        reason="Expected to fail on $failing_shell_name"
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

compile_test() { # c file: $1, expect_failed_compilation?: $2
  # 15s timeout to prevent infinite loops in pnut
  compile_pnut "$(test_pnut_comp_options $1)" "$2"
  if [ $bootstrap -eq 1 ]; then
    if [ "$backend" = "sh" ]; then
      timeout 15 $shell $pnut_comp "$1" $(test_comp_options $1)
    else # Use the compiled pnut executable
      timeout 15 $pnut_comp "$1" $(test_comp_options $1)
    fi
  else
    timeout 5 $pnut_comp "$1" $(test_comp_options $1)
  fi
}

run_test() { # file_to_test: $1
  file="$1"
  filename=$(basename "$file" .c)     # Get the filename without extension
  dir=$(dirname "$file")              # Get the directory of the test file
  golden_file="$dir/$filename.golden" # Path of the expected output
  expect_failed_comp=0                # Flag to indicate if compilation is expected to fail
  expect_failed_test=0                # Flag to indicate if execution is expected to fail, or not match the golden file

  if test_expect_comp_failure "$file"; then expect_failed_comp=1; fi
  if test_failure_is_expected "$file"; then expect_failed_test=1; fi

  # Print file name before generating golden file so we know it's getting processed
  printf "$file: "

  # Generate golden file if it doesn't exist
  if [ ! -f "$golden_file" ]; then
    compile_test "$file" "$expect_failed_comp" > "$dir/$filename.$ext" && \
      gcc "$file" $(test_comp_options $file) -o "$dir/$filename-gcc.$ext" 2> "$dir/$filename-by-gcc.err"
    compile_test_exit_code="$?"
    if [ "$compile_test_exit_code" -eq 0 ] && [ "$expect_failed_comp" -eq 0 ]; then
      chmod +x "$dir/$filename.$ext"
      execute_test "$dir/$filename.$ext" "$(test_timeout $file)" "$(test_args $file)" > "$dir/$filename.output"
      $dir/$filename-gcc.$ext $(test_args $file) > "$dir/$filename-gcc.output"
      if diff "$dir/$filename-gcc.output" "$dir/$filename.output"; then
        echo "🟡 Golden file generated by pnut"
        if [ "$create_golden_file" -eq 1 ]; then
          cp "$dir/$filename.output" "$golden_file"
        fi
      else
        echo "❌ Program compiled by gcc and pnut produced different outputs"
      fi

    elif [ "$expect_failed_comp" -eq 1 ]; then

      if [ "$compile_test_exit_code" -eq 0 ]; then
        echo "❌ Compilation succeeded when it should have failed"
        return 1
      else
        echo "🟡 Golden file generated by pnut"
        tail -n 1 "$dir/$filename.$ext" > "$golden_file" # Save the error message which is the last line
      fi

    else
      echo "❌ Failed to compile with pnut. See $dir/$filename.$ext and $dir/$filename-by-gcc.err"
    fi
    return 1
  fi

  # Compile the test file with pnut.exe
  compile_test "$file" "$expect_failed_comp" > "$dir/$filename.$ext"
  compile_test_exit_code="$?"

  if [ "$compile_test_exit_code" -eq 0 ] && [ "$expect_failed_comp" -eq 0 ]; then # If compilation was successful and not expected to fail

    if [ "$compile_only" -eq 1 ]; then
      echo "✅ Compiled $file"
      return 0
    fi

    chmod +x "$dir/$filename.$ext"
    execute_test "$dir/$filename.$ext" "$(test_timeout $file)" "$(test_args $file)" > "$dir/$filename.output" 2> "$dir/$filename.err"
    execute_test_exit_code=$?
    if [ "$execute_test_exit_code" -eq 0 ]; then # If the executable ran successfully as expected
      diff_out=$(diff "$dir/$filename.output" "$golden_file")
      if [ $? -eq 0 ]; then # If the output matches the golden file
        if [ "$expect_failed_test" -eq 1 ]; then
          echo "❌ Test passed but expected to fail"
          return 1
        else
          echo "✅ Test passed"
          return 0
        fi
      elif [ "$expect_failed_test" -eq 1 ]; then
        echo "⚠️  Test disabled ($reason)"
        return 0
      else
        echo "❌ Test failed"
        echo "diff (output vs expected)"
        echo "$diff_out"
        return 1
      fi
    elif test_failure_is_expected "$file"; then # Test exited with an error as expected
      echo "⚠️  Test disabled ($reason)"
      return 0
    else                                        # Test exited with an unexpected error
      echo "❌ Failed to run: $(cat "$dir/$filename.err")"
      return 1
    fi

  elif [ "$expect_failed_comp" -eq 1 ]; then # Compilation failed as expected

    if [ "$compile_test_exit_code" -eq 0 ]; then
      echo "❌ Compilation succeeded when it should have failed"
      return 1
    else
      diff_out=$(tail -n 1 "$dir/$filename.$ext" | diff - "$golden_file")
      if [ $? -eq 0 ]; then # If the error message matches the golden file
        echo "✅ Test passed (compilation failed as expected)"
        return 0
      else
        echo "❌ Compilation failed for a different reason than expected:"
        echo "diff (error vs expected)"
        echo "$diff_out"
        return 1
      fi
    fi

  else # Compilation failed when it should have succeeded

    echo "❌ Failed to compile with pnut: $(cat "$dir/$filename.$ext")"
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
  # Folder containing tests that expose bugs in pnut or shells
  run_tests_in_folder "tests/_bug"

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

compile_pnut # Precompile pnut to get an error message if it fails
find tests -name "*.exe" -exec rm {} \; # Clear cached pnut executables
run_tests "$pattern"
