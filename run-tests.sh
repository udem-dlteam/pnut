#!/bin/sh
#
# Run tests for pnut using different backends
# Usage: ./run-tests.sh <backend> [options]

trap "exit 1" INT

PNUT_SOURCE="pnut.c"

fail() { echo "❌ $1"; exit 1; }

usage() {
  echo "Usage: $0 <backend> [options]"
  echo "Backends: sh, i386_linux, x86_64_linux, x86_64_mac"
  echo "Options:"
  echo "  --shell <shell>          Shell to use for 'sh' backend (default: /bin/sh)"
  echo "  --match <pattern>        Only run tests matching pattern"
  echo "  --bootstrap              Use pnut compiled with pnut to run tests"
  echo "  --safe                   Enable safe mode (-DSAFE_MODE)"
  echo "  --fast                   Enable fast mode (sh backend: -DSH_SAVE_VARS_WITH_SET)"
  echo "  --one-pass-generator     Enable one-pass generator for native backends"
  echo "  --compile-only           Only compile, don't run tests"
  echo "  --no-create-golden-file  Don't create golden files if missing"
  exit 1
}

if [ $# -lt 1 ]; then usage; fi

# Default options
: ${PNUT_OPTIONS:=}
backend=$1; shift
shell="/bin/sh"
pattern=".*"
bootstrap=0
safe=0
fast=0
one_pass=0
compile_only=0
create_golden_file=1

# Parse arguments
while [ $# -gt 0 ]; do
  case $1 in
    --shell)                  shell="$2";               shift 2 ;;
    --match)                  pattern="$2";             shift 2 ;;
    --bootstrap)              bootstrap=1;              shift 1 ;;
    --safe)                   safe=1;                   shift 1 ;;
    --fast)                   fast=1;                   shift 1 ;;
    --one-pass-generator)     one_pass=1;               shift 1 ;;
    --compile-only)           compile_only=1;           shift 1 ;;
    --no-create-golden-file)  create_golden_file=0;     shift 1 ;;
    -*)                       fail "Unknown option: $1"         ;;
    *)                        fail "Unexpected argument: $1"    ;;
  esac
done

# --- Backend Configuration ---

case "$backend" in
  sh)
    ext="sh"
    pnut_target_flag="-Dtarget_sh"
    executor="$shell"
    test_folders="tests/_all tests/_sh tests/_bug"
    [ "$fast" -eq 1 ] && pnut_target_flag="$pnut_target_flag -DSH_SAVE_VARS_WITH_SET"
    ;;
  i386_linux|x86_64_linux|x86_64_mac)
    ext="exe"
    pnut_target_flag="-Dtarget_$backend"
    executor=""
    test_folders="tests/_all tests/_exe tests/_bug"
    [ "$one_pass" -eq 1 ] && pnut_target_flag="$pnut_target_flag -DONE_PASS_GENERATOR"
    ;;
  *)
    fail "Unknown backend: $backend"
    ;;
esac

PNUT_EXE_OPTIONS="$PNUT_OPTIONS $pnut_target_flag"
[ "$safe" -eq 1 ] && PNUT_EXE_OPTIONS="$PNUT_EXE_OPTIONS -DSAFE_MODE"

# --- Helper Functions ---

# Get a short hash of options to cache pnut binaries
executable_id() {
  if [ -z "$1" ]; then
    printf "base\n"
  else
    printf "%s" "$1" | md5sum | cut -c 1-16
  fi
}

# Metadata extraction from C files
get_test_metadata() {
  # Usage: get_test_metadata <file> <key> [separator]
  grep "// $2:" "$1" | sed "s/^\/\/ $2: //" | tr '
' "${3:- }" | sed "s/${3:- }$//"
}

test_comp_options()      { get_test_metadata "$1" "comp_opt"; }
test_pnut_comp_options() { get_test_metadata "$1" "comp_pnut_opt"; }
test_args()              { get_test_metadata "$1" "args"; }
test_timeout()           { get_test_metadata "$1" "timeout"; }
test_expect_failure()    { grep -q "// expect_failure$" "$1"; }
test_expect_comp_failure() { grep -q "// expect_comp_failure" "$1"; }
test_expect_failure_for_shells() { get_test_metadata "$1" "expect_failure_for"; }

shell_version() {
  case "$1" in
    bash) bash -c 'echo $BASH_VERSION' ;;
    ksh)  ksh  -c 'echo $KSH_VERSION'  ;;
    mksh) mksh -c 'echo $KSH_VERSION'  ;;
    yash) yash -c 'echo $YASH_VERSION' ;;
    zsh)  zsh -c  'echo $ZSH_VERSION'  ;;
    dash) dash -c 'echo unknown-possibly-0.5.12' ;;
    *)    echo "unknown" ;;
  esac
}

test_failure_is_expected() {
  file="$1"
  if test_expect_failure "$file"; then
    reason="Expected to fail"
    return 0
  fi

  failing_shells=$(test_expect_failure_for_shells "$file")
  for failing_shell in $failing_shells; do
    failing_shell_name=$(echo "$failing_shell" | sed 's/-.*//')
    failing_shell_version=$(echo "$failing_shell" | sed 's/.*-//')
    # First match on the shell name, then on the version if any
    if [ "$failing_shell_name" = "$shell" ]; then
      if [ -z "$failing_shell_version" ] || [ "$failing_shell_version" = "$failing_shell" ]; then
        reason="Expected to fail on $failing_shell_name"
        return 0 # No version specified, match!
      elif shell_version "$shell" | grep -q -E "$failing_shell_version"; then
        reason="Expected to fail on $failing_shell_name"
        return 0 # version didn't match!
      fi
    fi
  done
  return 1 # No match
}

# --- Core Logic ---

# Compile pnut, either using gcc or with pnut itself. Set the pnut_comp variable
# to the compiled pnut executable. The compiled pnut executable is cached in the
# tests folder to speed up subsequent runs with the same options.
compile_pnut() { # extra pnut compilation options: $1, expect_failed_compilation?: $2
  extra_opts="$1"
  expect_failed_compilation="${2:-0}"

  if [ "$safe" -eq 0 ] && [ "$expect_failed_compilation" -eq 1 ]; then
    extra_opts="$extra_opts -DSAFE_MODE"
  fi

  # Disable ONE_PASS_GENERATOR if we expect failure to avoid interference with error messages
  if [ "$expect_failed_compilation" -eq 1 ]; then
    extra_opts="$extra_opts -UONE_PASS_GENERATOR"
  fi

  extra_opts_id=$(executable_id "$extra_opts")
  extra_opts_suffix="-$extra_opts_id"

  pnut_exe="./tests/pnut-by-gcc${extra_opts_suffix}.exe"
  pnut_exe_backend="./tests/pnut${extra_opts_suffix}.$ext"

  if [ ! -f "$pnut_exe" ]; then
    gcc "$PNUT_SOURCE" $PNUT_EXE_OPTIONS $extra_opts -o "$pnut_exe" \
      || fail "Error: gcc failed to compile $PNUT_SOURCE"
  fi

  if [ "$bootstrap" -eq 1 ]; then
    if [ ! -f "$pnut_exe_backend" ]; then
      # Run pnut-by-gcc to produce pnut-in-backend-language
      $pnut_exe $PNUT_EXE_OPTIONS $extra_opts "$PNUT_SOURCE" > "$pnut_exe_backend" \
        || fail "Error: Failed to compile $PNUT_SOURCE with $pnut_exe (bootstrap)"
      chmod +x "$pnut_exe_backend"
    fi
    pnut_comp="$pnut_exe_backend"
  else
    pnut_comp="$pnut_exe"
  fi
}

execute_test() {
  exe_path="$1"
  timeout_val="${2:-30}"
  test_args_val="$3"

  if [ -n "$executor" ]; then
    timeout $timeout_val $executor "./$exe_path" $test_args_val
  else
    timeout ${2:-5} "./$exe_path" $test_args_val
  fi
}

compile_test() { # c file: $1, $2: output, expect_failed_compilation?: $3
  compile_pnut "$(test_pnut_comp_options "$1")" "$3"

  # Timeout to prevent infinite loops in pnut
  if [ $bootstrap -eq 1 ] && [ -n "$executor" ]; then
    timeout 15 $executor "$pnut_comp" "$1" $(test_comp_options "$1") > $2
    res=$?
  else
    timeout 5 "$pnut_comp" "$1" $(test_comp_options "$1") > $2
    res=$?
  fi
  chmod +x "$2"
  return $res
}

run_test() { # file_to_test: $1
  file="$1"                           # Path to the test C file
  filename=$(basename "$file" .c)     # Get the filename without extension
  dir=$(dirname "$file")              # Get the directory of the test file
  golden_file="$dir/$filename.golden" # Path of the expected output
  test_bin="$dir/$filename.$ext"      # Path of the compiled test binary
  gcc_bin="$dir/$filename-gcc"        # Path of the gcc compiled binary
  gcc_err="$dir/$filename-gcc.err"    # Path of gcc error output

  expect_failed_comp=0                # Indicates if compilation is expected to fail
  expect_failed_test=0                # Indicates if test is expected to fail

  if test_expect_comp_failure "$file"; then expect_failed_comp=1; fi
  if test_failure_is_expected "$file"; then expect_failed_test=1; fi

  # Print file name early so we have some context before test finishes
  printf "$file: "

  # Golden file generation
  if [ ! -f "$golden_file" ]; then
    compile_test "$file" "$test_bin" "$expect_failed_comp"
    comp_exit=$?

    if [ "$comp_exit" -eq 0 ] && [ "$expect_failed_comp" -eq 0 ]; then
      # Generate golden file using GCC output as reference
      gcc "$file" $(test_comp_options "$file") -o "$gcc_bin" 2> "$gcc_err"
      if [ $? -ne 0 ]; then
        echo "❌ Failed to compile with GCC:"
        cat "$gcc_err"
        return 1
      fi
      execute_test "$test_bin" "$(test_timeout "$file")" "$(test_args "$file")" > "$dir/$filename.output"
      "$gcc_bin" $(test_args "$file") > "$dir/$filename-gcc.output"

      if diff "$dir/$filename-gcc.output" "$dir/$filename.output" > /dev/null; then
        echo "🟡 Golden file generated"
        [ "$create_golden_file" -eq 1 ] && cp "$dir/$filename.output" "$golden_file"
      else
        echo "❌ Output mismatch with GCC during golden file generation"
      fi
    elif [ "$expect_failed_comp" -eq 1 ]; then
      if [ "$comp_exit" -eq 0 ]; then
        echo "❌ Compilation succeeded when it should have failed"
      else
        echo "🟡 Golden file generated (compilation error)"
        # Save the error message which is the last line
        tail -n 1 "$test_bin" > "$golden_file"
      fi
    else
      echo "❌ Failed to compile with pnut. See $test_bin and $gcc_err for details."
    fi
    return 1
  fi

  # Normal test run
  compile_test "$file" "$test_bin" "$expect_failed_comp"
  comp_exit=$?

  if [ "$comp_exit" -eq 0 ] && [ "$expect_failed_comp" -eq 0 ]; then
    # If compilation was successful as expected, run the test and compare output to golden file
    if [ "$compile_only" -eq 1 ]; then
      echo "✅ Compiled"
      return 0
    fi

    execute_test "$test_bin" "$(test_timeout "$file")" "$(test_args "$file")" > "$dir/$filename.output" 2> "$dir/$filename.err"
    exec_exit=$?

    if [ "$exec_exit" -eq 0 ]; then
      # If the executable ran successfully as expected
      if diff "$dir/$filename.output" "$golden_file" > /dev/null; then
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
        echo "❌ Test failed (output mismatch)"
        diff -u "$golden_file" "$dir/$filename.output" | head -n 20
        return 1
      fi
    elif test_failure_is_expected "$file"; then
      echo "⚠️  Test disabled ($reason)"
      return 0
    elif [ "$exec_exit" -eq 124 ]; then # Timeout exit code
      echo "❌ Test timed out after $(test_timeout "$file") seconds"
      return 1
    else
      echo "❌ Execution failed: $(cat "$dir/$filename.err")"
      return 1
    fi
  elif [ "$expect_failed_comp" -eq 1 ]; then
    # Compilation failed as expected, check if the error message matches the golden file
    if [ "$comp_exit" -eq 0 ]; then
      echo "❌ Compilation succeeded when it should have failed"
      return 1
    else
      if tail -n 1 "$test_bin" | diff - "$golden_file" > /dev/null; then
        echo "✅ Test passed (compilation failed as expected)"
        return 0
      else
        echo "❌ Compilation failed for a different reason than expected"
        tail -n 1 "$test_bin" | diff -u "$golden_file" - | head -n 20
        return 1
      fi
    fi
  else
    echo "❌ Compilation failed:"
    cat "$test_bin"
    return 1
  fi
}

# --- Main ---

passed_tests=""
failed_tests=""

# Initial compilation check, to fail fast if pnut doesn't compile
compile_pnut ""

# Clean up previous test binaries and scripts
find tests \( -name "*.exe" -o -name "*.sh" \) -exec rm {} + 2>/dev/null
# Also clean up any files without an extension which are likely test binaries
find tests -type f ! -name "*.*" -exec rm {} + 2>/dev/null

echo "Running tests for backend: $backend"

for folder in $test_folders; do
  [ ! -d "$folder" ] && continue
  for file in $(find "$folder" -type f -name "*.c" | sort | grep -E "$pattern"); do
    if run_test "$file"; then
      passed_tests="$passed_tests $file"
    else
      failed_tests="$failed_tests $file"
    fi
  done
done

num_passed=$(echo "$passed_tests" | wc -w)
num_failed=$(echo "$failed_tests" | wc -w)

echo ""
echo "Summary:"
echo "===================="
echo "Passed: $num_passed"
echo "Failed: $num_failed"

if [ "$num_failed" -gt 0 ]; then
  echo "Failed tests:"
  for file in $failed_tests; do
    echo " - $file"
  done
  exit "$num_failed"
fi
