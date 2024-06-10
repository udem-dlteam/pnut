#!/bin/bash

if [ $# -lt 2 ]; then
  echo "Usage: $0 <backend> {clean|test}"
  exit 1
fi

backend=$1
action=$2
pnut_source="pnut.c"
pnut_exe="./pnut.exe"

# Check if the pnut_source is a valid file
if [ ! -f "$pnut_source" ]; then
  echo "Error: $pnut_source is not a valid file."
  exit 1
fi

# Function to clean generated files
clean() {
    #clean up the generated files in each of the directories with a .err or .golden extension
    for file in $(find tests -type f -name "*.c" | sort); do
        filename=$(basename "$file" .c) # Get the filename without extension
        dir=$(dirname "$file") # Get the directory of the test file
        rm -f "$dir/$filename.err" "$dir/$filename.golden" "$dir/$filename.golden.sh" "$dir/$filename.golden.exe"
        rm -f $dir/$filename.$ext $dir/$filename.output $dir/$filename.golden.$ext $dir/$filename.golden.output
        rm -f $dir/out.sh
    done
}

# Compile pnut.c with the specified backend
# if action is clean echo "Cleaning up..."
# if action is test echo "Running tests..."
if [ "$action" = "clean" ]; then
  echo "Cleaning up..."
  clean
elif [ "$action" = "test" ]; then
  echo "Compiling $pnut_source with backend $backend..."
  gcc "$pnut_source" "$backend" -o "$pnut_exe"
  if [ $? -ne 0 ]; then
    echo "Error: Failed to compile $pnut_source with $backend"
    exit 1
  fi
fi


# Determine the file extension based on the backend
case "$backend" in
  -Dsh)
    ext="sh"
    ;;
  -Di386 | -Dx86_64)
    ext="exe"
    ;;
  *)
    echo "Unknown backend: $backend"
    exit 1
    ;;
esac

# Function to run tests
test() {
    echo "Running tests..."

    for file in $(find tests -type f -name "*.c" | sort); do
        filename=$(basename "$file" .c) # Get the filename without extension
        dir=$(dirname "$file") # Get the directory of the test file

#        # Generate golden file using gcc
#        gcc "$file" -o "$dir/$filename.gcc.exe"
#        if [ $? -eq 0 ]; then
#            "./$dir/$filename.gcc.exe" > "$dir/$filename.golden"
#            echo "$filename: ✅ Golden file generated by gcc"
#            rm -f "$dir/$filename.gcc.exe"
#        else
#            echo "$filename: ❌ Failed to compile with gcc"
#            continue
#        fi

        golden_file="$dir/$filename.golden"

        # Generate golden file using pnut only if it doesn't exist
        if [ ! -f "$golden_file" ]; then
            echo "Generating golden file for $filename using pnut..."
            "$pnut_exe" < "$file" > "$dir/$filename.golden.$ext" 2> "$dir/$filename.err"
            if [ $? -ne 1 ]; then
                chmod +x "$dir/$filename.golden.$ext"
                "./$dir/$filename.golden.$ext" > "$golden_file"
                echo "$filename: ✅ Golden file generated by pnut"
            else
                echo "$filename: ❌ Failed to compile with pnut"
                continue
            fi
        else
            echo "$filename: 🟡 Golden file already exists"
        fi

        # Compile the test file with pnut.exe
        "$pnut_exe" < "$file" > "$dir/$filename.$ext" 2> "$dir/$filename.err"
        if [ $? -eq 0 ]; then # If compilation was successful
            chmod +x "$dir/$filename.$ext"
            "./$dir/$filename.$ext" > "$dir/$filename.output" 2> "$dir/$filename.err"
            if [ $? -ne 1 ]; then # If the executable ran successfully
                diff_out=$(diff "$dir/$filename.output" "$dir/$filename.golden")
                if [ $? -eq 0 ]; then # If the output matches the golden file
                    echo "$filename: ✅ Test passed"
                    rm -f "$dir/$filename.$ext" # Clean up the executable if the test passed
                else
                    echo "$filename: ❌ Test failed"
                    echo "diff (output vs expected)"
                    echo "$diff_out"
                fi
            else
                echo "$filename: ❌ Failed to run: $(cat $dir/$filename.err)"
            fi
            rm -f "$dir/$filename.output"
        else
            echo "$filename: ❌ Failed to compile with pnut: $(cat $dir/$filename.err)"
        fi
        rm -f "$dir/$filename.err"
    done
}

# Determine action to perform
case "$action" in
  clean)
    clean
    ;;
  test)
    test
    ;;
  *)
    echo "Usage: $0 <backend> {clean|test}"
    exit 1
    ;;
esac