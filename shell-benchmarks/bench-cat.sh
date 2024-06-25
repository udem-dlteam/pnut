bench_with() { # $1: shell, $3: length
  printf "\n$1 - $2 - default"
  time $1 "shell-benchmarks/cat-default.sh" "shell-benchmarks/long-line-$2.txt" > /dev/null
}

fast_bench_with() { # $1: shell, $2: length
  printf "\n$1 - $2 - fast"
  time $1 "shell-benchmarks/cat.sh" "shell-benchmarks/long-line-$2.txt" > /dev/null
}

lengths="1000 5000 10000"
shells="dash bash zsh"

for len in $lengths; do
  for s in $shells; do
    bench_with $s $len
    fast_bench_with $s $len
  done
done

# bench_with ksh  4000
# bench_with dash 4000
# bench_with bash 4000
# bench_with zsh  4000

# fast_bench_with ksh  4000
# fast_bench_with dash 4000
# fast_bench_with bash 4000
# fast_bench_with zsh  4000

# bench_with zsh 1000
# bench_with zsh 5000
# bench_with zsh 10000
# bench_with zsh 50000

# fast_bench_with zsh 1000
# fast_bench_with zsh 5000
# fast_bench_with zsh 10000
# fast_bench_with zsh 50000

# bench_with ksh 1000
# bench_with ksh 5000
# bench_with ksh 10000

# bench_with dash 1000
# bench_with dash 5000
# bench_with dash 10000

# bench_with bash 1000
# bench_with bash 5000
# bench_with bash 10000

# bench_with zsh 1000
# bench_with zsh 5000
# bench_with zsh 10000

# fast_bench_with ksh 1000
# fast_bench_with ksh 5000
# fast_bench_with ksh 10000
# fast_bench_with ksh 50000

# fast_bench_with dash 1000
# fast_bench_with dash 5000
# fast_bench_with dash 10000
# fast_bench_with dash 50000

# fast_bench_with bash 1000
# fast_bench_with bash 5000
# fast_bench_with bash 10000
# fast_bench_with bash 50000

# fast_bench_with zsh 1000
# fast_bench_with zsh 5000
# fast_bench_with zsh 10000
# fast_bench_with zsh 50000
