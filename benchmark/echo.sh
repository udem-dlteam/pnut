if [ $# -lt 1 ]; then
  # Assign ksh dash bash zsh to $1 $2 $3 $4
  set ksh dash bash zsh
fi

while [ $# -gt 0 ]; do
  echo "\n$1: Without quotes"
  time $1 -c 'acc=""; c="abc"; i=0 ; while [ $i -lt 10000 ]; do i=$((i+1)); echo $c; done > /dev/null'

  echo "\n$1: With quotes"
  time $1 -c 'acc=""; c="abc"; i=0 ; while [ $i -lt 10000 ]; do i=$((i+1)); echo "$c"; done > /dev/null'
  shift
done
