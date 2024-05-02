#! /bin/sh

i=0

while [ $i -lt $SIZE ]; do
  : $(( _$i = i ))
  : $(( i += 1 ))
done

# echo "i: $i, _$i $((_$((i - 1))))"

sum() {
  t0=$(gdate +%s%6N)
  j=0
  sum=0

  while [ $j -lt 100 ]; do
    i=0
    while [ $i -lt 100 ]; do
      : $(( sum += _$i ))
      : $(( _$i = _$i + 1 ))
      : $(( i += 1 ))
    done
    : $(( j += 1 ))
  done
  t1=$(gdate +%s%6N)
  echo "$SIZE:$(( t1 - t0 ))"
}

sum
sum
sum
sum
sum
sum
sum
sum
sum
sum
