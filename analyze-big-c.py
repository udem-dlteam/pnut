import collections
import re

# Open the file
with open("./big.c", 'r') as f:
    txt = f.read()

# Find all the alphabetic characters
letters = re.findall(".", txt)
# Count them
counts = collections.Counter(letters)

# Print the star histogram
for i in sorted(counts.keys(), key=lambda x: counts[x], reverse=True):
    if i in counts:
        print(f"{i} | {counts[i]}")
    else: print(f"{i} | ")
