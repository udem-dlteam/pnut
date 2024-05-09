# Plot benchmark times for dynamic variables benchmarks.
# Input format is:
#
# With {shell1}
# {size}:{time}
# ...
# {size}:{time}
# With {shell2}
# {size}:{time}
# ...
# {size}:{time}
# ...
import collections
import re
import statistics

import matplotlib.pyplot as plt
import numpy as np

# Open the file
with open("../dynamic-variables-result.txt", 'r') as f:
  txt = f.read()

shell="default"
data={}
shells = ["bash", "dash", "ksh", "mksh", "yash", "zsh", "sh"]
for shell in shells:
  data[shell] = {}

for line in txt.split("\n"):
  # If the line starts with "With ":
  if line == "":
    continue
  if line.startswith("With "):
    # Print the line
    shell = line.split(" ")[1][0:-1]
    shells.append(shell)
    # print(shell)
  else:
    parts = line.split(":")
    if len(parts) != 2:
      print("Line is not in the expected format: ", line)
      continue

    size = int(line.split(":")[0])
    time = int(line.split(":")[1])
    if size in data[shell]:
      data[shell][size].append(time / size) # Normalize
    else:
      data[shell][size] = [time]


# print(data)

for shell in data:
  for size in data[shell]:
    data[shell][size] = statistics.median_high(data[shell][size])

# print(data)

# Plot each shell
fig, ax = plt.subplots()
ax.set_title("Dynamic Variables")
ax.set_xlabel("Environment size")
ax.set_ylabel("Time (us)")
ax.set_xscale("log")
for shell in data:
  sizes = list(data[shell].keys())
  times = list(data[shell].values())
  print(sizes)
  ax.plot(sizes, times, label=shell)
  ax.legend()

plt.show()
