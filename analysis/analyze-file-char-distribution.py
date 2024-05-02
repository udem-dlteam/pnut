# Analyze the character distribution of a file
# This can be useful when we optimize certain functions to handle the most common characters first
import collections
import re
import matplotlib.pyplot as plt
import numpy as np
import sys

# Take file as argument to script
if len(sys.argv) != 2:
  print(f"Usage: python3 {sys.argv[0]} <file>")
  sys.exit(1)

file = sys.argv[1]

# Open the file
with open(file, 'r') as f:
    txt = f.read()

# Find all the alphabetic characters
letters = re.findall(".", txt)
# Count them
counts = collections.Counter(letters)

total = counts.total()

running_total = 0

pos = 0

characters = []
proportion = []

# Print the star histogram
for i in sorted(counts.keys(), key=lambda x: counts[x], reverse=True):
  running_total += counts[i]
  if i in counts:
    characters.append("Space" if i == " " else i)
    proportion.append(counts[i] / total)
    print(f"{pos:<3d}: {i} | {counts[i]:<6d} {counts[i] / total * 100 :.2f}% {running_total / total * 100 :.2f}%")
  else: print(f"{i} | ")
  pos += 1

topN = 20
# Truncate to 50 characters
characters = characters[:topN]
characters.append("Rest")
proportion = proportion[:topN]
proportion.append(1 - sum(proportion))

proportion = np.array(proportion)

# np.random.seed(19680801)

# mu = 200
# sigma = 25
# n_bins = 25
# data = np.random.normal(mu, sigma, size=100)

# fig = plt.figure(figsize=(9, 4), layout="constrained")
# axs = fig.subplots(1, 2, sharex=True, sharey=True)

# # Cumulative distributions.
# axs[0].ecdf(data, label="CDF")
# n, bins, patches = axs[0].hist(data, n_bins, density=True, histtype="step",
#                                cumulative=True, label="Cumulative histogram")
# x = np.linspace(data.min(), data.max())
# y = ((1 / (np.sqrt(2 * np.pi) * sigma)) *
#      np.exp(-0.5 * (1 / sigma * (x - mu))**2))
# y = y.cumsum()
# y /= y[-1]
# axs[0].plot(x, y, "k--", linewidth=1.5, label="Theory")

# # Complementary cumulative distributions.
# axs[1].ecdf(data, complementary=True, label="CCDF")
# axs[1].hist(data, bins=bins, density=True, histtype="step", cumulative=-1,
#             label="Reversed cumulative histogram")
# axs[1].plot(x, 1 - y, "k--", linewidth=1.5, label="Theory")

# # Label the figure.
# fig.suptitle("Pnut.c character distribution")
# for ax in axs:
#     ax.grid(True)
#     ax.legend()
#     ax.set_xlabel("Character")
#     ax.set_ylabel("Proportion")
#     ax.label_outer()

# plt.show()

# plt.rcParams["figure.figsize"] = [7.50, 3.50]
# plt.rcParams["figure.autolayout"] = True

# for ax in axs:
#     ax.grid(True)
#     ax.legend()
# plt.set_xlabel("Character")
# plt.set_ylabel("Proportion")
# plt.label_outer()
plt.bar(characters, proportion, edgecolor='black')
plt.plot(characters, proportion.cumsum(), label="CDF")
plt.axhline(proportion.cumsum()[topN - 1], linestyle="--")
# plt.hlines(proportion.cumsum()[topN - 1], characters[0], characters[topN], colors='r', linestyle="--")
plt.show()
