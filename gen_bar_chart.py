import os
import re
import matplotlib.pyplot as plt

# Directory containing the benchmark results
output_dir = "benchmark_results/"
# Directory to save the generated charts
chart_output_dir = "benchmark_results/charts"

# Ensure the directory to save charts exists
os.makedirs(chart_output_dir, exist_ok=True)

# Function to extract benchmark result from a file
def extract_benchmark_results(file_path):
    results = []
    try:
        with open(file_path, 'r') as file:
            content = file.read()
            matches = re.findall(r"(\d+\.\d+s) for: (.+)", content)
            for match in matches:
                time_str, description = match
                time_in_seconds = float(time_str.replace('s', ''))
                # Removing the shell name if present
                normalized_description = re.sub(r'^\w+\s+', '', description)
                results.append((normalized_description, time_in_seconds))
    except Exception as e:
        print(f"Error reading file {file_path}: {e}")
    return results

# Custom titles for specific descriptions
custom_titles = {
    r"pnut-sh.sh -DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -Dsh pnut.c > pnut-sh-compiled-by-pnut-sh-sh.sh": "Execution Time with PNUT-SH Script",
    r"pnut-sh.sh -DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -Di386 pnut.c > pnut-i386-compiled-by-pnut-sh-sh.sh": "Execution Time with PNUT-i386 Script",
    r"pnut-i386-compiled-by-pnut-sh-sh.sh -DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -Di386 pnut.c > pnut-i386-compiled-by-pnut-i386-sh.exe": "Execution Time with PNUT-i386 Compiled Script",
    r"pnut-i386-compiled-by-pnut-i386-sh.exe -DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -Di386 pnut.c > pnut-i386-compiled-pnut-i386-exe.exe": "Execution Time with PNUT-i386 Compiled by PNUT-i386",
    r"-DSUPPORT_INCLUDE -DRT_NO_INIT_GLOBALS -Dsh pnut.c -o pnut-sh-compiled-by-gcc.exe": "Compilation Time with GCC",
    r"pnut-sh-compiled-by-gcc.exe -DSUPPORT_INCLUDE -DRT NO INIT GLOBALS -Dsh pnut.c > pnut-sh.sh": "Execution Time with PNUT.SH Compiled by GCC",
}

# Function to get the generic title
def get_generic_title(description):
    for pattern, title in custom_titles.items():
        if re.search(pattern, description):
            return title
    return description

# Function to generate a clean filename
def clean_filename(filename):
    return re.sub(r'[^\w\-_.]', '_', filename)

# List to store shell names and their benchmark results
shells = []
results_dict = {}

# Loop through each file in the benchmark results directory
try:
    files = os.listdir(output_dir)
    for file_name in files:
        if file_name.endswith("_results.txt"):
            shell_name = file_name.split('_')[0]
            file_path = os.path.join(output_dir, file_name)
            results = extract_benchmark_results(file_path)
            if results:
                shells.append(shell_name)
                if shell_name not in results_dict:
                    results_dict[shell_name] = []
                results_dict[shell_name].extend(results)
            else:
                print(f"No results extracted from: {file_path}")  # Log if no results extracted
except Exception as e:
    print(f"Error listing files in directory {output_dir}: {e}")


shells = sorted(list(set(shells))) # Sort the shell names

# Solarized color palette
solarized_colors = [
    '#b58900',  # yellow
    '#cb4b16',  # orange
    '#dc322f',  # red
    '#d33682',  # magenta
    '#6c71c4',  # violet
    '#268bd2',  # blue
    '#2aa198',  # cyan
    '#859900',  # green
]

solarized_base00 = '#002b36'
solarized_base0 = '#839496'
solarized_base1 = '#93a1a1'
solarized_base2 = '#eee8d5'
solarized_base3 = '#fdf6e3'

# Generate the combined bar charts
benchmark_descriptions = set([desc for shell_results in results_dict.values() for desc, _ in shell_results])

for i, description in enumerate(benchmark_descriptions):
    generic_title = get_generic_title(description)  # Use generic title if found
    # Special case workaround for pnut-sh-compiled-by-gcc.exe
    if description.startswith("pnut-sh-compiled-by-gcc.exe") or description.find("-Dsh pnut.c > pnut-sh.sh") != -1:
        generic_title = "Execution Time with PNUT.SH Compiled by GCC"
    times = []
    for shell in shells:
        found = False
        for desc, time in results_dict.get(shell, []):
            if desc == description:
                times.append(time)
                found = True
                break
        if not found:
            times.append(0)  # Add 0 if no result found for the shell

    # Apply Solarized style to the plot
    plt.figure(figsize=(12, 6))
    color = solarized_colors[i % len(solarized_colors)]
    plt.bar(shells, times, color=color)
    plt.xlabel('Shells', color=solarized_base0)
    plt.ylabel('Execution Time (seconds)', color=solarized_base0)
    plt.title(f'Shell Benchmark Results for: {generic_title}', color=solarized_base0)
    plt.xticks(rotation=45, color=solarized_base00)
    plt.yticks(color=solarized_base00)
    plt.gca().set_facecolor(solarized_base3)
    plt.tight_layout()

    # Save the figure
    chart_filename = os.path.join(chart_output_dir, f"{clean_filename(generic_title)}.png")
    plt.savefig(chart_filename, facecolor=solarized_base3)
    plt.close()