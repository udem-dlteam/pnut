import os
import matplotlib.pyplot as plt

# Set the root benchmark directory
root_benchmark_dir = "benchmarks"
charts_dir = os.path.join(root_benchmark_dir, "CHARTS")

def read_benchmark_results(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()[1:]  # Skip the first line
    results = []
    for line in lines:
        parts = line.split()
        if len(parts) < 4:
            continue  # Skip lines that don't have enough parts
        time_str = parts[0]
        full_description = " ".join(parts[1:])
        description_parts = full_description.split()
        if len(description_parts) < 6:
            continue  # Skip lines that don't have enough description parts
        shell = description_parts[1]
        metric = description_parts[3]
        options = description_parts[5]
        description = f"{shell}, {metric}, {options}"
        try:
            time_ms = float(time_str[:-1]) * 1000  # Convert seconds to milliseconds
            results.append((time_ms, description, shell))
        except ValueError:
            continue  # Skip lines with invalid time format
    return results

def plot_benchmark_results(benchmark_name, shell, results):
    descriptions = [desc for _, desc, _ in results]
    times = [time for time, _, _ in results]

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

    # Assign colors to bars
    colors = [solarized_colors[i % len(solarized_colors)] for i in range(len(descriptions))]

    plt.figure(figsize=(10, 6))
    plt.barh(descriptions, times, color=colors)
    plt.xlabel('Time (ms)', color=solarized_base0)
    plt.ylabel('Configuration', color=solarized_base0)
    plt.title(f'{benchmark_name} Benchmark Results ({shell})', color=solarized_base0)
    plt.xticks(rotation=45, color=solarized_base00)
    plt.yticks(color=solarized_base00)
    plt.gca().set_facecolor(solarized_base3)
    plt.tight_layout()

    # Save the plot to the CHARTS directory
    os.makedirs(charts_dir, exist_ok=True)
    plt.savefig(os.path.join(charts_dir, f"{benchmark_name}_{shell}_benchmark_results.png"))
    plt.close()

def process_benchmarks():
    for subdir in os.listdir(root_benchmark_dir):
        benchmark_dir = os.path.join(root_benchmark_dir, subdir)
        if not os.path.isdir(benchmark_dir) or subdir == "CHARTS":
            continue  # Skip if it's not a directory or is the CHARTS directory
        result_file = os.path.join(benchmark_dir, "benchmark-results", "output.txt")
        if os.path.isfile(result_file):
            benchmark_name = os.path.basename(benchmark_dir)
            results = read_benchmark_results(result_file)
            if not results:
                print(f"No valid results in {result_file}")
                continue
            shells = set(shell for _, _, shell in results)
            for shell in shells:
                shell_results = [result for result in results if result[2] == shell]
                plot_benchmark_results(benchmark_name, shell, shell_results)
        else:
            print(f"No results file found for {benchmark_dir}")

if __name__ == "__main__":
    process_benchmarks()
