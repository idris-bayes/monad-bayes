import csv
import matplotlib.pyplot as plt
import numpy as np

fixed_groupSize = 4

def groupBenchmarks(file):
  def chunksOf(xs, n):
      for i in range(0, len(xs), n):
          yield xs[i:i + n]

  csv_reader = csv.reader(file, delimiter=',')
  raw_data = [row for row in csv_reader]
  grouped_data = list(chunksOf(raw_data, fixed_groupSize))
  return grouped_data

def plotPage(hs_data, idr_data):
  fig_a, axis_a = plt.subplots(3, 3)
  fig_a.tight_layout()
  for row_idx, (hs_group, idr_group) in enumerate(zip(hs_data, idr_data)):
    # Get header
    header_row  = hs_group[0]
    x_parameter = header_row[0]
    x_values    = list(map(int, header_row[1:]))
    # Iterate over programs
    for col_idx, (hs_prog, idr_prog) in enumerate(zip(hs_group[1:], idr_group[1:])):
      # Set up plot labels
      axis_a[row_idx][col_idx].set_xlabel(x_parameter, fontsize=8)
      axis_a[row_idx][col_idx].set_xticks(x_values)
      axis_a[row_idx][col_idx].set_ylabel("time", fontsize=8)

      prog_name       = hs_prog[0]
      axis_a[row_idx][col_idx].set_title(prog_name, fontsize=10)

      # Plot benchmarks
      hs_prog_values  = list(map(float, hs_prog[1:]))
      idr_prog_values = list(map(float, idr_prog[1:]))
      axis_a[row_idx][col_idx].plot(x_values, hs_prog_values, label='Haskell')
      axis_a[row_idx][col_idx].plot(x_values, idr_prog_values, label='Idris2')

  axis_a[0][0].legend()

with open('haskell/benchmarks.csv') as hs_benchmarks, open('idris2/benchmarks.csv') as idr_benchmarks:
  hs_groups  = groupBenchmarks(hs_benchmarks)
  idr_groups = groupBenchmarks(idr_benchmarks)

  # benchmarks for varying over dataset size
  hs_vary_data  = hs_groups[0:3]
  idr_vary_data = idr_groups[0:3]
  plotPage(hs_vary_data, idr_vary_data)

  # benchmarks for varying over inference parameters
  hs_vary_inf = hs_groups[3:6]
  idr_vary_inf = idr_groups[3:6]
  plotPage(hs_vary_inf, idr_vary_inf)
  plt.show()