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

with open('haskell/benchmarks.csv') as hs_benchmarks, open('idris2/benchmarks.csv') as idr_benchmarks:
  hs_groups  = groupBenchmarks(hs_benchmarks)
  idr_groups = groupBenchmarks(idr_benchmarks)

  for hs_group, idr_group in zip(hs_groups, idr_groups):
    # Get header
    header_row  = hs_group[0]
    x_parameter = header_row[0]
    x_values    = list(map(int, header_row[1:]))
    # Iterate over programs
    for hs_prog, idr_prog in zip(hs_group[1:], idr_group[1:]):
      # Set up plot labels
      fig, ax         = plt.subplots(nrows=1)
      ax.set_xlabel(x_parameter)
      ax.set_xticks(x_values)
      ax.set_ylabel("time")

      prog_name       = hs_prog[0]
      ax.set_title(prog_name)

      # Plot benchmarks
      hs_prog_values  = list(map(float, hs_prog[1:]))
      idr_prog_values = list(map(float, idr_prog[1:]))
      ax.plot(x_values, hs_prog_values, label='Haskell')
      ax.plot(x_values, idr_prog_values, label='Idris2')
      ax.legend()

  plt.show()