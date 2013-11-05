from felzenszwalb._felzenszwalb_cy import _felzenszwalb_graph as segment
import numpy
import csv

PATH = '/home/fgregg/academic/neighborhoods/code/interchange/'
edges = numpy.loadtxt(PATH + 'edges.csv', skiprows = 1, delimiter=',', dtype=int)
edges -= 1

costs = numpy.loadtxt('costs.csv', skiprows = 1, dtype=float)

print edges
print costs

segs = segment(edges, costs, 1, 20)
print segs

numpy.savetxt('segments.txt', segs, fmt='%d')

