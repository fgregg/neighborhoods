import csv
import numpy
from pystruct.models import GraphCRF
from pystruct.learners import OneSlackSSVM

PATH = '/home/fgregg/academic/neighborhoods/code/interchange/'

js_age = numpy.loadtxt(PATH + 'js_age.csv', skiprows = 1)
js_family = numpy.loadtxt(PATH + 'js_family.csv', skiprows = 1)
js_race = numpy.loadtxt(PATH + 'js_race.csv', skiprows = 1)
js_housing = numpy.loadtxt(PATH + 'js_housing.csv', skiprows = 1)

rail = numpy.loadtxt(PATH + 'rail_intersects.csv', skiprows = 1)
highway = numpy.loadtxt(PATH + 'highway_intersects.csv', skiprows = 1)
grid_street = numpy.loadtxt(PATH + 'grid_intersects.csv', skiprows = 1)
water = numpy.loadtxt(PATH + 'water_intersects.csv', skiprows = 1)

zoning = numpy.loadtxt(PATH + 'zoning_crosses.csv', skiprows = 1)
elementary_school = numpy.loadtxt(PATH + 'elementary_schools_crosses.csv', skiprows = 1)
high_school = numpy.loadtxt(PATH + 'high_schools_crosses.csv', skiprows = 1)
numpy

node_attributes = numpy.vstack((js_age, 
                                js_family,
                                js_race,
                                js_housing,
                                rail,
                                highway,
                                grid_street,
                                water,
                                zoning,
                                elementary_school,
                                high_school)).transpose()

node_labels = numpy.loadtxt(PATH + 'border.csv', skiprows = 1, dtype=int)

edges = numpy.loadtxt(PATH + 'line_graph_edges.txt', skiprows = 1, dtype=int)

Y = (numpy.array(node_labels),)
X = numpy.array(node_attributes)

E = numpy.array(edges, dtype=numpy.int)
X = ((X, E),)

model = GraphCRF(n_features=node_attributes.shape[1], 
                 n_states=2, inference_method='ad3')
svm = OneSlackSSVM(model, verbose=3, n_jobs=10)

svm.fit(X, Y)
print X
print svm.w
predicted_borders = svm.predict(X)
                         
with open(PATH + 'training/predicted_borders.csv', 'w') as f :
   writer = csv.writer(f, delimiter=' ')
   for edge in predicted_borders[0] :
      writer.writerow([edge])
