import csv
import numpy
from pystruct.models import GraphCRF, PottsEdgeFeatureGraphCRF
from pystruct.learners import NSlackSSVM as learner

PATH = '/home/fgregg/academic/neighborhoods/code/interchange/'

node_labels = numpy.loadtxt(PATH + 'potts_labels.csv', skiprows = 1)
#node_labels = numpy.loadtxt(PATH + 'ks_label.csv', skiprows = 1)
_, node_labels = numpy.unique(node_labels, return_inverse=True)

edge_attributes = numpy.loadtxt('model.matrix', skiprows=1)

markers = numpy.loadtxt(PATH + 'markers.csv', skiprows=1, delimiter=',',
                        dtype=numpy.int)

markers[:,0] -= 1
markers[:,1] = node_labels[markers[:,0]]


print markers

raise

# js_age = numpy.loadtxt(PATH + 'js_age.csv', skiprows = 1)
# js_family = numpy.loadtxt(PATH + 'js_family.csv', skiprows = 1)
# js_race = numpy.loadtxt(PATH + 'js_race.csv', skiprows = 1)
# js_housing = numpy.loadtxt(PATH + 'js_housing.csv', skiprows = 1)

# rail = numpy.loadtxt(PATH + 'rail_intersects.csv', skiprows = 1)
# highway = numpy.loadtxt(PATH + 'highway_intersects.csv', skiprows = 1)
# grid_street = numpy.loadtxt(PATH + 'grid_intersects.csv', skiprows = 1)
# water = numpy.loadtxt(PATH + 'water_intersects.csv', skiprows = 1)

# zoning = numpy.loadtxt(PATH + 'zoning_crosses.csv', skiprows = 1)
# elementary_school = numpy.loadtxt(PATH + 'elementary_schools_crosses.csv', skiprows = 1)
# high_school = numpy.loadtxt(PATH + 'high_schools_crosses.csv', skiprows = 1)
# numpy

# block_angle = numpy.loadtxt(PATH + 'block_angles.csv', skiprows = 1)

# edge_attributes = numpy.vstack((js_age, 
#                                 js_family,
#                                 js_race,
#                                 js_housing,
#                                 rail,
#                                 highway,
#                                 grid_street,
#                                 water,
#                                 zoning,
#                                 elementary_school,
#                                 high_school,
#                                 block_angle)).transpose()

edges = numpy.loadtxt(PATH + 'edges.csv', skiprows = 1, dtype=int, delimiter=',')
edges += -1


Y = (numpy.array(node_labels, dtype=numpy.int),)
print Y 
X= numpy.empty((len(node_labels), 0), dtype=numpy.float)

#E = numpy.empty((0, 2), dtype=numpy.int)
E = numpy.array(edges, dtype=numpy.int)
X = ((X, E, edge_attributes),)


#model = GraphCRF(n_features=node_attributes.shape[1],
#                 #class_weight=[1, 0.001],
#                 n_states=2, inference_method='qpbo')

model = PottsEdgeFeatureGraphCRF(n_states = 23, 
                                 n_features = 0,
                                 n_edge_features = edge_attributes.shape[1],
                                 inference_method = 'qpbo',
                                 markers=markers)

                         
                         


svm = learner(model, 
              verbose=3, 
              n_jobs=5, 
              max_iter=1000, 
              C=0.01, 
              show_loss_every=1)

svm.fit(X, Y)
print X
print svm.w
predicted_borders = svm.predict(X)
                         
with open('/home/fgregg/academic/neighborhoods/code/training/predicted_borders.csv', 'w') as f :
   writer = csv.writer(f, delimiter=' ')
   for edge in predicted_borders[0] :
      writer.writerow([edge])
