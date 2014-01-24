import argparse
import csv
import numpy
from pystruct.models import GraphCRF, PottsEdgeFeatureGraphCRF
from pystruct.models import MixedSemanticPottsEdgeFeatureGraphCRF
from pystruct.learners import NSlackSSVM as learner

parser = argparse.ArgumentParser(description='Train Potts Model.')
parser.add_argument('regularizer', metavar='C', type=float, nargs='+',
                    help='regularizer', default=0.01)


args = parser.parse_args()

PATH = '/home/fgregg/academic/neighborhoods/code/interchange/'

#node_labels = numpy.loadtxt(PATH + 'potts_labels.csv', skiprows = 1)
node_labels = numpy.loadtxt(PATH + 'ks_label_semantic.csv', skiprows = 1)

print node_labels

node_features = numpy.loadtxt(PATH + 'block_pop.csv', skiprows = 1, dtype=numpy.float)

_, node_labels = numpy.unique(node_labels, return_inverse=True)

edge_attributes = numpy.loadtxt('model.matrix', skiprows=1)

edges = numpy.loadtxt(PATH + 'edges.csv', skiprows = 1, dtype=int, delimiter=',')
edges += -1

Y = (numpy.array(node_labels, dtype=numpy.int),)
X = node_features
X = X.reshape((-1, 1))


E = numpy.array(edges, dtype=numpy.int)
X = ((X, E, edge_attributes),)

model = MixedSemanticPottsEdgeFeatureGraphCRF(n_states = 38, 
                                              n_features = 1,
                                              n_edge_features = edge_attributes.shape[1],
                                              inference_method = 'qpbo',
                                              semantic_labels = [9])


svm = learner(model, 
              verbose=3, 
              n_jobs=1, 
              max_iter=1000, 
              C=args.regularizer[0], 
              show_loss_every=1)

svm.fit(X, Y)
print X

with open('/home/fgregg/academic/neighborhoods/code/training/weights.csv', 'w') as f :
   writer = csv.writer(f, delimiter=' ')
   writer.writerow(svm.w)

predicted_borders = svm.predict(X)
                         
with open('/home/fgregg/academic/neighborhoods/code/training/predicted_borders.csv', 'w') as f :
   writer = csv.writer(f, delimiter=' ')
   for edge in predicted_borders[0] :
      writer.writerow([edge])

chicago_nodes_n = len(numpy.loadtxt(PATH + 'chicago_node_labels.csv', skiprows = 1))
chicago_edges = numpy.loadtxt(PATH + 'chicago_edges.csv', skiprows = 1, dtype=int, delimiter=',')
X_chicago = ((numpy.empty((chicago_nodes_n, 0), dtype=numpy.float), 
              numpy.array(chicago_edges, dtype=numpy.int) - 1,
              numpy.loadtxt('chicago.model.matrix', skiprows=1)),)

predicted_chicago = svm.predict(X_chicago)
                         
with open('/home/fgregg/academic/neighborhoods/code/training/predicted_chicago.csv', 'w') as f :
   writer = csv.writer(f, delimiter=' ')
   for edge in predicted_chicago[0] :
      writer.writerow([edge])
