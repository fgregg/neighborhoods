import csv
import numpy
from pystruct.models import GraphCRF
from pystruct.learners import OneSlackSSVM

node_list = []
node_attributes = []
node_labels = []

edges = []

with open("edge_features.txt") as f :
   reader = csv.reader(f, delimiter = ' ')
   reader.next()
   for row in reader :
       node_list.append((int(row[0]), int(row[1])))
       node_attributes.append((float(row[2]),))
       node_labels.append(int(row[3]))

with open("line_graph_edges.txt") as f :
   reader = csv.reader(f, delimiter = ' ')
   for row in reader :
      edges.append([int(col) for col in row])


Y = (numpy.array(node_labels),)
X = numpy.array(node_attributes)

E = numpy.array(edges, dtype=numpy.int)
X = ((X, E),)

model = GraphCRF(n_features=1, n_states=2, inference_method='ad3')
svm = OneSlackSSVM(model, verbose=1)

svm.fit(X, Y)
print X
print svm.w
                         
