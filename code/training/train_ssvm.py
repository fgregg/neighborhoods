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
       node_attributes.append(tuple([float(atr) for atr in row[2:10]]))
       node_labels.append(int(row[10]))

with open("line_graph_edges.txt") as f :
   reader = csv.reader(f, delimiter = ' ')
   for row in reader :
      edges.append([int(col) for col in row])

Y = (numpy.array(node_labels),)
X = numpy.array(node_attributes)

E = numpy.array(edges, dtype=numpy.int)
X = ((X, E),)

model = GraphCRF(n_features=8, n_states=2, inference_method='ad3')
svm = OneSlackSSVM(model, verbose=3, n_jobs=10)

svm.fit(X, Y)
print X
print svm.w
predicted_borders = svm.predict(X)

import code
code.interact(local=locals())

                         
with open('predicted_borders.csv', 'w') as f :
   writer = csv.writer(f, delimiter=' ')
   for edge in predicted_borders[0] :
      writer.writerow([edge])
