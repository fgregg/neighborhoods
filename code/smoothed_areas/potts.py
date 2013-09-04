import numpy as np
from pygco import cut_simple, cut_from_graph, energy_of_graph_assignment
import csv

PATH = '/home/fgregg/academic/neighborhoods/code/interchange/'

def constantEdges() :
    edges = []
    with open(PATH + 'edges.csv', 'rb') as f :
        reader = csv.reader(f)
        reader.next()
        for row in reader :
            edges.append(row)
    edges = np.array(edges, dtype='int32', ndmin=2)
    edges = edges - 1

    unary = []
    with open(PATH + 'unary.csv', 'rb') as f :
        reader = csv.reader(f)
        reader.next()
        for row in reader :
            unary.append(row)

    unary = np.array(unary, dtype = "float32")
    unary = (-100 * unary).copy("C").astype(np.int32)

    edge_weights = []
    with open(PATH + 'edge_weights.csv', 'rb') as f :
        reader = csv.reader(f)
        reader.next()
        for row in reader :
            edge_weights.append(row)
            
    edge_weights = np.array(edge_weights, dtype = "int32")
    edge_weights.shape = (len(edges),)

    
    num_blocks, num_labels = unary.shape


    pairwise = -50 * np.eye(num_labels, dtype=np.int32)
#    pairwise[:, num_labels-1] = -40
#    pairwise[num_labels-1, :] = -40
#    pairwise[num_labels-1, num_labels-1] = -50

    print pairwise


    edge_weights.shape = (len(edge_weights), 1)
    
    edges = np.concatenate((edges, edge_weights), axis=1)


    (result_graph, energy) = cut_from_graph(edges, unary, pairwise, algorithm="swap", n_iter = -1)

    print energy

    print energy_of_graph_assignment(edges, unary, pairwise, result_graph)

    with open(PATH + 'potts_labels.csv', 'wb') as f:
        writer = csv.writer(f)
        writer.writerow(['label'])
        for label in result_graph :
            writer.writerow([label + 1])
    

constantEdges()
