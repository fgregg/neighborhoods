import numpy as np
from pygco import cut_simple, cut_from_graph, energy_of_graph_assignment
import csv

def constantEdges() :
    edges = []
    with open('edges.csv', 'rb') as f :
        reader = csv.reader(f)
        reader.next()
        for row in reader :
            edges.append(row)
    edges = np.array(edges, dtype='int32', ndmin=2)
    edges = edges - 1

    unary = []
    with open('unary.csv', 'rb') as f :
        reader = csv.reader(f)
        reader.next()
        for row in reader :
            unary.append(row)

    unary = np.array(unary, dtype = "float32")
    unary = (-100 * unary).copy("C").astype(np.int32)

    edge_weights = []
    with open('edge_weights.csv', 'rb') as f :
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

    with open('potts_labels.csv', 'wb') as f:
        writer = csv.writer(f)
        writer.writerow(['label'])
        for label in result_graph :
            writer.writerow([label + 1])
    

def example_binary():
    # generate trivial data
    x = np.ones((10, 10))

    x[:, 5:] = -1
    x_noisy = x + np.random.normal(0, 0.8, size=x.shape)

    x_thresh = x_noisy > .0


    # create unaries
    unaries = x_noisy

    # as we convert to int, we need to multipy to get sensible values
    unaries = (10 * np.dstack([unaries, -unaries]).copy("C")).astype(np.int32)
    # create potts pairwise
    pairwise = -15 * np.eye(2, dtype=np.int32)

    # do simple cut
    result = cut_simple(unaries, pairwise)

    # use the gerneral graph algorithm
    # first, we construct the grid graph
    inds = np.arange(x.size).reshape(x.shape)
    horz = np.c_[inds[:, :-1].ravel(), inds[:, 1:].ravel()]
    vert = np.c_[inds[:-1, :].ravel(), inds[1:, :].ravel()]
    edges = np.vstack([horz, vert]).astype(np.int32)

    print edges
    print pairwise
    print unaries.reshape(-1,2)

    # we flatten the unaries
    result_graph = cut_from_graph(edges, unaries.reshape(-1, 2), pairwise, 5, 'swap')

    # plot results
    plt.subplot(231, title="original")
    plt.imshow(x, interpolation='nearest')
    plt.subplot(232, title="noisy version")
    plt.imshow(x_noisy, interpolation='nearest')
    plt.subplot(233, title="rounded to integers")
    plt.imshow(unaries[:, :, 0], interpolation='nearest')
    plt.subplot(234, title="thresholding result")
    plt.imshow(x_thresh, interpolation='nearest')
    plt.subplot(235, title="cut_simple")
    plt.imshow(result, interpolation='nearest')
    plt.subplot(236, title="cut_from_graph")
    plt.imshow(result_graph.reshape(x.shape), interpolation='nearest')

    print result_graph.reshape(x.shape)

    plt.show()


def example_multinomial():
    # generate dataset with three stripes
    np.random.seed(15)
    x = np.zeros((10, 12, 3))
    x[:, :4, 0] = -1
    x[:, 4:8, 1] = -1
    x[:, 8:, 2] = -1
    print x
    unaries = x + 1.5 * np.random.normal(size=x.shape)
    x = np.argmin(x, axis=2)
    print x
    unaries = (unaries * 10).astype(np.int32)
    print unaries
    x_thresh = np.argmin(unaries, axis=2)

    # potts potential
    pairwise_potts = -2 * np.eye(3, dtype=np.int32)
    print pairwise_potts
    result = cut_simple(unaries, 10 * pairwise_potts)
    # potential that penalizes 0-1 and 1-2 less thann 0-2
    pairwise_1d = -15 * np.eye(3, dtype=np.int32) - 8
    pairwise_1d[-1, 0] = 0
    pairwise_1d[0, -1] = 0
    print(pairwise_1d)
    result_1d = cut_simple(unaries, pairwise_1d)
    plt.subplot(141, title="original")
    plt.imshow(x, interpolation="nearest")
    plt.subplot(142, title="thresholded unaries")
    plt.imshow(x_thresh, interpolation="nearest")
    plt.subplot(143, title="potts potentials")
    plt.imshow(result, interpolation="nearest")
    plt.subplot(144, title="1d topology potentials")
    plt.imshow(result_1d, interpolation="nearest")
    plt.show()
    print x


#example_binary()
#example_multinomial()

constantEdges()
