import networkx as nx
import random
import math
import matplotlib.pyplot as plt

def set_initial_infected(G, p0):
    """
        Set the initial infected nodes in a graph.

        Parameters
        ----------
        G : networkx.Graph
            The graph to set the initial infected nodes in.
        p0 : float
            The probability that a node is infected initially.

        Returns
        -------
        infected_nodes : list of int
            A list of the nodes that are infected initially.
        """
    # Set the initial infected nodes
    infected_nodes = []
    for node in G.nodes():
        if random.random() < p0:
            infected_nodes.append(node)
    return infected_nodes

def update_status(G, infected_nodes, beta, gamma):
    """
        Update the status of each node in a graph at time t.

        Parameters
        ----------
        G : networkx.Graph
            The graph to update the status of each node in.
        infected_nodes : list of int
            A list of the nodes that are infected at time t-1.
        beta : float
            The transmission probability for the SIS model.
        gamma : float
            The recovery probability for the SIS model.

        Returns
        -------
        new_infected_nodes : list of int
            A list of the nodes that are infected at time t.
        p : float
            The proportion of infected nodes at time t.
        """
    # Set the list of infected nodes at time t
    new_infected_nodes = []

    # Iterate over all nodes in the graph
    for node in G.nodes():
        # If the node is infected at time t-1
        if node in infected_nodes:
            # Recover with probability gamma
            if random.random() > gamma:
                new_infected_nodes.append(node)

        elif node not in infected_nodes:
            # Iterate over the neighbors of the node
            for neighbor in G.neighbors(node):
                # If the neighbor is healthy and becomes infected with probability beta
                if neighbor in infected_nodes and random.random() < beta:
                    new_infected_nodes.append(node)
                    break

        # If the node is not infected at time t-1, add it to the list of infected nodes
        # if it becomes infected with probability beta
        #elif random.random() < beta:
        #    new_infected_nodes.append(node)

    # Return the list of infected nodes and the proportion of infected nodes
    print(len(new_infected_nodes))
    return new_infected_nodes, len(new_infected_nodes) / len(G)


def simulate_spread(G, infected_nodes, beta, gamma, T):
    """
        Simulate the spread of a disease on a graph and plot the proportion of infected nodes over time.

        Parameters
        ----------
        G : networkx.Graph
            The graph to simulate the spread of the disease on.
        infected_nodes : list of int
            A list of the nodes that are infected at time t-1.
        beta : float
            The transmission probability for the SIS model.
        gamma : float
            The recovery probability for the SIS model.
        T : int
            The number of time steps to simulate.

        Returns
        -------
        proportion_infected : list of float
            The proportion of infected nodes at each time step.
        leading_node : int
            The node with the highest eigenvalue centrality.
        """
    # Initialize a list to store the proportion of infected nodes at each time step
    proportion_infected = []

    # Iterate over time steps
    for t in range(T):
        # Update the status of each node and store the proportion of infected nodes
        infected_nodes, p = update_status(G, infected_nodes, beta, gamma)
        proportion_infected.append(p)

    # Plot the proportion of infected nodes over time
    plt.plot(proportion_infected)
    plt.xlabel('Time step')
    plt.ylabel('Proportion of infected nodes')
    plt.show()

    # Calculate the eigenvalue centrality of each node
    eigen_centrality = nx.eigenvector_centrality(G,max_iter=600)

    # Find the highest eigenvalue centrality value
    max_eigen = max(eigen_centrality.values())

    # Find the node with the highest eigenvalue centrality
    leading_node = max(eigen_centrality, key=eigen_centrality.get)

    return proportion_infected, max_eigen, leading_node


def check_threshold(beta, gamma, max_eigen):
    """
    Check whether the epidemic threshold theorem holds for the SIS model on a graph.

    Parameters
    ----------
    beta : float
        The transmission probability for the SIS model.
    gamma : float
        The recovery probability for the SIS model.
    leading_node : int
        The node with the highest eigenvalue centrality.

    Returns
    -------
    None
    """
    # Calculate the value of beta/gamma
    beta_over_gamma = beta / gamma

    # Calculate Threshold
    threshold = 1 / max_eigen

    # Check whether the epidemic threshold theorem holds
    if beta_over_gamma > 1 / threshold:
        print('Epidemic occurs')
    else:
        print('No epidemic occurs')

    print("Threshold 1 /", max_eigen, " = ", threshold)


def main():
    # Set the parameters
    p0 = 0.01
    beta = 0.01
    gamma = 0.1
    T = 20
    n = 1000 # number of vertices

    # Erdos-Renyi graph
    #G = nx.fast_gnp_random_graph(n, 0.1)
    # Complete graph
    #G = nx.complete_graph(n)
    # Star graph
    #G = nx.star_graph(n-1)
    # 2D Lattice graph
    G = nx.grid_2d_graph(round(math.sqrt(n)), round(math.sqrt(n)))
    print(G)


    # Set the initial infected nodes
    infected_nodes = set_initial_infected(G, p0)

    # Update_status(G, infected_nodes, beta, gamma)
    update_status(G, infected_nodes, beta, gamma)

    # Simulate the spread of the disease
    proportion_infected, max_eigen, leading_node = simulate_spread(G, infected_nodes, beta, gamma, T)

    # Print the leading node
    print('Leading node:', leading_node, 'with eigenvalue centrality:', max_eigen)

    # Check whether the epidemic threshold theorem holds
    check_threshold(beta, gamma, max_eigen)

if __name__ == '__main__':
    main()

