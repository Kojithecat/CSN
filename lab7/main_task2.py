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
            if random.random() < gamma:
                continue
            # Iterate over the neighbors of the node
            for neighbor in G.neighbors(node):
                # If the neighbor is healthy and becomes infected with probability beta
                if neighbor not in infected_nodes and random.random() < beta:
                    new_infected_nodes.append(neighbor)

        # If the node is not infected at time t-1, add it to the list of infected nodes
        # if it becomes infected with probability beta
        elif random.random() < beta:
            new_infected_nodes.append(node)

    # Return the list of infected nodes and the proportion of infected nodes
    return new_infected_nodes, len(new_infected_nodes) / len(G)

def simulate_spread(G, G_type, infected_nodes, beta, gamma, T):
    """
        Simulate the spread of a disease on a graph and plot the proportion of infected nodes over time.

        Parameters
        ----------
        G : networkx.Graph
            The graph to simulate the spread of the disease on.
        G_type : str
            The type of graph.
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
    # plt.plot(proportion_infected)
    # plt.xlabel('Time step')
    # plt.ylabel('Proportion of infected nodes')
    # plt.show()

    # Calculate the eigenvalue centrality of each node
    if G_type == 'lattice' or G_type == 'star':
        eigen_centrality = nx.eigenvector_centrality(G, max_iter=600)
    else:
        eigen_centrality = nx.eigenvector_centrality(G)

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

    print('beta:', beta)
    print('gamma:', gamma)
    print('beta/gamma:', beta_over_gamma)

    # Check whether the epidemic threshold theorem holds
    if beta_over_gamma > threshold:
        print('Epidemic occurs')
    else:
        print('No epidemic occurs')


def main():
    # #Task 1
    # # Set the parameters
    # p0 = 0.01
    # beta = 0.1
    # gamma = 0.6
    # T = 10
    # n = 1000  # number of vertices
    #
    # # List of graph types
    # graph_types = ["random", "complete", "star", "lattice"]
    #
    # for graph_type in graph_types:
    #     if graph_type == "random":
    #         G = nx.fast_gnp_random_graph(n, 0.1)
    #     elif graph_type == "complete":
    #         G = nx.complete_graph(n)
    #     elif graph_type == "star":
    #         G = nx.star_graph(n)
    #     elif graph_type == "lattice":
    #         G = nx.grid_2d_graph(int(math.sqrt(n)), int(math.sqrt(n)))
    #
    #     # Set the initial infected nodes
    #     infected_nodes = set_initial_infected(G, p0)
    #
    #     # Update the status of the nodes
    #     update_status(G, infected_nodes, beta, gamma)
    #
    #     # Simulate the spread of the disease
    #     proportion_infected, max_eigen, leading_node = simulate_spread(G, graph_type, infected_nodes, beta, gamma, T)
    #
    #     # Print the leading node
    #     print("Graph type:", graph_type)
    #     print("Leading node:", leading_node, "with eigenvalue centrality:", max_eigen)
    #
    #     # Check whether the epidemic threshold theorem holds
    #     check_threshold(beta, gamma, max_eigen)

    # Taks 2: two sets of beta and gamma, one slightly above the threshold and one slightly below the threshold
    # Set the parameters
    p0 = 0.01
    T = 10
    n = 1000  # number of vertices

    # List of graph types
    graph_types = ["random", "complete", "star", "lattice"]

    for graph_type in graph_types:
        if graph_type == "random":
            G = nx.fast_gnp_random_graph(n, 0.1)
            threshold = 1 / 0.04097554219168759
        elif graph_type == "complete":
            G = nx.complete_graph(n)
            threshold = 1 / 0.0316227766016838
        elif graph_type == "star":
            G = nx.star_graph(n)
            threshold = 1 / 0.7070926221900043
        elif graph_type == "lattice":
            G = nx.grid_2d_graph(int(math.sqrt(n)), int(math.sqrt(n)))
            threshold = 1 / 0.062295051731419034

        # Set the beta and gamma values slightly above and below the threshold
        value_above_t = threshold * 1.01
        gamma_above = (threshold / value_above_t)/100
        beta_above = (value_above_t)/100

        value_below_t = threshold * 0.98
        gamma_below = (threshold / value_below_t)/100
        beta_below = (value_below_t)/100

        # Set the initial infected nodes
        infected_nodes = set_initial_infected(G, p0)

        # Simulate the spread of the disease using the beta and gamma values above the threshold
        proportion_infected_above, max_eigen_above, leading_node_above = simulate_spread(G, graph_type,
                                                                                         infected_nodes, beta_above,
                                                                                         gamma_above, T)

        # Simulate the spread of the disease using the beta and gamma values below the threshold
        proportion_infected_below, max_eigen_below, leading_node_below = simulate_spread(G, graph_type,
                                                                                         infected_nodes, beta_below,
                                                                                         gamma_below, T)

        # Print the leading nodes and proportion infected for each simulation
        print("Graph type:", graph_type)
        print("Threshold:", threshold)
        check_threshold(beta_above, gamma_above, max_eigen_above)
        check_threshold(beta_below, gamma_below, max_eigen_below)
        print('##############################################')

if __name__ == '__main__':
    main()

