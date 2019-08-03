# -*- coding: utf-8 -*-
"""
Created on Tue Aug 28 15:03:24 2018

@author: albeiro.alvarez
"""

import math,pants,os,re
import random
import numpy as np

#from aco1 import AntColony
#from aco2 import AntColony
#from aco3 import AntColony
#from plot1 import plot

from aco4 import ACO, Graph
from plot import plot


"Matriz de distancias entre nodos"

distances = np.array([[np.inf, 2, 2, 15, 7, 6, 20],
                      [2, np.inf, 4, 8, 2, 4, 4],
                      [2, 4, np.inf, 1, 3, 6, 6],
                      [15, 8, 1, np.inf, 2, 7, 7],
                      [7, 2, 3, 2, np.inf, 8, 3],
                      [6, 4, 6, 7, 8, np.inf, 3],
                      [20, 4, 6, 7, 3, 3,  np.inf]])


def main():
    coord = []
    nodes = []
    with open('Med.txt') as f:
        for line in f.readlines():
            nodo = line.split( )
            coord.append(dict(index=float(nodo[0]), x=float(nodo[1]), y=float(nodo[2])))
            nodes.append((float(nodo[1]), float(nodo[2])))
    cost_matrix = []
    num_nodos = len(coord)
    
    for i in range(num_nodos):
        row = []
        for j in range(num_nodos):
            row.append(distances[i][j])
        cost_matrix.append(row)
    
    "Para Correr con ACO4"
    #aco = ACO(n_ants, n_iterations, alpha, beta, rho, NodoInicio, q, strategy)
    """aco = ACO(100, 100, 1.0, 10.0, 0.5, 4, 10, 2)
    graph = Graph(cost_matrix, num_nodos)
    path, cost = aco.solve(graph)
    print('costo: {}, Camino: {}'.format(cost, path))
    plot(nodes, path)"""

    #print(coord)
    #print(puntos)
    #print(cost_matrix)
    #plot(points, path)

    #_colony_size = 10
   # _steps = 50
    #_NodoInicio = 2
    #_NodoFin = 3

    #_nodes = [(random.uniform(-400, 400), random.uniform(-400, 400)) for _ in range(0, 15)]
    #_nodes = puntos
    
    "Para Correr con ACO3"
    ant_colony = AntColony(distances, nodes, 1, 1, 100, 0.95, 4, alpha=1, beta=1)
    shortest_path = ant_colony.run()
    print ( "shorted_path: {}".format(shortest_path[0]))
    print("Distancia Total de viaje para completar el tour: {}".format(shortest_path[1]))
    plot(nodes, path)
    #ant_colony.plot()

    #graph = Graph(cost_matrix, rank)
    #plot(nodes, path)



    

if __name__ == '__main__':
    main()
