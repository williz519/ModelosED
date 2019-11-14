import random as rn
import numpy as np
import math
import pants,os,re
from numpy.random import choice as np_choice
from matplotlib import pyplot as plt


class Graph(object):
    def __init__(self, cost_matrix: list, rank: int):
        """
        :param cost_matrix:
        :param rank: rank of the cost matrix
        """
        self.matrix = cost_matrix
        self.rank = rank
        # noinspection PyUnusedLocal
        #self.pheromone = [[1 / (rank * rank) for j in range(rank)] for i in range(rank)]


class AntColony(object):

    def __init__(self, distances, nodes, n_ants, n_best, n_iterations, rho, NodoIni, NodoFin, alpha=1, beta=1) :
        """
        Argumentos:
            distancia (2D numpy.array): Matriz cuadrada de distanceias. la Diagonal es asumida a ser np.inf.
            n_ants (int): Nummero de hormigas corriendo por iteracion
            n_best (int): Numero de mejores hormigas que depositan pheromona
            n_iteration (int): Numero de iteraciones
            rho (float): Rate de decaimiento de la pheromona. La pheromone es multiplicada por el decaimiento, so 0.95 will lead to rho, 0.5 to much faster rho.
            alpha (int o float): exponenet on pheromone, higher alpha gives pheromone more weight. Default=1
            beta (int o float): exponent on distance, higher beta give distance more weight. Default=1

        Example:
            ant_colony = AntColony(german_distances, 100, 20, 2000, 0.95, alpha=1, beta=2)          
        """
        self.distances  = distances
        self.nodes = nodes
        self.pheromone = np.ones(self.distances.shape) / len(distances)
        self.all_inds = range(len(distances))
        self.n_ants = n_ants
        self.n_best = n_best
        self.n_iterations = n_iterations
        self.rho = rho
        self.NodoIni = NodoIni
        self.NodoFin = NodoFin
        self.alpha = alpha
        self.beta = beta
        self.NodoActual = 0

        self.eta = [[0 if i == j else 1 / distances[i][j] for j in range(self.Graph.rank)] for i in
                    range(self.Graph.rank)]  # Información Heuristica
        self.NoVisitados = [i for i in range(self.Graph.rank)]  # nodos que están permitidos para la próxima selección
        

    def run(self):
        shortest_path = None
        all_time_shortest_path = ("posicion", np.inf)
        for i in range(self.n_iterations):
            all_paths = self.gen_all_paths()
            self.spread_pheronome(all_paths, self.n_best, shortest_path=shortest_path)
            shortest_path = min(all_paths, key=lambda x: x[1])
            #print (shortest_path)
            if shortest_path[1] < all_time_shortest_path[1]:
                #print('path: ', shortest_path[1])
                all_time_shortest_path = shortest_path            
            self.pheromone * self.rho
        return all_time_shortest_path

    def spread_pheronome(self, all_paths, n_best, shortest_path):
        sorted_paths = sorted(all_paths, key=lambda x: x[1])
        for path, distances in sorted_paths[:n_best]:
            for move in path:
                self.pheromone[move] += 1.0 / self.distances[move]

    def gen_path_dist(self, path):
        total_dist = 0
        for ele in path:
            total_dist += self.distances[ele]
            #print("Distancia Total: ", total_dist)
        return total_dist

    def gen_all_paths(self):
        all_paths = []
        for i in range(self.n_ants):
            path = self.gen_path(0,1)
            all_paths.append((path, self.gen_path_dist(path)))
        return all_paths

    def gen_path(self, start, goal):
        self.Visitados = []

        path = []
        if self.NodoIni is None:
            start = [random.randint(0,self.all_inds-1)]
        else:
            start = self.NodoIni
        
        goal = self.NodoFin
        
        self.Visitados.append(start)
        self.current = start
        self.NoVisitados.remove(start)
 
        prev = start
        
        #NodoActual = goal
        y = 0
        while goal != start:
            try:
                for i in range(len(self.distances) - 1):
                    move = self.pick_move(self.pheromone[prev], self.distances[prev], self.Visitados)
                    path.append((prev, move))
                    prev = move
                    self.Visitados.insert(y,move)
                
                path.insert(y,goal)
                goal = self.Visitados[goal]
                y = y + 1
            except KeyError:
                break
                print("El camino no es accesible")

            
        
        path.insert(0,start)

        #path.append((prev, start)) # going back to where we started    
        print('Path: ', path)
        return path
        

    def pick_move(self, pheromone, distances, Visitados): #salto de nodo
        pheromone = np.copy(pheromone)
        pheromone[list(Visitados)] = 0

        row = 0

        for i in self.NoVisitados:
            denominator += pheromone[self.current][i] ** self.alpha * self.eta[self.current][i] ** self.beta
        
        probabilities = [0 for i in range(self.Graph.rank)]  # probabilities for moving to a node in the next step
        for i in range(self.Graph.rank):
            try:
                self.NoVisitados.index(i)  # test if allowed list contains i
                probabilities[i] = pheromone[self.current][i] ** self.alpha * \
                    self.eta[self.current][i] ** self.beta / denominator
            except ValueError:
                pass  # do nothing
        # select next node by probability roulette
        move = 0
        rand = random.random()
        for i, probability in enumerate(probabilities):
            rand -= probability
            if rand <= 0:
                move = i
                break
        self.NoVisitados.remove(selected)
        self.Visitados.append(selected)
        #self.total_cost += self.graph.matrix[self.current][selected]
        self.current = move
        return move