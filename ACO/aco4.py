# -*- coding: utf-8 -*-
"""
Created on Tue Aug 28 15:05:33 2018

@author: albeiro.alvarez
"""

import random


class Graph(object):
    def __init__(self, cost_matrix: list, rank: int):
        """
        cost_matrix: Matriz de costos
        rank: Rango de la matriz de costos
        """
        self.matrix = cost_matrix
        self.rank = rank
        self.pheromone = [[1 / (rank * rank) for j in range(rank)] for i in range(rank)]


class ACO(object):
    def __init__(self, n_ants: int, n_iterations: int, alpha: float, beta: float, rho: float, NodoIni: int, q: int,
                 NodoFin: int, strategy: int):
        """
        n_ants: Número de Hormigas
        n_iterations: Número de Iteraciones
        alpha: relative importance of pheromone
        beta: relative importance of heuristic information
        rho: pheromone residual coefficient
        q: pheromone intensity
        strategy: pheromone update strategy. 0 - ant-cycle, 1 - ant-quality, 2 - ant-density
        """
        self.Q = q
        self.rho = rho
        self.NodoIni = NodoIni
        self.NodoFin = NodoFin
        self.beta = beta
        self.alpha = alpha
        self.n_ants = n_ants
        self.n_iterations = n_iterations
        self.update_strategy = strategy

    def _update_pheromone(self, graph: Graph, ants: list):
        for i, row in enumerate(graph.pheromone):
            for j, col in enumerate(row):
                graph.pheromone[i][j] *= self.rho
                for ant in ants:
                    graph.pheromone[i][j] += ant.pheromone_delta[i][j]

    
    def solve(self, graph: Graph):
        
        best_cost = float('inf')
        best_solution = []
        for gen in range(self.n_iterations):
            # noinspection PyUnusedLocal
            ants = [_Ant(self, graph) for i in range(self.n_ants)]
            for ant in ants:
                for i in range(graph.rank - 1):
                    ant._select_next()
                ant.total_cost += graph.matrix[ant.tabu[-1]][ant.tabu[0]]
                if ant.total_cost < best_cost:
                    best_cost = ant.total_cost
                    best_solution = [] + ant.tabu
                
                # actualizar pheromona
                ant._update_pheromone_delta()
            self._update_pheromone(graph, ants)
            #print('generation #{}, best cost: {}, path: {}'.format(gen, best_cost, best_solution))
        return best_solution, best_cost
        print('generation #{}, best cost: {}, path: {}'.format(gen, best_cost, best_solution))


class _Ant(object):
    def __init__(self, aco: ACO, graph: Graph):
        self.colony = aco
        self.graph = graph
        self.total_cost = 0.0
        self.tabu = []  # Lista Tabu
        self.pheromone_delta = []  # emento local de pheromona
        self.allowed = [i for i in range(graph.rank)]  # nodos que están permitidos para la próxima selección
        self.eta = [[0 if i == j else 1 / graph.matrix[i][j] for j in range(graph.rank)] for i in
                    range(graph.rank)]  # Información Heuristica
        
        # Comienza desde un nodo elegido
        if aco.NodoIni is None:
            start = random.randint(0, graph.rank - 1)  
        else:
            start = aco.NodoIni
        
        self.tabu.append(start)
        self.current = start
        self.allowed.remove(start)
    
    # Finaliza en el nodo Elegido
    

    def _select_next(self):
        denominator = 0
    
        for i in self.allowed:
            denominator += self.graph.pheromone[self.current][i] ** self.colony.alpha * self.eta[self.current][
                                                                                            i] ** self.colony.beta
        
        probabilities = [0 for i in range(self.graph.rank)]  # probabilities for moving to a node in the next step
        for i in range(self.graph.rank):
            try:
                self.allowed.index(i)  # test if allowed list contains i
                probabilities[i] = self.graph.pheromone[self.current][i] ** self.colony.alpha * \
                    self.eta[self.current][i] ** self.colony.beta / denominator
            except ValueError:
                pass  # do nothing
        # select next node by probability roulette
        selected = 0
        rand = random.random()
        for i, probability in enumerate(probabilities):
            rand -= probability
            if rand <= 0:
                selected = i
                break
        self.allowed.remove(selected)
        self.tabu.append(selected)
        self.total_cost += self.graph.matrix[self.current][selected]
        self.current = selected

    # 
    def _update_pheromone_delta(self):
        self.pheromone_delta = [[0 for j in range(self.graph.rank)] for i in range(self.graph.rank)]
        for _ in range(1, len(self.tabu)):
            i = self.tabu[_ - 1]
            j = self.tabu[_]
            if self.colony.update_strategy == 1:  # ant-quality system
                self.pheromone_delta[i][j] = self.colony.Q
            elif self.colony.update_strategy == 2:  # ant-density system
                # noinspection PyTypeChecker
                self.pheromone_delta[i][j] = self.colony.Q / self.graph.matrix[i][j]
            else:  # ant-cycle system
                self.pheromone_delta[i][j] = self.colony.Q / self.total_cost