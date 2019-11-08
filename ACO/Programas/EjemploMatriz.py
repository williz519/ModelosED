
import numpy as np

from ant_colony import AntColony

M=99999 

distances = np.array([[np.inf,38,55,20,24,M,38,45,51,47,20,14,24,14,M],
                      [38, np.inf,  23, 1,  3,  22, 21, 14, 57, 24, 14 ,M , 28, M,  33],
                      [55, 23, np.inf,  25, 10 ,1 , M , 28, M,  37 ,M,  49, M,  58, M],
                      [20, 1,  25, np.inf,  84, 98, 7 , 10, 7 , 6 , M , 5,  M,  4,  M],
                      [24, 3, 10, 84, np.inf , 4 , M,  7,  5,  13, 14, 37, 9,  M,  6],
                      [M,  22, 1 , 98, 4,  np.inf,  41, 48, 57, 68, 97, 75, 54, 66, 99],
                      [38, 21, M,  7,  M , 41, np.inf,  12, 14, 65, 84, M , 95 ,73, 21],
                      [45, 14, 28, 10, 7,  48, 12, np.inf,  48, M,  1 , M,  M,  1,  M],
                      [51, 57, M,  7,  5 , 57, 14, 48, np.inf,  24, 91,24, 7 , 54, 35],
                      [47, 24, 37, 6,  13 ,68, 65, M,  24 , np.inf , M,  M,  14, M , M],
                      [20, 14, M,  M,  14 ,97, 84, 1,  91, M,  np.inf , M,  45, 9 , 47],
                      [14, M,  49, 5,  37 ,75, M , M,  24, M,  M , np.inf,  10, 14, 14],
                      [24, 28, M,  M , 9  ,54, 95, M,  7,  14, 45, 10 , np.inf,  68, 14],
                      [14, M,  58, 4,  M  ,66, 73, 1,  54 ,M , 9 , 14, 68 , np.inf,  3],
                      [M,  33, M,  M , 6  ,99, 21, M,  35, M , 47 ,14 ,14, 3 , np.inf]])

"""Argumentos ant_colony = AntColony(distances, n_ants, n_best, n_iterations, decay, alpha=1, beta=1)"""

ant_colony = AntColony(distances, 10, 1, 100, 0.95, alpha=1, beta=1)
shortest_path = ant_colony.run()

print ( "shorted_path: {}".format(shortest_path))