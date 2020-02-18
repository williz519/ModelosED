
import numpy as np 

class AntColony:
    """Clase utilizada para manejar el comportamiento de toda la colonia de hormigas"""
    class Ant:
        """Clase utilizada para manejar el comportamiento individual de las hormigas"""
        def __init__(self, start_node_pos, final_node_pos):
            self.start_pos = start_node_pos
            self.actual_node = start_node_pos
            self.final_node = final_node_pos
            self.visited_nodes = []
            self.final_node_reached = False
            self.remember_visited_node(start_node_pos)

        def move_ant(self, node_to_visit):
            """Mueve la hormiga al nodo selecccionado"""
            self.actual_node = node_to_visit
            self.remember_visited_node(node_to_visit)

        def remember_visited_node(self, node_pos):
            """Agrega el nodo visitado a la lista de nodos visitados"""
            self.visited_nodes.append(node_pos)

        def get_visited_nodes(self):
            """Devuelve la lista de nodos visitados"""
            return self.visited_nodes

        def is_final_node_reached(self):
            """Comprueba si la hormiga ha llegado al destino final"""
            if self.actual_node == self.final_node:
                self.final_node_reached = True

        def enable_start_new_path(self):
            """Habilita una nueva busqueda de ruta configurando la variable final_node-reached como Falsa"""
            self.final_node_reached = False
        
        def setup_ant(self):
            """Borra la lista de nodos visitados, almacena el primero y seleccional el primero como inicial"""
            self.visited_nodes[1:] = []
            self.actual_node = self.start_pos

        def __init__(self, in_graph, n_ants, iterations, evaporation_factor, pheromone_adding_constant):
            self.graph = in_graph
            self.n_ants = n_ants
            self.iterations = iterations
            self.evaporation_factor = evaporation_factor
            self.pheromone_adding_constant = pheromone_adding_constant
            self.paths = []
            self.ants = self.create_ants()
            self.best_result = []

        def create_ants(self):
            """Crea una lista que contiene el número total de hormigas especificadas en el nodo inicial"""
            ants = []
            for i in range(self.n_ants):
                ants.append(self.Ant(self.graph.initial_node, self.graph.final_node))
            return ants

        def select_next_node(self, actual_node):
            """Selecciona aleatoriamente el siguiente nodo a visitar"""
            #Calculo de la sumatoria total de la pheromona en cada borde
            total_sum = 0.0
            for edge in actual_node.edges:
                total_sum += edge['Pheromone']

            #Calculo de la probabilidad de cada borde
            prob = 0
            edge_list = []
            p = []
            for edge in actual_node.edges:
                prob = edge['Pheromone']/total_sum
                edge['Probability'] = prob
                edges_list.append(edge)
                p.append(prob)

            #Limpiamos los valores de probabilidad
            for edge in actual_node.edges:
                edge['Probability'] = 0.0
            """Devuelve el nodo en función de la probabilidad de las soluciones"""    
            return np.random.choice(edge_list,1,p)[0]['FinalNode']
        
        def pheromone_update(self):
            """Actualiza el nivel de feromonas de cada uno de los caminos y ordena los caminos por longitud"""
            #Ordenar la lista de acuerdo con el tamaño de la lista
            self.sort_paths()
            for i, path in enumerate(self.paths):
                for j, element in enumerate(path):
                    for edge in self.map.nodes_array[element[0]][element[1]].edges:
                        if (j+1) < len(path):
                            if edge['FinalNode'] == path[j+1]:
                                edge['Pheromone'] = (1.0 - self.evaporation_factor)*edge['Pheromone'] + \
                                    self.pheromone_adding_constant/float(len(path))
                            else:
                                edge['Pheromone'] = (1.0 - self.evaporation_factor)*edge['Pheromone']

        def empty_paths(self):
            """Vaciar la lista de rutas"""
            self.paths[:]

        def sort_paths(self):
            """Ordenar los caminos"""
            self.paths.sort(key=len)

        def add_to_path_results(self, in_path):
            """Añadir la ruta a la lista de resultados"""
            self.paths.append(in_path)

        def get_coincidence_indices(self, lst, element):
            """Obtiene los indices de las coincidencias de elementos del camino"""
            result = []
            offset = -1
            while True:
                try:
                    offset = lst.index(element, offset+1)
                except ValueError:
                    return result
                result.append(offset)

        def delete_loops(self, in_path):
            """Comprueba si hay un bucle en la ruta resultante y lo elimina"""
            res_path = list(in_path)
            for element in res_path:
                coincidences = self.get_coincidence_indices(res_path, element)
                #Reversa la lista para eliminar elementos de atras hacia adelante de la lista
                coincidences.reverse()
                for i, coincidence in enumerate(coincidences):
                    if not i == len(coincidences)-1:
                        res_path[coincidences[i+1]:coincidence] = []

            return res_path

        def calculate_path(self):
            """Realiza el proceso para obtener la mejor ruta"""
            #Se repite el ciclo para el número especificado de veces
            for i in range(self.iterations):
                for ant in self.ants:
                    ant.setup_ant()
                    while not ant.final_node_reached:
                        #Seleccion aleatoria del nodo a visitar
                        node_to_vist = self.select_next_node(self.map.nodes_array[int(ant.actual_node[0])][int(ant.actual_node[1])])
                        #Mover la hormiga al siguiente nodo seleccionado al azar
                        ant.move_ant(node_to_visit)
                        #Compruebe si se ha alcanzado la solución
                        ant.is_final_node_reached()
                    #Agregar la ruta resultante a la lista de rutas
                    self.add_to_path_results(self.delete_loops(ant.get_visited_nodes()))
                    # Habilitar a la hormiga para otra busqueda
                    ant.enable_start_new_path()
                
                # Actualizar el nivel global de feromonas
                self.pheromone_update()
                self.best_result = self.paths[0]

                #Vaciar la lista de rutas
                self.empty_paths()
                print('Iteration: ', i, 'lenght of the path: ', len(self.best_result))
            return self.best_result








