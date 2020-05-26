# -*- coding: utf-8 -*-
"""
Created on Tue Aug 28 15:06:46 2018

@author: albeiro.alvarez
"""

import operator, math

import matplotlib.pyplot as plt


def plot(points, path: list, line_width=0.5, point_radius=math.sqrt(2.0)):
    x = []
    y = []
    for point in points:
        x.append(point[0])
        y.append(point[1])
    # noinspection PyUnusedLocal
    y = list(map(operator.sub, [max(y) for i in range(len(points))], y))
    plt.plot(x, y, 'co')

    for _ in range(1, len(path)):
        i = path[_ - 1]
        j = path[_]
        # noinspection PyUnresolvedReferences
        plt.arrow(x[i], y[i], x[j] - x[i], y[j] - y[i], color='r', length_includes_head=True)

    # noinspection PyTypeChecker
    #plt.plot(x, y, linewidth=line_width)
    plt.scatter(x, y, s=math.pi * (point_radius ** 2.0))

    #plt.xlim(0, max(x) * 1.1)
    # noinspection PyTypeChecker
    #plt.ylim(0, max(y) * 1.1)
    plt.show()
