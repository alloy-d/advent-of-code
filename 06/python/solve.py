#!/usr/bin/env python3

from functools import reduce
from itertools import groupby
from operator import concat
from sys import stdin

def finite_area(coords):
    x_coords = list(map(lambda c: c[0], coords))
    y_coords = list(map(lambda c: c[1], coords))

    return {
        "x": range(min(x_coords), max(x_coords)+1),
        "y": range(min(y_coords), max(y_coords)+1),
    }

def coords_on_edge(coords):
    """Returns the coordinate pairs that represent points
    on the outside edges of all points.

    These points will be the ones with infinite areas."""
    x_coords = list(map(lambda c: c[0], coords))
    y_coords = list(map(lambda c: c[1], coords))

    x_edges = set([min(x_coords), max(x_coords)])
    y_edges = set([min(y_coords), max(y_coords)])

    return filter(lambda c: c[0] in x_edges or c[1] in y_edges, coords)

def shifts_for_distance(distance):
    for i in range(distance+1):
        dx = i
        dy = distance - i

        yield (dx, dy)
        if dx != 0:
            yield (-dx, dy)
        if dy != 0:
            yield (dx, -dy)
        if dx != 0 and dy != 0:
            yield (-dx, -dy)

def coords_within_distance(distance, start):
    (x, y) = start
    for (dx, dy) in shifts_for_distance(distance):
        yield (x+dx, y+dy)

def produce_areas(coords):
    field = finite_area(coords)
    infinite = set()
    distance = -1
    areas = {}
    enlarged = set(coords)

    while enlarged > infinite:
        print(f"distance: {distance}, enlarged: {enlarged}")
        distance += 1
        enlargements = {}

        for center in coords:
            for coord in coords_within_distance(distance, center):
                if coord not in areas:
                    areas[coord] = (distance, center)
                    enlargements[center] = 1 + enlargements.get(center, 0)

                    if coord[0] not in field["x"] or coord[1] not in field["y"]:
                        infinite.add(center)

                else:
                    (shortest_distance, closest_coord) = areas[coord]
                    if shortest_distance == distance:
                        # The known nearest point is no closer than the
                        # one we're looking at now, which means that this
                        # point has no single closest point.
                        areas[coord] = (-1, None)
                        enlargements[closest_coord] -= 1

        enlarged = set(coord for coord in enlargements if enlargements[coord] > 0)
        unenlarged = set(coord for coord in enlargements if enlargements[coord] == 0)

        infinite.difference_update(unenlarged)

    return {
        "areas": areas,
        "infinite": infinite,
    }

def largest_area(results):
    key = lambda area: area[1]
    infinite = results["infinite"]
    print(f"infinite: {infinite}")
    useful_coords = filter(lambda v: v[1] != None and v[1] not in infinite,
                           results["areas"].values())
    grouped = groupby(sorted(useful_coords, key=key), key)
    sized = map(lambda group: (group[0], len(list(group[1]))), grouped)
    return max(sized, key=lambda group: group[1])

def solve():
    coords = [tuple(map(int, line.split(", "))) for line in stdin]
    print(largest_area(produce_areas(coords)))

if __name__ == "__main__":
    solve()
