#!/usr/bin/env python3

import re
from itertools import groupby
from sys import stdin

def parse(line):
    steps = re.findall(r"step (\w+)", line, re.I)
    return tuple(steps)
    
def group_deps(dep_pairs):
    get_dep = lambda pair: pair[0]
    get_step = lambda pair: pair[1]

    all_steps = set(map(get_dep, dep_pairs)) | set(map(get_step, dep_pairs))

    sorted_pairs = sorted(dep_pairs, key=get_step)
    grouped = {}

    for step, entries in groupby(sorted_pairs, get_step):
        deps = map(get_dep, entries)
        grouped[step] = set(deps)

    for step in all_steps - set(grouped.keys()):
        grouped[step] = set()

    return grouped

def sequence(dependency_map):
    all_steps = set(dependency_map.keys())
    completed = set()
    order = []

    while completed < all_steps:
        ready = sorted(step for step, deps
                       in dependency_map.items()
                       if step not in completed and deps <= completed)
        completed.add(ready[0])
        order.append(ready[0])

    return order
    

if __name__ == "__main__":
    dependency_map = group_deps([parse(line) for line in stdin])
    print("".join(sequence(dependency_map)))
