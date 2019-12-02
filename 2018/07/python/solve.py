#!/usr/bin/env python3

import re
from itertools import groupby, repeat
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
    
def time_for_task(task):
    return 61 + ord(task) - ord("A")

class Worker:
    def __init__(self):
        self.free()

    def free(self):
        self.task = None
        self.completion_time = None

def sequence_with_workers(depedency_map, num_workers):
    all_steps = set(dependency_map.keys())
    completed = set()
    now = 0
    workers = []
    for i in range(num_workers):
        workers.append(Worker())

    while completed < all_steps:
        for worker in workers:
            if worker.completion_time == now:
                completed.add(worker.task)
                worker.free()

        ongoing = set(w.task for w in workers if w.task != None)
        ready_workers = [w for w in workers if w.task == None]
        ready_tasks = sorted(step for step, deps
                             in dependency_map.items()
                             if step not in completed
                             and step not in ongoing
                             and deps <= completed)

        for worker, task in zip(ready_workers, ready_tasks):
            worker.task = task
            worker.completion_time = now + time_for_task(task)

        now += 1

    return now - 1

if __name__ == "__main__":
    dependency_map = group_deps([parse(line) for line in stdin])
    single_worker_sequence = "".join(sequence(dependency_map))
    multiworker_time = sequence_with_workers(dependency_map, 5)

    print(f"""\
    The sequence for a single worker is {single_worker_sequence}.

    With 5 workers, it takes {multiworker_time} seconds.\
    """)
