#!/usr/bin/env python3

import math
import multiprocessing.pool
from functools import reduce
from itertools import zip_longest
from operator import concat
from sys import stdin

unit_types = list(map(chr, range(ord('a'), ord('z')+1)))
reacting_pairs = set(reduce(concat,
                            ([(t, t.upper()), (t.upper(), t)]
                             for t in unit_types)))

def react_once(units):
    accum = []
    progression = zip_longest(units, units[1:])
    for pair in progression:
        if pair in reacting_pairs:
            next(progression, None)
            continue
        accum.append(pair[0])
    return accum

def react_until_stable(units):
    previous_length = math.inf
    current_length = len(units)
    while current_length < previous_length:
        units = react_once(units)
        previous_length = current_length
        current_length = len(units)
    return units

def remove_unit_type_and_react(type, units):
    removed = set([type.lower(), type.upper()])
    return react_until_stable(list(filter(lambda c: c not in removed, units)))

def run_inhibition_test_job(job):
    (type_to_remove, units) = job
    return (type_to_remove,
            len(remove_unit_type_and_react(type_to_remove, units)))

def find_inhibiting_unit_type(units):
    jobs = map(lambda u: (u, units), unit_types)
    with multiprocessing.pool.Pool() as pool:
        results = pool.map(run_inhibition_test_job, jobs)
    return sorted(results, key=lambda r: r[1])[0]

def run(input):
    units = list(input)
    units_after_reacting = react_until_stable(units)
    (inhibiting_unit, size_without_inhibitor) =\
        find_inhibiting_unit_type(units)
    print(F"""\
In the fully reacted polymer, {len(units_after_reacting)} units remain.

Unit '{inhibiting_unit}' inhibits the polymer.
Removing it results in a polymer of size {size_without_inhibitor}.""")

if __name__ == "__main__":
    run(stdin.readline().rstrip())
