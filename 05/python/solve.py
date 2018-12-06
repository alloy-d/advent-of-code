#!/usr/bin/env python3

import math
from functools import reduce
from itertools import zip_longest
from operator import concat
from sys import stdin

reacting_pairs = set(reduce(concat,
                            ([(c, c.upper()), (c.upper(), c)]
                             for c in map(chr, range(ord('a'), ord('z')+1)))))

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

def run(input):
    print("""\
    In the fully reacted polymer, {0} units remain.
    """.format(len(react_until_stable(list(input)))))

if __name__ == "__main__":
    run(stdin.readline().rstrip())
