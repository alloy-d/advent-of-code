#!/usr/bin/env python3

import itertools
from sys import stdin

ids = [id.strip() for id in stdin.readlines()]

def repetition_counts(id):
    return set(len(list(group))
               for char, group
               in itertools.groupby(sorted(id)))
    
def checksum(ids):
    counts = list(map(repetition_counts, ids))
    return (sum(1 for count in counts if 2 in count)
          * sum(1 for count in counts if 3 in count))

def omit(string, pos):
    return string[:pos] + string[pos+1:]

def find_deviant_pos(ids):
    count = len(ids)
    for pos in range(len(ids[0])):
        if len(set(omit(id, pos) for id in ids)) < count:
            return pos

def find_common_part(ids, deviant_pos):
    seen = set()
    for part in (omit(id, deviant_pos) for id in ids):
        if part in seen:
            return part
        seen.add(part)

deviant_pos = find_deviant_pos(ids)
print("""\
The checksum is {0}.

The position at which two IDs deviate by a single character is {1}.
The part of the IDs that doesn't deviate is {2}.
""".format(checksum(ids), deviant_pos, find_common_part(ids, deviant_pos)))
