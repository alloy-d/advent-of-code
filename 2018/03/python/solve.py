#!/usr/bin/env python3

import re
from functools import reduce
from itertools import repeat
from sys import stdin

class Claim:
    parser = re.compile(r"""\#(?P<id>\d+)\ @
                            \ (?P<top>\d+),(?P<left>\d+):
                            \ (?P<height>\d+)x(?P<width>\d+)
                        """, re.X)

    def __init__(self, string):
        self.string = string
        for field, value in self.parser.search(string).groupdict().items():
            setattr(self, field, int(value))
    
    @property
    def inches(self):
        return ((x,y) for x in range(self.left, self.left + self.width)
                      for y in range(self.top, self.top + self.height))

def determine_extents(claims):
    return (
        max(map(lambda claim: claim.left + claim.width, claims)),
        max(map(lambda claim: claim.top + claim.height, claims))
    )

def claim_counts(claims):
    width, height = determine_extents(claims)
    counters = list(repeat(0, width*height))
    for claim in claims:
        for (w, h) in claim.inches:
            counters[w + width*h] += 1
    return counters

def find_isolated_claim(claims):
    claimed = list(map(lambda claim: set(claim.inches), claims))
    for i in range(0, len(claims)):
        other_claimed_inches = set().union(*claimed[:i], *claimed[i+1:])
        if other_claimed_inches.isdisjoint(claimed[i]):
            return claims[i].id

claims = [Claim(line) for line in stdin]

print("""\
{0} inches are part of multiple claims.

The only isolated claim is #{1}.\
""".format(len(list(filter(lambda n: n > 1, claim_counts(claims)))),
           find_isolated_claim(claims)))
