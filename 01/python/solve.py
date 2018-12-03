#!/usr/bin/env python3
import sys
import itertools

shifts = [int(line) for line in sys.stdin]

def find_final_frequency(shifts):
    return sum(shifts)

def find_first_repeated_frequency(shifts):
    seen = set([0])
    for frequency in itertools.accumulate(shifts):
        if frequency in seen:
            return frequency
        seen.add(frequency)

print("""\
The resulting frequency after all shifts is {0}.
The first repeated freqency is {1}.\
""".format(find_final_frequency(shifts),
           find_first_repeated_frequency(itertools.cycle(shifts)))
)
