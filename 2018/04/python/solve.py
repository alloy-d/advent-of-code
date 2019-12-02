#!/usr/bin/env python3

import operator
import re
from functools import reduce
from itertools import groupby, repeat
from sys import stdin

guard_id_re = re.compile(r'#(?P<id>\d+)')
minute_re = re.compile(r':(?P<minute>\d{2})')
def parse_entry(entry):
    if "begins shift" in entry:
        return ('guard',
                int(guard_id_re.search(entry).group('id')))
    else:
        minute = int(minute_re.search(entry).group('minute'))
        if "falls asleep" in entry:
            return ('sleep', minute)
        else:
            return ('wake', minute)

def summarize_log(entries):
    log = list(map(parse_entry, entries))
    pairs = zip(log, log[1:])
    
    def summarize(pair):
        (a,b) = pair
        (event_a, value_a) = a
        (event_b, value_b) = b

        if event_a == 'guard':
            return a
        elif event_a == 'sleep' and event_b == 'wake':
            return ('asleep', range(value_a, value_b))

    return filter(None, map(summarize, pairs))

def summarize_by_night(summaries):
    nights = []
    wip = None
    for type, value in summaries:
        if type == 'guard':
            nights.append(wip)
            wip = (value, [])
        else:
            wip[1].append(value)
    nights.append(wip)

    return nights[1:]

def summarize_sleep_ranges_by_id(nights):
    key = lambda pair: pair[0]
    grouped = groupby(sorted(nights, key=key), key=key)

    def summarizer(pair):
        (id, entries) = pair
        return (id, reduce(operator.concat,
                           map(lambda e: e[1], entries)))

    by_id = map(summarizer, grouped)
    return by_id

def find_sleepiest(summarized_ranges):
    def counter(pair):
        (id, ranges) = pair
        minutes = list(repeat(0, 60))
        for r in ranges:
            for i in r:
                minutes[i] += 1
        return (id, minutes)

    counts = map(counter, summarized_ranges)

    by_minutes = sorted(counts,
                        key=lambda c: max(c[1]),
                        reverse=True)
    
    sleepiest = by_minutes[0]
    most_slept_minute = (0, 0)
    for minute in range(60):
        if sleepiest[1][minute] > most_slept_minute[1]:
            most_slept_minute = (minute, sleepiest[1][minute])
        
    return [sleepiest[0], most_slept_minute[0]]

records = sorted(line.rstrip() for line in stdin)

for line in find_sleepiest(summarize_sleep_ranges_by_id(summarize_by_night(summarize_log(records)))):
    print(line)
