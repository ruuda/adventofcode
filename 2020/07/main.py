#!/usr/bin/env python3

from collections import defaultdict
from typing import Dict, NamedTuple, Set


class Bag(NamedTuple):
    outer_color: str
    inner: Dict[str, int]


def parse(line: str) -> Bag:
    words = line.split()
    c1, c2, bags, contain = words[0:4]
    outer_color = f'{c1} {c2}'
    inner = {}

    words = words[4:]
    while len(words) > 0:
        if words[0] == 'no':
            assert words[1:3] == ['other', 'bags.']
            words = words[3:]
        else:
            n = int(words[0])
            inner_color = f'{words[1]} {words[2]}'
            assert words[3] in ('bags,', 'bag,', 'bags.', 'bag.')
            words = words[4:]
            inner[inner_color] = n

    return Bag(outer_color, inner)


outers: Dict[str, Set[str]] = defaultdict(lambda: set())
bags: Dict[str, Dict[str, int]] = {}

for line in open('input.txt', 'r', encoding='ascii'):
    bag = parse(line.strip())
    bags[bag.outer_color] = bag.inner
    for inner in bag.inner.keys():
        outers[inner].add(bag.outer_color)

frontier = set()
valid = set()
frontier = {'shiny gold'}

while len(frontier) > 0:
    color = frontier.pop()
    new_valid = valid | outers[color]
    frontier = frontier | (new_valid - valid)
    valid = new_valid

print(len(valid))

def num_bags(color: str) -> int:
    return 1 + sum(n * num_bags(inner) for inner, n in bags[color].items())

# Subtract 1, because the outer bag does not count itself.
print(num_bags('shiny gold') - 1)
