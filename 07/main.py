#!/usr/bin/env python3

from typing import Dict, NamedTuple


class Bag(NamedTuple):
    color: str
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


for line in open('input.txt', 'r', encoding='ascii'):
    b = parse(line.strip())
    print(b)
