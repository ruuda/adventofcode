#!/usr/bin/env python3

import sys

from typing import Iterable, NamedTuple


class Amount(NamedTuple):
    ore: int
    clay: int
    obsidian: int
    geode: int


class Blueprint(NamedTuple):
    cost_ore: Amount
    cost_clay: Amount
    cost_obsidian: Amount
    cost_geode: Amount


def load_blueprints() -> Iterable[Blueprint]:
    for i, line in enumerate(open(sys.argv[1], "r", encoding="utf-8")):
        parts = line.split()
        assert i + 1 == int(parts[1][:-1])

        assert parts[3] == "ore" and parts[7] == "ore."
        cost_ore = Amount(ore=int(parts[6]), clay=0, obsidian=0, geode=0)

        assert parts[9] == "clay" and parts[13] == "ore."
        cost_clay = Amount(ore=int(parts[12]), clay=0, obsidian=0, geode=0)

        assert parts[15] == "obsidian" and parts[19] == "ore" and parts[22] == "clay."
        cost_obsidian = Amount(ore=int(parts[18]), clay=int(parts[21]), obsidian=0, geode=0)

        assert parts[24] == "geode" and parts[28] == "ore" and parts[31] == "obsidian."
        cost_geode = Amount(ore=int(parts[27]), clay=0, obsidian=int(parts[30]), geode=0)

        yield Blueprint(cost_ore, cost_clay, cost_obsidian, cost_geode)


if __name__ == "__main__":
    for bp in load_blueprints():
        print(bp)

