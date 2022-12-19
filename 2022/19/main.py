#!/usr/bin/env python3

from __future__ import annotations

import sys
import heapq

from typing import Iterable, List, NamedTuple, Set


class Amount(NamedTuple):
    ore: int
    clay: int
    obsidian: int
    geode: int

    def add(self: Amount, other: Amount) -> Amount:
        return Amount(*(x + y for x, y in zip(self, other)))

    def sub(self: Amount, other: Amount) -> Amount:
        return Amount(*(x - y for x, y in zip(self, other)))

    def can_afford(self: Amount, cost: Amount) -> bool:
        return all(x >= y for x, y in zip(self, cost))


class Blueprint(NamedTuple):
    id_: int
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

        yield Blueprint(i + 1, cost_ore, cost_clay, cost_obsidian, cost_geode)


class State(NamedTuple):
    # An upper bound on the number of geodes that can be produced.
    # Negated because Python has a min-heap, but we want to extract the element
    # with the maximum first.
    neg_max_geodes: int

    # How many steps there are left.
    minutes_left: int

    # How many resource-producing robots we currently have of each kind.
    robots: Amount

    # How much resources we currently have of each kind.
    inventory: Amount

    @staticmethod
    def initial() -> State:
        return State(
            neg_max_geodes=0,
            minutes_left=24,
            robots=Amount(1, 0, 0, 0),
            inventory=Amount(0, 0, 0, 0),
        )

    def push_options(self, bp: Blueprint, heap: List[State]) -> None:
        assert self.minutes_left > 0

        candidates = [
            (Amount(1, 0, 0, 0), bp.cost_ore),
            (Amount(0, 1, 0, 0), bp.cost_clay),
            (Amount(0, 0, 1, 0), bp.cost_obsidian),
            (Amount(0, 0, 0, 1), bp.cost_geode),
            # Not producing anything is also an option, sometimes the only one.
            (Amount(0, 0, 0, 0), Amount(0, 0, 0, 0)),
        ]
        for candidate, cost in candidates:
            if not self.inventory.can_afford(cost):
                continue

            # Pay for the robots we built, then add the new inventory produced
            # by the robots we have.
            new_inventory = self.inventory.sub(cost).add(self.robots)

            new_robots = self.robots.add(candidate)

            # In the remaining n minutes, we will produce at most this many
            # geodes, assuming we produce one robot in each of the remaining
            # ticks, and every robot produces one geode. Then we get this
            # arithmetic series, which works out to the value below.
            n = self.minutes_left - 1
            k = new_robots.geode
            geodes_produced_n = n * (2 * k + n - 1) // 2
            max_geodes = new_inventory.geode + geodes_produced_n

            new_state = State(
                neg_max_geodes=-max_geodes,
                minutes_left=n,
                robots=new_robots,
                inventory=new_inventory,
            )

            heapq.heappush(heap, new_state)


def evaluate_blueprint(bp: Blueprint) -> int:
    """
    Return the maximum number of geodes that can be opened in 24 minutes with
    this blueprint.
    """
    heap = [State.initial()]
    states_excluded: Set[State] = set()
    minutes_left = heap[0].minutes_left

    while True:
        state = heapq.heappop(heap)

        if state in states_excluded:
            continue

        # TODO: Figure out why duplicates pile up in the first place.
        states_excluded.add(state)

        if state.minutes_left < minutes_left:
            minutes_left = state.minutes_left
            print(f"{bp.id_=} {minutes_left=} {len(heap)=}")

        if state.minutes_left == 0:
            return state.inventory.geode

        state.push_options(bp, heap)


if __name__ == "__main__":
    total_quality = 0

    for bp in list(load_blueprints()):
        max_geodes = evaluate_blueprint(bp)
        print(f"Blueprint {bp.id_} would open {max_geodes} geodes at most.")
        quality_level = max_geodes * bp.id_
        total_quality += quality_level

    print(total_quality)
