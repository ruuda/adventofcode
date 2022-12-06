#!/usr/bin/env python3

def index_first_n_different(n: int, datastream: str) -> int:
    for i in range(n, len(datastream)):
        if len(set(datastream[i - n:i])) == n:
            return i

    raise Exception("No four consecutive characters are different.")

datastream = open("input.txt", "r", encoding="utf-8").read().strip()
print("Part 1 answer:", index_first_n_different(4, datastream))
print("Part 2 answer:", index_first_n_different(14, datastream))
