#!/usr/bin/env python3

def index_first_four_different(datastream: str) -> int:
    for i in range(4, len(datastream)):
        if len(set(datastream[i - 4:i])) == 4:
            return i

    raise Exception("No four consecutive characters are different.")

datastream = open("input.txt", "r", encoding="utf-8").read().strip()
print(index_first_four_different(datastream))
