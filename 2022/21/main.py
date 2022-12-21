#!/usr/bin/env python3

from typing import Dict, NamedTuple 
import sys

class Thunk(NamedTuple):
    lhs: str
    op: str
    rhs: str


def load_expressions() -> Dict[str, Thunk | int]:
    result: Dict[str, Thunk | int] = {}
    for line in open(sys.argv[1], "r", encoding="utf-8"):
        parts = line.strip().split(" ")
        key = parts[0][:-1]  # Cut off the trailing colon
        if len(parts) > 2:
            result[key] = Thunk(*parts[1:])
        else:
            result[key] = int(parts[1])

    return result


def eval(heap: Dict[str, Thunk | int], key: str) -> int:
    value = heap[key]
    if isinstance(value, int):
        return value
    else:
        lhs = eval(heap, value.lhs)
        rhs = eval(heap, value.rhs)
        match value.op:
            case '+':
                result = lhs + rhs
            case '-':
                result = lhs - rhs
            case '*':
                result = lhs * rhs
            case '/':
                result = lhs // rhs
            case _:
                raise Exception("Invalid operation: " + value.op)
        heap[key] = result
        return result


if __name__ == "__main__":
    heap = load_expressions()
    print(eval(heap, "root"))
