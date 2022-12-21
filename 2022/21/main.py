#!/usr/bin/env python3

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Optional
import sys

@dataclass(frozen=True)
class Expr:
    def simplify(self) -> Expr:
        raise NotImplementedError


@dataclass(frozen=True)
class Var(Expr):
    def simplify(self) -> Var:
        return self


@dataclass(frozen=True)
class Val(Expr):
    value: int

    def simplify(self) -> Val:
        return self


@dataclass(frozen=True)
class Op(Expr):
    lhs: Expr
    op: str
    rhs: Expr

    def simplify(self) -> Expr:
        lhs = self.lhs.simplify()
        rhs = self.rhs.simplify()

        if isinstance(lhs, Val) and isinstance(rhs, Val):
            match self.op:
                case "+":
                    return Val(lhs.value + rhs.value)
                case "-":
                    return Val(lhs.value - rhs.value)
                case "*":
                    return Val(lhs.value * rhs.value)
                case "/":
                    q = lhs.value // rhs.value
                    assert q * rhs.value == lhs.value
                    return Val(q)
                case _:
                    raise ValueError

        elif isinstance(lhs, Val):
            # Push values to the right as much as possible. We can do this for
            # all commutative operators.
            if self.op in ("/", "-"):
                return Op(lhs, self.op, rhs)
            else:
                return Op(rhs, self.op, lhs).simplify()

        return Op(lhs, self.op, rhs)


@dataclass(frozen=True)
class Eq(Expr):
    lhs: Expr
    rhs: Expr

    def simplify(self) -> Expr:
        lhs = self.lhs.simplify()
        rhs = self.rhs.simplify()

        # By default, push all complexity into the right-hand side and
        # simplify on the left. If we are all the way simplified on the left
        # already, then go the other way around.
        new_rhs: Expr
        inverse = {
            "+": "-",
            "-": "+",
            "*": "/",
            "/": "*",
        }

        if isinstance(lhs, Var):
            # If lhs is a variable, then we are done.
            return Eq(lhs, rhs)

        elif isinstance(lhs, Op):
            # We have an expression of the form "a <op> b = c",
            # solving for a, we get "a = c <inv-op> b".
            new_rhs = Op(rhs, inverse[lhs.op], lhs.rhs)
            return Eq(lhs.lhs, new_rhs).simplify()

        elif isinstance(lhs, Val) and isinstance(rhs, Op):
            # We have an expression of the form "c1 = a <op> b",
            # solving for a, we get "a = c1 <inv-op> b".
            new_rhs = Op(lhs, inverse[rhs.op], rhs.rhs)
            return Eq(rhs.lhs, new_rhs).simplify()

        else:
            return Eq(lhs, rhs)


@dataclass(frozen=True)
class Thunk:
    lhs: str
    op: str
    rhs: str


def load_expressions() -> Dict[str, Thunk | int | None]:
    result: Dict[str, Thunk | int | None] = {}

    for line in open(sys.argv[1], "r", encoding="utf-8"):
        parts = line.strip().split(" ")
        key = parts[0][:-1]  # Cut off the trailing colon
        if len(parts) > 2:
            result[key] = Thunk(parts[1], parts[2], parts[3])
        else:
            result[key] = int(parts[1])

    return result


def eval(heap: Dict[str, Thunk | int | None], key: str) -> Expr:
    value = heap[key]

    if isinstance(value, int):
        return Val(value)

    elif value is None:
        return Var()

    elif isinstance(value, Thunk):
        lhs = eval(heap, value.lhs)
        rhs = eval(heap, value.rhs)
        if value.op == "=":
            return Eq(lhs, rhs)
        else:
            return Op(lhs, value.op, rhs)


if __name__ == "__main__":
    heap: Dict[str, int | Thunk | None] = load_expressions()
    root_expr = eval(heap, "root")
    print("Part 1 answer: ", root_expr.simplify())

    heap = load_expressions()
    root = heap["root"]
    assert isinstance(root, Thunk)
    heap["root"] = Thunk(root.lhs, "=", root.rhs)
    heap["humn"] = None
    root_expr = eval(heap, "root")
    print("Part 2 answer: ", root_expr.simplify())
