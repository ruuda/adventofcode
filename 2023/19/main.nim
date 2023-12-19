import std/sequtils
import std/strscans
import std/strutils
import std/strformat
import std/tables

type
  Rule = object
    prop: char
    cond: char
    rhs: int
    next: string

  Workflow = object
    rules: seq[Rule]
    final: string

  Part = object
    x, m, a, s: int

func getProp(part: Part, prop: char): int =
  return case prop:
    of 'x': part.x
    of 'm': part.m
    of 'a': part.a
    of 's': part.s
    else: raise newException(Defect, "Unexpected property.")

proc parseWorkflow(line: string, workflows: var Table[string, Workflow]) =
  var wf: Workflow
  var name: string
  var input = line

  if not scanf(input, "$w{", name):
      raise newException(Defect, "Unexpected input.")

  input = input[input.find("{") + 1..^1]

  while true:
    var prop, cond: char
    var rhs: int
    var next: string

    if scanf(input, "$c$c$i:$w", prop, cond, rhs, next):
      wf.rules.add(Rule(prop: prop, cond: cond, rhs: rhs, next: next))
      input = input[input.find(",") + 1..^1]
    else:
      wf.final = input[0..^2]
      workflows[name] = wf
      break

func parsePart(line: string): Part =
  var x, m, a, s: int
  if not scanf(line, "{x=$i,m=$i,a=$i,s=$i}", x, m, a, s):
      raise newException(Defect, "Unexpected input.")
  return Part(x: x, m: m, a: a, s: s)

func evalPart(part: Part, workflows: Table[string, Workflow]): uint64 =
  var loc = "in"
  while true:
    if loc == "A":
      return uint64(part.x + part.m + part.a + part.s)
    if loc == "R":
      return 0

    var wf = workflows[loc]
    var ok = false
    for rule in wf.rules:
      var p = part.getProp(rule.prop)
      ok = case rule.cond:
        of '<': p < rule.rhs
        of '>': p > rule.rhs
        else: raise newException(Defect, "Unexpected condition.")
      if ok:
        loc = rule.next
        break

    if not ok:
      loc = wf.final

var workflows = initTable[string, Workflow]()
var parts: seq[Part] = @[]

# We begin parsing workflows.
var mode = 'W'
for line in "input.txt".lines:
  if mode == 'W':
    if line == "":
      mode = 'P'
      continue
    parseWorkflow(line, workflows)

  if mode == 'P':
    if line == "":
      break
    parts.add(parsePart(line))

for name, wf in workflows:
  echo fmt"{name}:"
  for rule in wf.rules:
    echo fmt"  {rule.prop} {rule.cond} {rule.rhs}: jmp {rule.next}"
  echo fmt"  else: jmp {wf.final}"

var part1Answer: uint64 = 0
for part in parts:
  part1Answer += part.evalPart(workflows)

echo fmt"Part 1: {part1Answer}"
