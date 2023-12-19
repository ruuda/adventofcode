import std/enumerate
import std/strformat
import std/strscans
import std/strutils
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

  PartRange = object
    # All XMAS part ratings in the range satisfy >= lower.
    lower: array[4, int]
    # All XMAS part ratings in the range satisfy < upper.
    upper: array[4, int]

  # For part 2, an edge incoming to a node, with where it came from, and the
  # condition that needs to hold to traverse the edge.
  Edge = object
    prop: char
    cond: char
    rhs: int
    src: string

func getProp(part: Part, prop: char): int =
  return case prop:
    of 'x': part.x
    of 'm': part.m
    of 'a': part.a
    of 's': part.s
    else: raise newException(Defect, "Unexpected property.")

func flip(r: Rule): Rule =
  if r.cond == '>':
    Rule(prop: r.prop, cond: '<', rhs: r.rhs + 1, next: r.next)
  else:
    Rule(prop: r.prop, cond: '>', rhs: r.rhs - 1, next: r.next)

# Cut the range in half as specified by the edge, return the half that satisfies
# the condition.
func cut(r: PartRange, edge: Edge): PartRange =
  var i = case edge.prop:
    of 'x': 0
    of 'm': 1
    of 'a': 2
    of 's': 3
    else: raise newException(Defect, "Unexpected property.")

  if edge.cond == '>':
    var k = [0, 0, 0, 0]
    k[i] = edge.rhs + 1
    var lower = [
      max(k[0], r.lower[0]),
      max(k[1], r.lower[1]),
      max(k[2], r.lower[2]),
      max(k[3], r.lower[3]),
    ]
    return PartRange(lower: lower, upper: r.upper)
  else:
    var k = [4001, 4001, 4001, 4001]
    k[i] = edge.rhs
    var upper = [
      min(k[0], r.upper[0]),
      min(k[1], r.upper[1]),
      min(k[2], r.upper[2]),
      min(k[3], r.upper[3]),
    ]
    return PartRange(lower: r.lower, upper: upper)

func isEmpty(r: PartRange): bool =
  for i in [0, 1, 2, 3]:
    if r.lower[i] >= r.upper[i]:
      return true
  return false

func `$`(r: PartRange): string =
  fmt"[{r.lower[0]:4}..{r.upper[0]:4} x  {r.lower[1]:4}..{r.upper[1]:4} m  {r.lower[2]:4}..{r.upper[2]:4} a  {r.lower[3]:4}..{r.upper[3]:4} s]"

func size(r: PartRange): uint64 =
  var res: uint64 = 1
  for i in [0, 1, 2, 3]:
    res = res * uint64(r.upper[i] - r.lower[i])
  return res

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

func collectEdges(workflows: Table[string, Workflow]): Table[string, seq[Edge]] =
  var edges = initTable[string, seq[Edge]]()
  for src_base, wf in workflows:
    for (i, rule) in enumerate(wf.rules):
      var src = fmt"{src_base}{i}"
      var dst_t = fmt"{rule.next}0"
      var dst_f = if i + 1 == wf.rules.len: fmt"{wf.final}0" else: fmt"{src_base}{i+1}"
      var rule_f = rule.flip
      var edge_t = Edge(prop: rule.prop, cond: rule.cond, rhs: rule.rhs, src: src)
      var edge_f = Edge(prop: rule.prop, cond: rule_f.cond, rhs: rule_f.rhs, src: src)
      edges.mgetOrPut(dst_t, @[]).add(edge_t)
      edges.mgetOrPut(dst_f, @[]).add(edge_f)
  return edges

proc solveSymbolic(edges: Table[string, seq[Edge]]): uint64 =
  var open = initTable[string, seq[PartRange]]()
  var inputs: seq[PartRange] = @[]
  var full = PartRange(lower: [1, 1, 1, 1], upper: [4001, 4001, 4001, 4001])
  open["A0"] = @[full]

  while open.len > 0:
    var newOpen = initTable[string, seq[PartRange]]()
    for node, states in open:
      if node == "in0":
        inputs.add(open["in0"])
        continue

      echo fmt"{node}:"
      for inEdge in edges[node]:
        for outState in states:
          var inState = outState.cut(inEdge)
          echo fmt"  <- {inState} [{inEdge.src}]"
          if not inState.isEmpty:
            newOpen.mgetOrPut(inEdge.src, @[]).add(inState)

    open = newOpen

  echo "All input ranges:"
  var res: uint64 = 0
  for r in inputs:
    echo r
    res += r.size

  return res

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

const debug_print_input = false
if debug_print_input:
  for name, wf in workflows:
    echo fmt"{name}:"
    for rule in wf.rules:
      echo fmt"  {rule.prop} {rule.cond} {rule.rhs}: jmp {rule.next}"
    echo fmt"  else: jmp {wf.final}"

var part1Answer: uint64 = 0
for part in parts:
  part1Answer += part.evalPart(workflows)
echo fmt"Part 1: {part1Answer}"

var edges = collectEdges(workflows)
for dst, srcs in edges:
  for src in srcs:
    echo fmt"{dst:5} <--[ {src.prop} {src.cond} {src.rhs:4} ]-- {src.src}"

var part2Answer = solveSymbolic(edges)
echo fmt"Part 2: {part2Answer}"
