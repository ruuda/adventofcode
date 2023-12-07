import std/algorithm
import std/enumerate
import std/sequtils
import std/strscans
import std/strutils
import std/sugar

let part = 2

func charCount(target: char, s: string): int =
  filter(s, c => c == target).len

proc charWorth(target: char): int =
  if part == 1:
    "23456789TJQKA".find(target)
  else:
    "J23456789TQKA".find(target)

func cmpSeq(xs: seq[int], ys: seq[int]): int =
  for (x, y) in zip(xs, ys):
    if x == y:
      continue
    else:
      return y - x

# Deconstruct the hand into a sequence of int so the sequence ordering matches
# the hand order for part 1.
func deconstructHand1(x: string): seq[int] =
  sorted(map(x, c => charCount(c, x)), system.cmp[int], Descending)

func deconstructHand2(x: string): seq[int] =
  var counts = deconstructHand1(x.replace("J", ""))

  if counts.len == 0:
    # The best we can get is 5 of a kind.
    return @[5, 5, 5, 5, 5]

  while counts.len < 5:
    for i in countup(0, counts[0] - 1):
      counts[i] += 1
    counts.insert(counts[0], 0)

  counts

proc cmpHand(x, y: string): int =
  let cx = if part == 1: deconstructHand1(x) else: deconstructHand2(x)
  let cy = if part == 1: deconstructHand1(y) else: deconstructHand2(y)
  let d = cmpSeq(cx, cy)
  if d == 0:
    cmpSeq(map(x, charWorth), map(y, charWorth))
  else:
    d

type Hand = object
  hand: string
  bid: int

func initHand(hand: string, bid: int): Hand =
  result.hand = hand
  result.bid = bid

var hands: seq[Hand] = @[]

for line in "input.txt".lines:
  var hand: string
  var bid: int

  if not scanf(line, "$+ $i", hand, bid):
    continue

  hands.add(initHand(hand, bid))
  echo hand, " -> ", deconstructHand2(hand)

var winnings_total = 0

for (i, hand) in enumerate(sorted(hands, (h1, h2) => cmpHand(h1.hand, h2.hand))):
  let rank = hands.len - i
  let winnings = hand.bid * rank
  echo rank, " ", hand
  winnings_total += winnings

echo winnings_total
