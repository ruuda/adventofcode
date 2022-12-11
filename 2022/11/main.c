#include "assert.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#define MAX_MONKEYS 64
#define MAX_ITEMS 128
#define MATCHES(x, y) strncmp(x, y, strlen(y)) == 0

typedef struct {
  int items[MAX_ITEMS];
  int n_items;
  char operation;
  int rhs;
  int divisor;
  int to_if_divides;
  int to_if_not_divides;
} monkey_t;

monkey_t monkeys[MAX_MONKEYS];

int main(int argc, const char** argv) {
  assert(argc == 2);
  FILE* f = fopen(argv[1], "r");
  assert(f != NULL);

  size_t n_monkeys = 0;

  char* line = NULL;
  size_t len = 0;
  monkey_t* monkey = monkeys;

  while (1) {
    ssize_t nread = getline(&line, &len, f);
    if (nread <= 0) break;

    // Skip empty lines.
    if (nread == 1) continue;

    if (MATCHES(line, "Monkey")) {
      // We don't check the monkey number, we assume it's ordered.
      monkey = &monkeys[n_monkeys];
      n_monkeys++;
      if (n_monkeys > MAX_MONKEYS) {
        printf("Too many monkeys.\n");
        return 1;
      }
    } else if (MATCHES(line, "  Starting items: ")) {
      // TODO: Read items.
      monkey->n_items = 1;
    } else if (MATCHES(line, "  Operation: new = old ")) {
      size_t offset = strlen("  operation: new = old ");
      monkey->operation = line[offset];
      monkey->rhs = atoi(line + offset + 1);
    } else if (MATCHES(line, "  Test: divisible by ")) {
      size_t offset = strlen("  Test: divisible by ");
      monkey->divisor = atoi(line + offset);
    } else if (MATCHES(line, "    If true: throw to monkey ")) {
      size_t offset = strlen("    If true: throw to monkey ");
      monkey->to_if_divides = atoi(line + offset);
    } else if (MATCHES(line, "    If false: throw to monkey ")) {
      size_t offset = strlen("    If false: throw to monkey ");
      monkey->to_if_not_divides = atoi(line + offset);
    } else {
      printf("Unknown directive: %s", line);
      return 1;
    }
  }

  for (int i = 0; i < n_monkeys; i++) {
    monkey_t* monkey = monkeys + i;

    printf(
      "Monkey %d: n_items=%d, op=%c, rhs=%d, divisor=%d, to_t=%d, to_f=%d\n",
      i, monkey->n_items, monkey->operation, monkey->rhs, monkey->divisor,
      monkey->to_if_divides, monkey->to_if_not_divides
    );
  }

  free(line);

  return 0;
}
