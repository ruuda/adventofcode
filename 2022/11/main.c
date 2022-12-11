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
long int n_inspects[MAX_MONKEYS];

// Parse a comma-delimited list of integers into the monkey.
int parse_items(monkey_t* monkey, const char* line) {
  while (1) {
    char* result = memchr(line, ',', strlen(line));
    int* item = monkey->items + monkey->n_items;
    *item = atoi(line);
    monkey->n_items++;

    if (monkey->n_items >= MAX_ITEMS) {
      printf("Too many items.\n");
      return 1;
    }

    if (result == NULL) return 0;

    // There is more after the comma. atoi skips the space,
    // so we don't need to skip it here.
    line = result + 1;
  }
}

// Add an item at the back of the monkeys queue.
int push_item(monkey_t* monkey, int item) {
  monkey->items[monkey->n_items] = item;
  monkey->n_items++;
  if (monkey->n_items > MAX_ITEMS) {
    printf("Too many items.\n");
    return 1;
  }
  return 0;
}

// Pop an item from the front of the monkeys queue.
int pop_item(monkey_t* monkey) {
  if (monkey->n_items == 0) {
    printf("Error: underflow when popping monkey.\n");
    return 0;
  }

  int result = monkey->items[0];
  monkey->n_items--;

  for (int i = 0; i < monkey->n_items; i++) {
    monkey->items[i] = monkey->items[i + 1];
  }

  return result;
}

int process_monkey(int index, int modulus) {
  monkey_t* monkey = monkeys + index;

  while (monkey->n_items > 0) {
    long int worry = pop_item(monkey);
    n_inspects[index]++;
    assert(worry >= 0);

    printf("  Monkey inspects item with a worry level of %ld.\n", worry);

    long int rhs;
    switch (monkey->operation) {
      case '+':
        // We abuse the fact that the text 'old' parses as the number 0 here;
        // in some inputs the formula is 'old * old', not a number on the
        // right-hand side.
        rhs = monkey->rhs > 0 ? monkey->rhs : worry;
        printf("    Worry gets increased by %ld.\n", rhs);
        worry = worry + rhs;
        break;
      case '*':
        rhs = monkey->rhs > 0 ? monkey->rhs : worry;
        printf("    Worry gets multiplied by %ld.\n", rhs);
        worry = worry * rhs;
        break;
      default:
        printf("Invalid operation: %c\n", monkey->operation);
    }

    // For part 1, this division should be enabled.
    // worry = worry / 3;
    // For part 2, we have the modulus instead.
    worry = worry % modulus;

    printf("    After inspection, worry level is %ld.\n", worry);

    if (worry % monkey->divisor == 0) {
      int result = push_item(monkeys + monkey->to_if_divides, worry);
      if (result != 0) return 1;
      printf("    Item thrown to monkey %d.\n", monkey->to_if_divides);
    } else {
      int result = push_item(monkeys + monkey->to_if_not_divides, worry);
      if (result != 0) return 1;
      printf("    Item thrown to monkey %d.\n", monkey->to_if_not_divides);
    }
  }

  return 0;
}

int compare_int_desc(const void* pa, const void* pb) {
  int a = *((int*)pa);
  int b = *((int*)pb);
  return (a < b) - (a > b);
}

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
      if (n_monkeys >= MAX_MONKEYS) {
        printf("Too many monkeys.\n");
        return 1;
      }
    } else if (MATCHES(line, "  Starting items: ")) {
      size_t offset = strlen("  Starting items: ");
      monkey->n_items = 0;
      int result = parse_items(monkey, line + offset);
      if (result != 0) {
        printf("Failed to parse monkey %ld.\n", n_monkeys);
        return 1;
      }
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

  free(line);

  for (int i = 0; i < n_monkeys; i++) {
    monkey_t* monkey = monkeys + i;

    printf(
      "Monkey %d: n_items=%d, op=%c, rhs=%d, divisor=%d, to_t=%d, to_f=%d\n",
      i, monkey->n_items, monkey->operation, monkey->rhs, monkey->divisor,
      monkey->to_if_divides, monkey->to_if_not_divides
    );
    for (int j = 0; j < monkey->n_items; j++) {
      printf("  item: %d\n", monkey->items[j]);
    }
  }

  // For part 1, this should be 20, not 10000.
  size_t n_rounds = 10000;

  // For part 2, the worry level keeps on growing, it overflows int. But note
  // that the monkeys only care about the worry level *modulo* their own
  // divisor. So we can work modulo the product of all their divisors, that way
  // they are still guaranteed to find the same result of the modulo operation.
  // And the product of a few monkey's divisors should fit just fine.
  int modulus = 1;
  for (int i = 0; i < n_monkeys; i++) {
    modulus *= monkeys[i].divisor;
  }
  printf("Modulus: %d, %d\n", modulus, modulus * modulus);

  for (int round = 0; round < n_rounds; round++) {
    for (int i = 0; i < n_monkeys; i++) {
      printf("Round %d, processing monkey %d:\n", round, i);
      int result = process_monkey(i, modulus);
      if (result != 0) return 1;
    }
  }

  // Find the two most active monkeys.
  qsort(n_inspects, n_monkeys, sizeof(long int), compare_int_desc);
  for (int i = 0; i < n_monkeys; i++) {
    printf("Activity %d: %ld\n", i, n_inspects[i]);
  }
  printf("Answer part 1: %ld\n", n_inspects[0] * n_inspects[1]);

  return 0;
}
