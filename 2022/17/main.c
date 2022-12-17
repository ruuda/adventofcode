#include "assert.h"
#include "memory.h"
#include "stdio.h"
#include "stdlib.h"

typedef struct shape {
  const char* blob;
  // The coordinate system is x from left to right, y from bottom to top.
  int w;
  int h;
} shape_t;

int field_width = 7;

int fits(
  const char* field,
  const shape_t* shape,
  int x,
  int y
) {
  if (y < 0) {
    // It would fall off the bottom.
    return 0;
  }
  if (x < 0 || x + shape->w > field_width) {
    // It would fall off the side.
    return 0;
  }

  for (int i = 0; i < shape->h; i++) {
    for (int j = 0; j < shape->w; j++) {
      char shape_at = shape->blob[i * shape->w + j];
      assert(shape_at == '#' || shape_at == '.');
      if (shape_at == '.') continue;

      int py = y + i;
      int px = x + j;
      char field_at = field[py * field_width + px];
      if (field_at == 0) continue;

      // If we get here, the cell is blocked.
      return 0;
    }
  }

  return 1;
}

void mark(
  char* field,
  const shape_t* shape,
  int x,
  int y,
  char marker
) {
  for (int i = 0; i < shape->h; i++) {
    for (int j = 0; j < shape->w; j++) {
      char shape_at = shape->blob[i * shape->w + j];
      assert(shape_at == '#' || shape_at == '.');
      if (shape_at == '.') continue;

      int py = y + i;
      int px = x + j;
      field[py * field_width + px] = marker;
    }
  }
}

int max(int x, int y) {
  return x > y ? x : y;
}

// Print a slice of the tower, for the half-open y-range [ymin, ymax).
void print_tower(const char* field, int ymin, int ymax) {
  for (int j = ymax - 1; j >= ymin; j--) {
    for (int i = 0; i < field_width; i++) {
      char at_field = field[j * field_width + i];
      if (at_field == 0) {
        printf(".");
      } else {
        printf("%c", at_field);
      }
    }
    printf("\n");
  }
}

// Fill `tops` with the lowest unoccupied y-coordinates per column.
void find_tops(const char* field, int* tops, int ymax) {
  int found_ground[field_width];
  for (int i = 0; i < field_width; i++) found_ground[i] = 0;

  for (int j = ymax; j >= -1; j--) {
    int is_done = 1;
    for (int i = 0; i < field_width; i++) {
      char at_field = j >= 0 ? field[j * field_width + i] : '#';
      if (found_ground[i] == 0 && at_field != 0) {
        found_ground[i] = 1;
        tops[i] = j + 1;
      }
      is_done *= found_ground[i];
    }
    if (is_done) return;
  }
}

int main(int argc, const char** argv) {
  assert(argc == 2);
  FILE* f = fopen(argv[1], "r");
  assert(f != NULL);

  char* input = NULL;
  size_t buffer_len = 0;
  size_t input_len = getline(&input, &buffer_len, f);

  size_t nshapes = 5;
  shape_t shapes[] = {
    { "####", 4, 1 },
    { ".#.###.#.", 3, 3 },
    { "###..#..#", 3, 3 },
    { "####", 1, 4 },
    { "####", 2, 2 },
  };

  int field_height = 7;
  int tower_height = 0;
  int cursor = 0;

  char* field = malloc(field_width * field_height);
  memset(field, 0, field_width * field_height);

  // Every time we cycle through all shapes, test the cursor position. As soon
  // as we find one that we have seen before, we might have found a cycle in the
  // tower pattern.
  int max_rounds = 5000;
  int cursors[max_rounds];
  int tops[max_rounds];

  // At every round, for every column, store 1 + the highest non-empty y-coordinate.
  int floors[max_rounds * field_width];

  long rounds = 2022; // For part 1.
  rounds = 1000000000000;

  cursors[0] = 0;
  tops[0] = 0;
  for (int i = 0; i < field_width; i++) floors[i] = 0;

  for (long round = 0; round < rounds; round++) {
    shape_t* shape = shapes + (round % nshapes);

    // Per round we store:
    // - The position of the cursor (it must match, otherwise we are not
    //   periodic).
    // - The height of the tower at that point (to compute the growth).
    // - The height profile at that point (to ensure that the situations are
    //   identical).
    assert(round < max_rounds);
    cursors[round] = cursor;
    tops[round] = tower_height;
    find_tops(field, floors + (round * field_width), tower_height + 1);

    // For part 2, simulating the 1e12 rounds is infeasible, but we can
    // check if the tower repeats. If so, we don't need to simulate the full
    // thing.
    if (round > 0 && round % nshapes == 0) {
      int k = round;
      for (int i = 0; i < k; i += nshapes) {
        if (cursors[i] == cursor) {
          printf("Found possible cycle, from %d to %d.\n", i, k);
          printf("Early floor: ");
          for (int j = 0; j < field_width; j++) {
            printf("%d, ", tops[i] - floors[i * field_width + j]);
          }
          printf("\n");

          printf("Late floor: ");
          for (int j = 0; j < field_width; j++) {
            printf("%d, ", tops[k] - floors[k * field_width + j]);
          }
          printf("\n");

          int is_match = 1;
          for (int j = 0; j < field_width; j++) {
            int d1 = tops[i] - floors[i * field_width + j];
            int d2 = tops[k] - floors[k * field_width + j];
            if (d1 != d2) {
              is_match = 0;
              break;
            }
          }

          if (is_match) {
            printf("Early slice:\n");
            print_tower(field, tops[i] - 5, tops[i] + 1);

            printf("Late slice:\n");
            print_tower(field, tops[k] - 5, tops[k] + 1);

            long height_growth = tops[k] - tops[i];
            long period = k - i;
            long rounds_pre = i;
            long n_repeats = (rounds - rounds_pre) / period;
            long rounds_repeat = n_repeats * period;
            long rounds_post = rounds - rounds_pre - rounds_repeat;
            long height_pre = tops[i];
            long height_repeats = height_growth * n_repeats;
            long height_post = tops[i + rounds_post] - height_pre;
            printf("The tower would grow as follows:\n");
            printf(" - %ld rounds to height %ld\n", rounds_pre, height_pre);
            printf(
              " - %ld times %ld rounds, %ld height at a time, %ld total\n",
              n_repeats,
              period,
              height_growth,
              height_repeats
            );
            printf(" - %ld rounds, %ld more height\n", rounds_post, height_post);
            printf(
              "For a total height of %ld.\n",
              height_pre + height_repeats + height_post
            );
            exit(0);
          }
        }
      }
    }

    // New blocks fall with 3 empty rows of space.
    int x = 2;
    int y = tower_height + 3;

    while (1) {
      char move = input[cursor];
      switch (move) {
        case '<':
          if (fits(field, shape, x - 1, y)) x--;
          break;
        case '>':
          if (fits(field, shape, x + 1, y)) x++;
          break;
        default:
          printf("Invalid input: %c", move);
          exit(1);
      }

      cursor++;
      if (cursor >= input_len - 1) {
        cursor = 0;
      }

      if (fits(field, shape, x, y - 1)) {
        y--;
      } else {
        mark(field, shape, x, y, '1' + (round % nshapes));
        tower_height = max(tower_height, y + shape->h);

        // If the field is no longer big enough to store everything, double it
        // in size.
        if (tower_height + 8 >= field_height) {
          size_t prev_size = field_width * field_height;
          field = realloc(field, 2 * prev_size);
          memset(field + prev_size, 0, prev_size);
          field_height = 2 * field_height;
        }

        if (round % 1000 == 0 || round == rounds - 1) {
          printf("Round %ld at x=%d, y=%d, tower_height=%d:\n", round, x, y, tower_height);
        }

        // Uncomment to debug print the tower.
        // print_tower(field, 0, tower_height);

        break;
      }
    }
  }

  free(input);
  free(field);

  return 0;
}
