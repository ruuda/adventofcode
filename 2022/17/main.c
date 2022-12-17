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
  int y
) {
  for (int i = 0; i < shape->h; i++) {
    for (int j = 0; j < shape->w; j++) {
      char shape_at = shape->blob[i * shape->w + j];
      assert(shape_at == '#' || shape_at == '.');
      if (shape_at == '.') continue;

      int py = y + i;
      int px = x + j;
      field[py * field_width + px] = '#';
    }
  }
}

int max(int x, int y) {
  return x > y ? x : y;
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

  for (int round = 0; round < 2022; round++) {
    shape_t* shape = shapes + (round % nshapes);
    int x = 2;
    // New blocks fall with 3 empty rows of space.
    int y = tower_height + 3;
    while (1) {
      char move = input[cursor];
      switch (move) {
        case '<':
          if (fits(field, shape, x - 1, y)) x--;
          // printf("< -> x=%d y=%d\n", x, y);
          break;
        case '>':
          if (fits(field, shape, x + 1, y)) x++;
          // printf("> -> x=%d y=%d\n", x, y);
          break;
        case '\n':
          // If we reach the end of the input, start over from the start.
          cursor = 0;
          continue;
        default:
          printf("Invalid input: %c", move);
          exit(1);
      }

      cursor++;

      if (fits(field, shape, x, y - 1)) {
        y--;
      } else {
        mark(field, shape, x, y);
        tower_height = max(tower_height, y + shape->h);

        // If the field is no longer big enough to store everything, double it
        // in size.
        if (tower_height + 8 >= field_height) {
          size_t prev_size = field_width * field_height;
          field = realloc(field, 2 * prev_size);
          memset(field + prev_size, 0, prev_size);
          field_height = 2 * field_height;
        }

        printf("Round %d at x=%d, y=%d, tower_height=%d:\n", round, x, y, tower_height);

        int print_field = 0;
        if (print_field) {
          for (int j = tower_height - 1; j >= 0; j--) {
            for (int i = 0; i < field_width; i++) {
              char at_field = field[j * field_width + i];
              if (at_field != 0) {
                printf("#");
              } else {
                printf(".");
              }
            }
            printf("\n");
          }
        }

        break;
      }
    }
  }

  free(input);
  free(field);

  return 0;
}
