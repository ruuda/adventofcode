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
  int field_height,
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
    { "..#..####", 3, 3 },
    { "####", 1, 4 },
    { "####", 2, 2 },
  };

  for (size_t i = 0; i < input_len; i++) {
    printf("%c ", input[i]);
  }

  printf("\n");

  for (size_t i = 0; i < nshapes; i++) {
    shape_t* shape = shapes + i;
    int j = 0;
    for (int y = 0; y < shape->h; y++) {
      for (int x = 0; x < shape->w; x++) {
        printf("%c", shape->blob[j]);
        j++;
      }
      printf("\n");
    }
    printf("\n");
  }

  int field_height = 7;
  int tower_height = 0;
  int cursor = 0;

  char* field = malloc(field_width * field_height);
  memset(field, 0, field_width * field_height);

  for (int round = 0; round < 2022; round++) {
    shape_t* shape = shapes + (round % nshapes);
    int x = 2;
    int y = tower_height + 3;
    while (1) {
      char move = input[cursor];
      switch (move) {
        case '<':
          if (fits(field, field_height, shape, x - 1, y)) x--;
          break;
        case '>':
          if (fits(field, field_height, shape, x + 1, y)) x++;
          break;
        case '\n':
          // If we reach the end of the input, start over from the start.
          cursor = 0;
          continue;
        default:
          printf("Invalid input: %c", move);
          exit(1);
      }

      if (fits(field, field_height, shape, x, y - 1)) {
        y--;
      } else {
        // TODO: Mark in the field.
        printf("Round %d at x=%d, y=%d\n", round, x, y);
        break;
      }

      cursor++;
    }
  }

  free(input);
  free(field);

  return 0;
}
