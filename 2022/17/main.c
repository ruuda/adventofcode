#include "assert.h"
#include "stdio.h"
#include "stdlib.h"

typedef struct shape {
  const char* blob;
  int w;
  int h;
} shape_t;

int main(int argc, const char** argv) {
  assert(argc == 2);
  FILE* f = fopen(argv[1], "r");
  assert(f != NULL);

  char* input = NULL;
  size_t buffer_len = 0;
  size_t len = getline(&input, &buffer_len, f);

  size_t nshapes = 5;
  shape_t shapes[] = {
    { "####", 4, 1 },
    { ".#.###.#.", 3, 3 },
    { "..#..####", 3, 3 },
    { "####", 1, 4 },
    { "####", 2, 2 },
  };

  for (size_t i = 0; i < len; i++) {
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

  free(input);

  return 0;
}
