#!/usr/bin/awk -f

BEGIN {
  first_score = 0;
  first_bingo = 75;

  last_score = 0;
  last_bingo = 0;

  board_line = 0;
}

function eval_board() {
  for (i = 1; i <= length(draws); i++) {
    # Mark the number that we just drew by setting the value to "x".
    for (j = 0; j < 25; j++) {
      if (board[j] == draws[i]) board[j] = "x";
    }

    bingo = 0;

    for (row = 0; row < 5; row++) {
      row_bingo = 1;
      for (col = 0; col < 5; col++) {
        if (board[row * 5 + col] != "x") {
          row_bingo = 0;
        }
      }
      if (row_bingo) bingo = 1;
    }
    for (col = 0; col < 5; col++) {
      col_bingo = 1;
      for (row = 0; row < 5; row++) {
        if (board[row * 5 + col] != "x") col_bingo = 0;
      }
      if (col_bingo) bingo = 1;
    }

    if (bingo) {
      sum = 0;
      for (j = 0; j < 25; j++) {
        if (board[j] != "x") sum += board[j];
      }

      score = sum * draws[i];

      if (i < first_bingo) {
        first_score = score;
        first_bingo = i;
      }
      if (i == first_bingo && score > first_score) {
        first_score = score;
      }

      if (i > last_bingo) {
        last_score = score;
        last_bingo = i;
      }
      if (i == last_bingo && score < last_score) {
        last_score = score;
      }

      print("Bingo at draw", i);
      return;
    }
  }
}

NR == 1 {
  # Line 0 contains the draws, delimited by comma.
  # Store them in the "draws" array.
  split($0, draws, ",");
  first_bingo = length(draws);
}
$0 == "" && NR > 2 {
  eval_board();
  board_line = 0;
}
$0 != "" && NR > 2 {
  board[board_line * 5 + 0] = $1;
  board[board_line * 5 + 1] = $2;
  board[board_line * 5 + 2] = $3;
  board[board_line * 5 + 3] = $4;
  board[board_line * 5 + 4] = $5;
  board_line++;
}

END {
  # We still need to evaluate the final board, as there is no blank line after it.
  eval_board();
  print("First win: score", first_score, "at draw", first_bingo);
  print("Last win:  score", last_score, "at draw", last_bingo);
}
