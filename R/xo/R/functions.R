#' plot the state of play
xo_plot <- function(moves) {
  # grid layout
  grid_lines <- data.frame(
    x = c(0, 0, 1, 2),
    y = c(1, 2, 0, 0),
    xend = c(3, 3, 1, 2),
    yend = c(1, 2, 3, 3)
  )

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = grid_lines,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      color = "black",
      linewidth = 1
    ) +
    ggplot2::geom_text(
      data = moves,
      ggplot2::aes(x = x, y = y, label = symbol),
      size = 10,
      fontface = "bold"
    ) +
    ggplot2::scale_x_continuous(limits = c(-0.5, 3.5), breaks = 1:2) +
    ggplot2::scale_y_continuous(limits = c(-0.5, 3.5), breaks = 1:2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

is_xxnull <- function(moves, line_name, my_symbol, lines) {
  check_moves <- moves |>
    dplyr::semi_join(
      lines[lines$line == line_name, ],
      by = c("x", "y")
    )
  nrow(check_moves[check_moves$symbol == my_symbol, ]) == 2 &
    nrow(check_moves[check_moves$symbol != my_symbol, ]) == 0
}

is_line <- function(moves, line_name, lines) {
  check_moves <- moves |>
    dplyr::semi_join(
      lines[lines$line == line_name, ],
      by = c("x", "y")
    )
  check_moves |> dplyr::count(symbol) |> dplyr::filter(n == 3) |> nrow() == 1
}

lines_data <- function() {
  pos <- c(0.5, 1.5, 2.5)

  dplyr::bind_rows(
    expand.grid(x = pos, y = pos) |>
      dplyr::mutate(line = rep(c("row1", "row2", "row3"), each = 3)),
    expand.grid(y = pos, x = pos) |>
      dplyr::mutate(line = rep(c("col1", "col2", "col3"), each = 3)),
    data.frame(
      x = c(0.5, 1.5, 2.5),
      y = c(0.5, 1.5, 2.5)
    ) |>
      dplyr::mutate(line = "diag1"),
    data.frame(
      x = c(2.5, 1.5, 0.5),
      y = c(0.5, 1.5, 2.5)
    ) |>
      dplyr::mutate(line = "diag2")
  )
}

computer_plays <- function(moves, my_symbol) {
  # if two adjacent matching symbols, play in the third cell ---------------------
  continue <- TRUE

  complete_cols <- moves |>
    dplyr::count(x) |>
    dplyr::filter(n == 3) |>
    dplyr::select(-n)
  check_cols <- moves |>
    dplyr::filter(!x %in% complete_cols$x) |>
    dplyr::count(x, symbol) |>
    dplyr::filter(n == 2) |>
    dplyr::slice(1)
  if (nrow(check_cols) == 1) {
    taken <- dplyr::semi_join(moves, check_cols, by = c("x", "symbol")) |>
      dplyr::pull(y)
    available <- setdiff(c(0.5, 1.5, 2.5), taken)
    new_move <- data.frame(x = check_cols$x, y = available, symbol = my_symbol)
    continue <- FALSE
  }

  # then do above for rows
  if (continue) {
    complete_rows <- moves |>
      dplyr::count(y) |>
      dplyr::filter(n == 3) |>
      dplyr::select(-n)
    check_rows <- moves |>
      dplyr::filter(!y %in% complete_rows$y) |>
      dplyr::count(y, symbol) |>
      dplyr::filter(n == 2) |>
      dplyr::slice(1)
    if (nrow(check_rows) == 1) {
      taken <- dplyr::semi_join(moves, check_rows, by = c("y", "symbol")) |>
        dplyr::pull(x)
      available <- setdiff(c(0.5, 1.5, 2.5), taken)
      new_move <- data.frame(
        x = available,
        y = check_rows$y,
        symbol = my_symbol
      )
      continue <- FALSE
    }
  }

  # then do above for diagonals
  if (continue) {
    diag <- data.frame(
      x = c(0.5, 1.5, 2.5),
      y = c(0.5, 1.5, 2.5)
    )

    diag_moves <- moves |> dplyr::inner_join(diag, by = c("x", "y"))
    # diag_completed <- nrow(diag_moves) == 3
    if (nrow(diag_moves) == 2) {#} & !diag_completed) {
      complete_diag <- diag_moves |>
        dplyr::count(symbol) |>
        dplyr::filter(n == 2) |>
        nrow() == 1
      if (complete_diag) {
        new_move <- dplyr::anti_join(diag, diag_moves, by = c("x", "y")) |>
          dplyr::mutate(symbol = my_symbol)
        continue <- FALSE
      }
    }

    if (continue) {
      diag <- data.frame(
        x = c(2.5, 1.5, 0.5),
        y = c(0.5, 1.5, 2.5)
      )

      diag_moves <- moves |> dplyr::inner_join(diag, by = c("x", "y"))
      # diag_completed <- nrow(diag_moves) == 3
      if (nrow(diag_moves) == 2) {# & !diag_completed) {
        complete_diag <- diag_moves |>
          dplyr::count(symbol) |>
          dplyr::filter(n == 2) |>
          nrow() == 1
        if (complete_diag) {
          new_move <- dplyr::anti_join(diag, diag_moves, by = c("x", "y")) |>
            dplyr::mutate(symbol = my_symbol)
          continue <- FALSE
        }
      }
    }
  }

  # if there's a move that creates 2 lines of 2 in a row, play it ----------------
  if (continue) {
    pos <- c(0.5, 1.5, 2.5)
    squares <- expand.grid(x = pos, y = pos)
    available <- dplyr::setdiff(squares, moves |> dplyr::select(-symbol))

    # for each of the available, add square to moves and count the number of lines that are xx-
    # define the lines and count the symbols
    lines <- lines_data()
    line_names <- unique(lines$line)

    # for all available
    i <- 1
    while (continue & i <= nrow(available)) {
      try_move <- available[i, ] |> dplyr::mutate(symbol = my_symbol)
      # print(try_move)
      test_moves <- dplyr::bind_rows(moves, try_move)
      # print(continue)
      pair_count <- 0

      purrr::walk(
        line_names,
        ~ {
          two_in_a_line <- is_xxnull(test_moves, .x, my_symbol, lines)
          if (two_in_a_line) pair_count <<- pair_count + 1
        }
      )

      if (pair_count > 1) {
        # play that move
        new_move <- try_move
        continue <- FALSE
      }

      i <- i + 1
    }
  }

  # if centre is free go there ---------------------------------------------------
  if (continue) {
    centre_free <- moves |> dplyr::filter(x == 1.5 & y == 1.5) |> nrow() == 0
    if (centre_free) {
      new_move <- data.frame(x = 1.5, y = 1.5, symbol = my_symbol)
      continue <- FALSE
    }
  }

  # if opponent has played in a corner, play in the opposite corner --------------
  if (continue) {
    corners <- expand.grid(x = c(0.5, 2.5), y = c(0.5, 2.5)) |>
      dplyr::mutate(pair = c("lr", "rl", "rl", "lr"))
    # last_move <- moves |> tail(1)
    opponent_moves <- moves |> dplyr::filter(symbol != my_symbol)
    opponent_corner <- dplyr::semi_join(corners, opponent_moves, by = c("x", "y")) |> tail(1)
    if (nrow(opponent_corner) == 1) {
      available_corners <- dplyr::semi_join(corners, available, by = c("x", "y"))
      opposite_corner <- dplyr::semi_join(
        available_corners,
        opponent_corner,
        by = "pair"
      )
      new_move <- opposite_corner |>
        dplyr::select(-pair) |>
        dplyr::mutate(symbol = my_symbol)
      if (nrow(new_move) == 1) continue <- FALSE
    }
  }

  # if there's an empty corner, take it ------------------------------------------
  if (continue) {
    corners <- expand.grid(x = c(0.5, 2.5), y = c(0.5, 2.5))
    available_corners <- dplyr::intersect(available, corners)
    if (nrow(available_corners) > 0) {
      new_move <- available_corners |>
        dplyr::slice_sample(n = 1) |>
        dplyr::mutate(symbol = my_symbol)
      continue <- FALSE
    }
  }

  # else play empty square -------------------------------------------------------
  if (continue) {
    new_move <- available |>
      dplyr::slice_sample(n =1) |>
      dplyr::mutate(symbol = my_symbol)
  }

  return(moves |> dplyr::bind_rows(new_move))
}
