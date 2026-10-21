# you're an expert R programmer. write an algorithm in the R language for a noughts and crosses game.
# Noughts and Crosses (Tic-Tac-Toe) in R

# Initialize the board
init_board <- function() {
  matrix(rep(" ", 9), nrow = 3, ncol = 3)
}

# Print the board
print_board <- function(board) {
  cat("\n")
  for (i in 1:3) {
    cat(paste(board[i, ], collapse = " | "), "\n")
    if (i < 3) cat("---------\n")
  }
  cat("\n")
}

# Check for a win
check_win <- function(board, player) {
  win_patterns <- list(
    board[1, ], board[2, ], board[3, ],         # rows
    board[, 1], board[, 2], board[, 3],         # columns
    diag(board), diag(t(apply(board, 2, rev)))  # diagonals
  )
  any(sapply(win_patterns, function(x) all(x == player)))
}

# Check for a draw
check_draw <- function(board) {
  all(board != " ")
}

# Main game loop
play_game <- function() {
  board <- init_board()
  players <- c("X", "O")
  turn <- 1

  repeat {
    print_board(board)
    player <- players[turn %% 2 + 1]
    cat("Player", player, "'s turn.\n")

    repeat {
      move <- as.integer(strsplit(readline("Enter row and column (e.g., 1 3): "), " ")[[1]])
      if (length(move) == 2 && all(move >= 1 & move <= 3) && board[move[1], move[2]] == " ") {
        board[move[1], move[2]] <- player
        break
      } else {
        cat("Invalid move. Try again.\n")
      }
    }

    if (check_win(board, player)) {
      print_board(board)
      cat("Player", player, "wins!\n")
      break
    }

    if (check_draw(board)) {
      print_board(board)
      cat("It's a draw!\n")
      break
    }

    turn <- turn + 1
  }
}

# Run the game
play_game()

# Add AI opponent to the game -----------------------------------------------
# Initialize the board
init_board <- function() {
  matrix(rep(" ", 9), nrow = 3, ncol = 3)
}

# Print the board
print_board <- function(board) {
  cat("\n")
  for (i in 1:3) {
    cat(paste(board[i, ], collapse = " | "), "\n")
    if (i < 3) cat("---------\n")
  }
  cat("\n")
}

# Check for a win
check_win <- function(board, player) {
  win_patterns <- list(
    board[1, ], board[2, ], board[3, ],
    board[, 1], board[, 2], board[, 3],
    diag(board), diag(t(apply(board, 2, rev)))
  )
  any(sapply(win_patterns, function(x) all(x == player)))
}

# Check for a draw
check_draw <- function(board) {
  all(board != " ")
}

# Get available moves
get_available_moves <- function(board) {
  which(board == " ", arr.ind = TRUE)
}

# AI move logic
ai_move <- function(board, ai, human) {
  moves <- get_available_moves(board)

  # Try to win
  for (i in 1:nrow(moves)) {
    test_board <- board
    test_board[moves[i, 1], moves[i, 2]] <- ai
    if (check_win(test_board, ai)) return(moves[i, ])
  }

  # Block human win
  for (i in 1:nrow(moves)) {
    test_board <- board
    test_board[moves[i, 1], moves[i, 2]] <- human
    if (check_win(test_board, human)) return(moves[i, ])
  }

  # Otherwise, pick random move
  moves[sample(1:nrow(moves), 1), ]
}

# Main game loop
play_game_ai <- function() {
  board <- init_board()
  human <- "X"
  ai <- "O"
  turn <- 1

  repeat {
    print_board(board)

    if (turn %% 2 == 1) {
      cat("Your turn (Player", human, ").\n")
      repeat {
        move <- as.integer(strsplit(readline("Enter row and column (e.g., 1 3): "), " ")[[1]])
        if (length(move) == 2 && all(move >= 1 & move <= 3) && board[move[1], move[2]] == " ") {
          board[move[1], move[2]] <- human
          break
        } else {
          cat("Invalid move. Try again.\n")
        }
      }
    } else {
      cat("AI's turn (Player", ai, ").\n")
      move <- ai_move(board, ai, human)
      board[move[1], move[2]] <- ai
    }

    if (check_win(board, human)) {
      print_board(board)
      cat("You win!\n")
      break
    } else if (check_win(board, ai)) {
      print_board(board)
      cat("AI wins!\n")
      break
    } else if (check_draw(board)) {
      print_board(board)
      cat("It's a draw!\n")
      break
    }

    turn <- turn + 1
  }
}

# Run the game
play_game_ai()

# create gui with shiny
library(shiny)

# Helper functions
init_board <- function() {
  matrix(rep(" ", 9), nrow = 3, ncol = 3)
}

check_win <- function(board, player) {
  win_patterns <- list(
    board[1, ], board[2, ], board[3, ],
    board[, 1], board[, 2], board[, 3],
    diag(board), diag(t(apply(board, 2, rev)))
  )
  any(sapply(win_patterns, function(x) all(x == player)))
}

check_draw <- function(board) {
  all(board != " ")
}

get_available_moves <- function(board) {
  which(board == " ", arr.ind = TRUE)
}

ai_move <- function(board, ai, human) {
  moves <- get_available_moves(board)

  for (i in 1:nrow(moves)) {
    test_board <- board
    test_board[moves[i, 1], moves[i, 2]] <- ai
    if (check_win(test_board, ai)) return(moves[i, ])
  }

  for (i in 1:nrow(moves)) {
    test_board <- board
    test_board[moves[i, 1], moves[i, 2]] <- human
    if (check_win(test_board, human)) return(moves[i, ])
  }

  moves[sample(1:nrow(moves), 1), ]
}

# UI
ui <- fluidPage(
  titlePanel("Noughts and Crosses (Tic-Tac-Toe) vs AI"),
  fluidRow(
    column(12,
           uiOutput("board_ui"),
           textOutput("game_status"),
           actionButton("reset", "Reset Game")
    )
  )
)

# Server
server <- function(input, output, session) {
  board <- reactiveVal(init_board())
  turn <- reactiveVal("X")
  game_over <- reactiveVal(FALSE)

  observeEvent(input$reset, {
    board(init_board())
    turn("X")
    game_over(FALSE)
  })

  output$board_ui <- renderUI({
    b <- board()
    grid <- lapply(1:3, function(i) {
      fluidRow(
        lapply(1:3, function(j) {
          idx <- paste0("cell_", i, "_", j)
          actionButton(inputId = idx, label = b[i, j], width = "60px")
        })
      )
    })
    do.call(tagList, grid)
  })

  observe({
    if (game_over()) return()

    for (i in 1:3) {
      for (j in 1:3) {
        local({
          ii <- i; jj <- j
          observeEvent(input[[paste0("cell_", ii, "_", jj)]], {
            b <- board()
            if (b[ii, jj] == " " && turn() == "X") {
              b[ii, jj] <- "X"
              board(b)

              if (check_win(b, "X")) {
                game_over(TRUE)
                return()
              } else if (check_draw(b)) {
                game_over(TRUE)
                return()
              }

              turn("O")

              # AI move
              isolate({
                Sys.sleep(0.5)
                move <- ai_move(board(), "O", "X")
                b <- board()
                b[move[1], move[2]] <- "O"
                board(b)

                if (check_win(b, "O") || check_draw(b)) {
                  game_over(TRUE)
                } else {
                  turn("X")
                }
              })
            }
          }, ignoreInit = TRUE)
        })
      }
    }
  })

  output$game_status <- renderText({
    b <- board()
    if (check_win(b, "X")) {
      "You win!"
    } else if (check_win(b, "O")) {
      "AI wins!"
    } else if (check_draw(b)) {
      "It's a draw!"
    } else {
      paste("Turn:", turn())
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

# Implement a scoring system
ui <- fluidPage(
  titlePanel("Noughts and Crosses (Tic-Tac-Toe) vs AI"),
  fluidRow(
    column(12,
           uiOutput("board_ui"),
           textOutput("game_status"),
           actionButton("reset", "Reset Game"),
           hr(),
           h4("Scoreboard"),
           verbatimTextOutput("score_display")
    )
  )
)
server <- function(input, output, session) {
  board <- reactiveVal(init_board())
  turn <- reactiveVal("X")
  game_over <- reactiveVal(FALSE)

  # Score tracking
  score <- reactiveValues(player = 0, ai = 0, draw = 0)

  observeEvent(input$reset, {
    board(init_board())
    turn("X")
    game_over(FALSE)
  })

  output$board_ui <- renderUI({
    b <- board()
    grid <- lapply(1:3, function(i) {
      fluidRow(
        lapply(1:3, function(j) {
          idx <- paste0("cell_", i, "_", j)
          actionButton(inputId = idx, label = b[i, j], width = "60px")
        })
      )
    })
    do.call(tagList, grid)
  })

  observe({
    if (game_over()) return()

    for (i in 1:3) {
      for (j in 1:3) {
        local({
          ii <- i; jj <- j
          observeEvent(input[[paste0("cell_", ii, "_", jj)]], {
            b <- board()
            if (b[ii, jj] == " " && turn() == "X") {
              b[ii, jj] <- "X"
              board(b)

              if (check_win(b, "X")) {
                score$player <- score$player + 1
                game_over(TRUE)
                return()
              } else if (check_draw(b)) {
                score$draw <- score$draw + 1
                game_over(TRUE)
                return()
              }

              turn("O")

              # AI move
              isolate({
                Sys.sleep(0.5)
                move <- ai_move(board(), "O", "X")
                b <- board()
                b[move[1], move[2]] <- "O"
                board(b)

                if (check_win(b, "O")) {
                  score$ai <- score$ai + 1
                  game_over(TRUE)
                } else if (check_draw(b)) {
                  score$draw <- score$draw + 1
                  game_over(TRUE)
                } else {
                  turn("X")
                }
              })
            }
          }, ignoreInit = TRUE)
        })
      }
    }
  })

  output$game_status <- renderText({
    b <- board()
    if (check_win(b, "X")) {
      "You win!"
    } else if (check_win(b, "O")) {
      "AI wins!"
    } else if (check_draw(b)) {
      "It's a draw!"
    } else {
      paste("Turn:", turn())
    }
  })

  output$score_display <- renderText({
    paste0("Player Wins: ", score$player,
           "\nAI Wins: ", score$ai,
           "\nDraws: ", score$draw)
  })
}

# use ggplot2 to render the game board

# Integrate ggplot2 board in Shiny
library(shiny)

# Helper functions
init_board <- function() {
  matrix(rep(" ", 9), nrow = 3, ncol = 3)
}

check_win <- function(board, player) {
  win_patterns <- list(
    board[1, ], board[2, ], board[3, ],
    board[, 1], board[, 2], board[, 3],
    diag(board), diag(t(apply(board, 2, rev)))
  )
  any(sapply(win_patterns, function(x) all(x == player)))
}

check_draw <- function(board) {
  all(board != " ")
}

get_available_moves <- function(board) {
  which(board == " ", arr.ind = TRUE)
}

ai_move <- function(board, ai, human) {
  moves <- get_available_moves(board)

  for (i in 1:nrow(moves)) {
    test_board <- board
    test_board[moves[i, 1], moves[i, 2]] <- ai
    if (check_win(test_board, ai)) return(moves[i, ])
  }

  for (i in 1:nrow(moves)) {
    test_board <- board
    test_board[moves[i, 1], moves[i, 2]] <- human
    if (check_win(test_board, human)) return(moves[i, ])
  }

  moves[sample(1:nrow(moves), 1), ]
}

ui <- fluidPage(
  titlePanel("Tic-Tac-Toe with ggplot2 Board"),
  fluidRow(
    column(6,
           plotOutput("game_plot", click = "plot_click"),
           textOutput("game_status"),
           actionButton("reset", "Reset Game")
    ),
    column(6,
           h4("Scoreboard"),
           verbatimTextOutput("score_display")
    )
  )
)

server <- function(input, output, session) {
  library(ggplot2)

  board <- reactiveVal(init_board())
  turn <- reactiveVal("X")
  game_over <- reactiveVal(FALSE)
  score <- reactiveValues(player = 0, ai = 0, draw = 0)

  observeEvent(input$reset, {
    board(init_board())
    turn("X")
    game_over(FALSE)
  })

  output$game_plot <- renderPlot({
    b <- board()
    df <- expand.grid(row = 1:3, col = 1:3)
    df$symbol <- as.vector(t(b))

    ggplot(df, aes(x = col, y = 4 - row)) +
      geom_tile(fill = "white", color = "black", size = 1.5) +
      geom_text(aes(label = symbol), size = 10) +
      scale_x_continuous(breaks = 1:3) +
      scale_y_continuous(breaks = 1:3) +
      theme_void() +
      theme(panel.grid = element_blank())
  })

  observeEvent(input$plot_click, {
    if (game_over()) return()
    x <- round(input$plot_click$x)
    y <- round(input$plot_click$y)

    if (x %in% 1:3 && y %in% 1:3) {
      row <- 4 - y
      col <- x
      b <- board()

      if (b[row, col] == " " && turn() == "X") {
        b[row, col] <- "X"
        board(b)

        if (check_win(b, "X")) {
          score$player <- score$player + 1
          game_over(TRUE)
          return()
        } else if (check_draw(b)) {
          score$draw <- score$draw + 1
          game_over(TRUE)
          return()
        }

        turn("O")

        isolate({
          Sys.sleep(0.5)
          move <- ai_move(board(), "O", "X")
          b <- board()
          b[move[1], move[2]] <- "O"
          board(b)

          if (check_win(b, "O")) {
            score$ai <- score$ai + 1
            game_over(TRUE)
          } else if (check_draw(b)) {
            score$draw <- score$draw + 1
            game_over(TRUE)
          } else {
            turn("X")
          }
        })
      }
    }
  })

  output$game_status <- renderText({
    b <- board()
    if (check_win(b, "X")) {
      "You win!"
    } else if (check_win(b, "O")) {
      "AI wins!"
    } else if (check_draw(b)) {
      "It's a draw!"
    } else {
      paste("Turn:", turn())
    }
  })

  output$score_display <- renderText({
    paste0("Player Wins: ", score$player,
           "\nAI Wins: ", score$ai,
           "\nDraws: ", score$draw)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
