xo_app <- function() {
  lines <- lines_data()
  line_names <- unique(lines$line)

  ui <- fluidPage(
    titlePanel("Noughts and Crosses"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "human_symbol",
          "Choose your symbol:",
          choices = c("O", "X"),
          inline = TRUE
        ),
        br(),
        tags$hr(),
        h4("Game Status"),
        textOutput("game_status"),
        br(),
        tags$hr(),
        h4("Score tracker"),
        textOutput("wins"),
        textOutput("losses"),
        textOutput("stalemates")
      ),
      mainPanel(
        actionButton("reset", "Reset Game"),
        plotOutput("game_plot", click = "plot_click")
      )
    )
  )

  server <- function(input, output, session) {
    moves <- reactiveVal(
      data.frame(x = numeric(), y = numeric(), symbol = character())
    )
    j <- reactiveVal(0)
    has_won <- reactiveVal(FALSE)
    game_status <- reactiveVal("Your move")
    score <- reactiveValues(
      wins = 0,
      losses = 0,
      stalemates = 0
    )

    observeEvent(input$reset, {
      moves(data.frame(x = numeric(), y = numeric(), symbol = character()))
      j(0)
      has_won(FALSE)
      game_status("Your move")
    })

    observeEvent(input$plot_click, {
      req(!has_won(), input$plot_click)

      human_symbol <- stringr::str_to_upper(input$human_symbol)
      my_symbol <- setdiff(c("X", "O"), human_symbol)

      # Convert click to grid coordinates
      click <- input$plot_click
      x <- floor(click$x) + 0.5
      y <- floor(click$y) + 0.5
      new_move <- data.frame(x = x, y = y, symbol = human_symbol)

      # Check if move is valid
      current_moves <- moves()
      is_available <- nrow(
        dplyr::intersect(
          current_moves |> dplyr::select(-symbol),
          new_move |> dplyr::select(-symbol)
        )
      ) ==
        0

      if (is_available) {
        updated_moves <- dplyr::bind_rows(current_moves, new_move)
        j(j() + 1)
        moves(updated_moves)

        # Check win
        if (
          any(unlist(purrr::map(
            line_names,
            ~ is_line(updated_moves, .x, lines)
          )))
        ) {
          has_won(TRUE)
          game_status(paste("You won!", praise::praise()))
          score$wins <- score$wins + 1
          return()
        }

        # Computer plays
        updated_moves <- computer_plays(updated_moves, my_symbol)
        j(j() + 1)
        moves(updated_moves)

        # Check win again
        if (
          any(unlist(purrr::map(
            line_names,
            ~ is_line(updated_moves, .x, lines)
          )))
        ) {
          has_won(TRUE)
          game_status("You lost! Don't be sad!")
          score$losses <- score$losses + 1
          return()
        }

        if (j() >= 9) {
          game_status("Stalemate. You'll get me next time!")
          score$stalemates <- score$stalemates + 1
          has_won(TRUE)
        }
      }
    })

    output$game_plot <- renderPlot({
      p <- xo_plot(moves())
      if (has_won()) {
        p +
          ggplot2::annotate(
            "text",
            x = 1.5,
            y = 3.3,
            label = game_status(),
            size = 10,
            fontface = "bold",
            colour = "red"
          )
      } else {
        p
      }
    })

    output$game_status <- renderText({
      game_status()
    })
    output$wins <-       renderText(paste("Wins:", score$wins))
    output$losses <-     renderText(paste("Losses:", score$losses))
    output$stalemates <- renderText(paste("Draws:", score$stalemates))
  }

  shinyApp(ui, server)
}
