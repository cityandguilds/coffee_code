library(shiny)

ui <- fluidPage(
  fluidRow(textInput("name", "What's your name?")),
  fluidRow(radioButtons("cowboy", "Are you a cowboy?", choices = c("Yes", "No"))),
  fluidRow(actionButton("go", "Generate!")),
  hr(),
  br(),
  fluidRow(textOutput("message")),
  fluidRow(imageOutput("photo"))
)

server <- function(input, output, session) {
  greeting <- reactive(
    ifelse(input$cowboy == "Yes", "Howdy", "Hello")
  )

  observeEvent(input$go, {
    output$message <- renderText(
      sprintf(
        "%s %s! %s",
        greeting(),
        input$name,
        praise::praise()
      )
    )

    output$photo <- renderImage({
      fname <- normalizePath(
        file.path(
          here::here(),
          "R/37_howdy_shiny/www",
          ifelse(input$cowboy == "Yes", "cowboy.png", "businessman.png")
        )
      )
      list(src = fname)
    }, deleteFile = FALSE)
  })
}

shinyApp(ui, server)
