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
  greeting <- reactive({
    if (input$cowboy == "Yes") "Howdy" else "Hello"
  })

  observeEvent(input$go, {
    output$message <- renderText({
      sprintf(
        "%s %s! %s",
        isolate(greeting()),
        isolate(input$name),
        praise::praise()
      )
    })

    output$photo <- renderImage({
      fname <- normalizePath(
        file.path(
          "R/37_howdy_shiny/www",
          ifelse(isolate(input$cowboy == "Yes"), "cowboy.png", "businessman.png")
        )
      )
      list(src = fname)
    }, deleteFile = FALSE)
  })
}

shinyApp(ui, server)
