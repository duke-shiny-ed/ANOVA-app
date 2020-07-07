library(shiny)

ui <- fluidPage(
    navbarPage(title = "ANOVA",
        tabPanel("About",
                tags$h1("About the App"),
                tags$br(),
                tags$p("contents")
                ),
        tabPanel("Robustness of Assumptions",
                ),
        tabPanel("Relationship between ANOVA and F-statistic")
        )
    )

server <- function(input, output) {
}


shinyApp(ui = ui, server = server)
