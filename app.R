library(shiny)

ui <- navbarPage(title = "ANOVA",
        tabPanel("About",
                    tags$h1("About the App"),
                    tags$br(),
                    tags$p("contents")),
                    tags$hr(),
        
                    tags$h1("Acknowledgements"),
        tabPanel("Robustness of Assumptions",
                    sidebarLayout(
                            sidebarPanel(
                            fluidRow(
                            selectInput(inputId = "skew", 
                                        label = "Shape",
                                        choices = c("Normal",
                                                    "Right Skewed",
                                                    "Left Skewed",
                                        selected = "Normal")
                                        ),
                            tags$br(),
                            tags$p("Instructions")),
                            
                            fluidRow(tags$h4("Whithin group variance"),
                                     tags$p("Instructions"),
                                     tags$br(),
                                     
                                     sliderInput(inputId = "sd1",
                                                 label = p("Curve 1", style = "color:red"),
                                                 min = 0.5, max = 2, value = 1, step = 0.5),
                                     tags$br(),
                                     sliderInput(inputId = "sd2",
                                                 label = p("Curve 2", style = "color:blue"),
                                                 min = 0.5, max = 2, value = 1, step = .05),
                                     tags$br(),
                                     sliderInput(inputId = "sd3",
                                                 label = p("Curve 3", style = "color:green"),
                                                 min = 0.5, max = 2, value = 1, step = .05))),
                    mainPanel(
                    fluidRow(
                        column(plotOutput(outputId = "curve"), width = 8),
                        column(verbatimTextOutput(outputId = "aovTest"), width = 4,
                               tags$br(),
                               ## Confirm this latex is correct; pretty sure it isn't
                               tags$p("At the $$\alpha = .05$$ level this F-stat corresponds to a p-value that suggests there is"),
                               textOutput(outputId = "concl"))))
                )
             ),
        tabPanel("Relationship between ANOVA and F-Statistic",
                sidebarLayout(
                    sidebarPanel(tags$h4("Whithin group variance"),
                                tags$p("Instructions"),
                                tags$br(),
                                         
                                sliderInput(inputId = "sd1",
                                            label = p("Curve 1", style = "color:red"),
                                            min = 0.5, max = 2, value = 1, step = 0.5),
                                tags$br(),
                                sliderInput(inputId = "sd2",
                                            label = p("Curve 2", style = "color:blue"),
                                            min = 0.5, max = 2, value = 1, step = .05),
                                tags$br(),
                                sliderInput(inputId = "sd3",
                                            label = p("Curve 3", style = "color:green"),
                                            min = 0.5, max = 2, value = 1, step = .05),
                                tags$br(),
                                tags$br(),
                                         
                                sliderInput(inputId = "btwsd",
                                            label = "Between group variance",
                                            min = 1, max = 5, value = 1),
                                tags$br(),
                                tags$p("Instructions"))),
                    mainPanel(
                        fluidRow(
                            column(plotOutput(outputId = "boxplot"), width = 8),
                            column(tags$p("Non-reactive text explaining what F-stat is..."),
                                    tags$br(),
                                    verbatimTextOutput(outputId = "aovTest"),
                                    tags$br(),
                                    tags$br(),
                                   ## Check this latex
                                   tags$p("At the $$\\alpha = .05$$ level this F-stat corresponds to a p-value that suggests there is"),  
                                   textOutput(outputId = "concl"), width = 4)))
               
    ),
      tabPanel("Glossary",
               tags$h1("Vocabulary"),
               tags$p("contents"),
               tags$br(),
               
               tags$h1("Equations"),
               tags$p("contents")),
  ##---------------------------------------------------------quiz on boxplots comparing F-stats  
      tabPanel("Quiz",
              tags$img(src = "...png", width = "200", height = "100"),
              tags$p("Explain what image is showing. i.e. what the boxplots represent"),
              tags$br(),
                             
              radioButtons(inputId = "q1",
                           label = "1. Write question here",
                           choices = c("True", "False")),
                             
              tags$br(),
              radioButtons(inputId = "q2",
                           label = "2. Write question here",
                           choices = c("True", "False")),
                             
              tags$br(),
              radioButtons(inputId = "q3",
                           label = "3. Write question here",
                           choices = c("True", "False")),
                             
              actionButton(inputId = "submit",
                           label = "Submit"),
              textOutput(outputId = "answers"))
                    

)


server <- function(input, output) {}


shinyApp(ui = ui, server = server)
