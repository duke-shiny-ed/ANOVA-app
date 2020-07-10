library(shiny)
library(tidyr)
library(ggplot2)

ui <- navbarPage(title = "ANOVA",
        tabPanel("About",
                    tags$h1("About the App"),
                    tags$br(),
                    tags$p("contents"),
                    tags$hr(),
        
                    tags$h1("Acknowledgements"),
                    tags$p("contents")),
        tabPanel("Robustness of Assumptions",
                    sidebarLayout(
                            sidebarPanel(
                            fluidRow(
                              sliderInput(inputId = "btwsd",
                                          label = "Between group variance",
                                          min = 1, max = 2, value = 1),
                              tags$br(),
                              tags$p("Instructions"),
                              tags$hr(),
                              
                              tags$h2("Assumptions"),
                              tags$br(),
                              tags$h4("Shape"),
                              tags$p("Instructions"),
                              tags$br(),
                              
                              selectInput(inputId = "skew1", 
                                        label = p("Curve 1", style = "color:red"),
                                        choices = c("Normal" = "norm",
                                                    "Right Skewed" = "rskew",
                                                    "Left Skewed" = "lskew",
                                        selected = "norm")
                                        ),
                            tags$br(),
                            selectInput(inputId = "skew2", 
                                        label = p("Curve 2", style = "color:blue"),
                                        choices = c("Normal" = "norm",
                                                    "Right Skewed" = "rskew",
                                                    "Left Skewed" = "lskew",
                                        selected = "norm")
                                        ),
                            tags$br(),
                            selectInput(inputId = "skew3", 
                                        label = p("Curve 3", style = "color:green"),
                                        choices = c("Normal" = "norm",
                                                    "Right Skewed" = "rskew",
                                                    "Left Skewed" = "lskew",
                                        selected = "norm")
                                        ),
                            tags$br()),
                            
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
                    sidebarPanel(sliderInput(inputId = "btwsd",
                                             label = "Between group variance",
                                             min = 1, max = 2, value = 1),
                                 tags$br(),
                                 tags$p("Instructions"),
                                 tags$br(),
                                 tags$br(),
                      
                                tags$h4("Whithin group variance"),
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
                                            min = 0.5, max = 2, value = 1, step = .05)),
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
                )
               
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
                           choices = c("True", "False"),
                           selected = "_None"),
                             
              tags$br(),
              radioButtons(inputId = "q2",
                           label = "2. Write question here",
                           choices = c("True", "False"),
                           selected = "_None"),
                             
              tags$br(),
              radioButtons(inputId = "q3",
                           label = "3. Write question here",
                           choices = c("True", "False"),
                           selected = "_None"),
                             
              actionButton(inputId = "submit",
                           label = "Submit"),
              textOutput(outputId = "answers"))
                    

)


server <- function(input, output) {
  ##--------------------------------------------------------------Tab 2
  dist <- function(skew, within, between) {
    if(skew == "norm") {
      set.seed(1)
      (rbeta(10000, shape1 = 22, shape2 = 22) * within + (between * 0.1))
      #initial mean = 0.5002202
    } else if(skew == "rskew") {
      set.seed(1)
      (rbeta(10000, shape1 = 13, shape2 = 31) * within + (between * 0.1))
      #initial mean = 0.2955792
    } else {
      set.seed(1)
      (rbeta(10000, shape1 = 31, shape2 = 13) * within + (between * 0.1))
      #initial mean = 0.7044208
    }
  }
  
  d1 <- reactive({dist(skew = input$skew1, within = input$sd1, between = input$btwsd)})
  d2 <- reactive({dist(skew = input$skew2, within = input$sd2, between = input$btwsd)})
  d3 <- reactive({dist(skew = input$skew3, within = input$sd3, between = input$btwsd)})
  
  df <- reactive({dataframe(d1(), d2(), d3())})
  df_long <- reactive({
    df() %>%
      gather(key = dataset, value = value)
    })
  
  ##look into using ggplot for density curves; also how to add title
output$curve <- renderPlot({
  ggplot(data = df_long, aes(x = value)) + 
    geom_density(aes(color = dataset)) + 
    coord_cartesian(xlim =c(0, 1))
  })  
  
}


shinyApp(ui = ui, server = server)
