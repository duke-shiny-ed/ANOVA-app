library(shiny)
library(shinythemes)
library(mathjaxr)
library(tidyr)
library(ggplot2)

ui <- navbarPage(theme = shinytheme("lumen"),
                 title = "ANOVA",
                 tabPanel("About",
                          h1("About the App"),
                          p("contents"),
                          hr(),
                          
                          h1("Acknowledgements"),
                          p("contents")),
                 
                 tabPanel("Robustness of Assumptions", value = 1,
                          sidebarLayout(
                            
                            sidebarPanel(
                              h3(strong("Between Group Variance")),
                              p("Instructions", style = "color:grey"),
                              br(),
                              
                              sliderInput(inputId = "btwsd1",
                                          label = NULL,
                                          min = 1, max = 2, value = 1),
                              br(),
                              br(),
                              
                    
                              h3(strong("Assumptions")),
                              wellPanel(
                                h4("Shape"),
                                p("Instructions", style = "color:grey"),
                                br(),       
                                
                                selectInput(inputId = "skew1", 
                                            label = p("Curve 1", style = "color:red"),
                                            choices = c("Normal" = "norm",
                                                        "Right Skewed" = "rskew",
                                                        "Left Skewed" = "lskew"),
                                            selected = "norm"),
                                selectInput(inputId = "skew2", 
                                            label = p("Curve 2", style = "color:green"),
                                            choices = c("Normal" = "norm",
                                                        "Right Skewed" = "rskew",
                                                        "Left Skewed" = "lskew"),
                                            selected = "norm"),
                                selectInput(inputId = "skew3", 
                                            label = p("Curve 3", style = "color:blue"),
                                            choices = c("Normal" = "norm",
                                                        "Right Skewed" = "rskew",
                                                        "Left Skewed" = "lskew"),
                                            selected = "norm")),
                              
                              wellPanel(
                                h4("Within group variance"),
                                p("Instructions", style = "color:grey"),
                                br(),
                                sliderInput(inputId = "sd1.1",
                                            label = p("Curve 1", style = "color:red"),
                                            min = 0.5, max = 1.5, value = 1, step = 0.5),
                                sliderInput(inputId = "sd1.2",
                                            label = p("Curve 2", style = "color:green"),
                                            min = 0.5, max = 1.5, value = 1, step = 0.5),
                                sliderInput(inputId = "sd1.3",
                                            label = p("Curve 3", style = "color:blue"),
                                            min = 0.5, max = 1.5, value = 1, step = 0.5)
                                )
                            ),
                            
                            mainPanel(
                              
                              fluidRow(column(width = 12, plotOutput(outputId = "curve"))),
                              fluidRow( 
                                verbatimTextOutput(outputId = "aovTest"),
                                br(),
                                
                                ## check this LaTeX
                                withMathJax(),
                                p("At the $$\\alpha = .05$$ level this F-stat corresponds to a p-value that suggests there is", 
                                  textOutput(outputId = "concl")))
                            )
                          )
                 ),
                 
                 tabPanel("Relationship between ANOVA and F-Statistic", value = 2,
                          sidebarLayout(
                            
                            sidebarPanel(
                              h3(strong("Between Group Variance")),
                              p("Instructions", style = "color:grey"),
                              br(),
                              sliderInput(inputId = "btwsd2",
                                          label = NULL,
                                          min = 1, max = 2, value = 1),
                              br(),
                              
                              h3(strong("Within group variance")),
                              p("Instructions", style = "color:grey"),
                              br(),
                              sliderInput(inputId = "sd2.1",
                                          label = p("Curve 1", style = "color:red"),
                                          min = 0.5, max = 1.5, value = 1, step = 0.5),
                              sliderInput(inputId = "sd2.2",
                                          label = p("Curve 2", style = "color:green"),
                                          min = 0.5, max = 1.5, value = 1, step = 0.5),
                              sliderInput(inputId = "sd2.3",
                                          label = p("Curve 3", style = "color:blue"),
                                          min = 0.5, max = 1.5, value = 1, step = 0.5)
                            ),
                            
                            mainPanel(
                              fluidRow(p("test test"))
                              #fluidRow(
                               # column(width = 8, plotOutput(outputId = "boxplot")),
                                #column(width = 4, 
                                 #    p("Non-reactive text explaining what F-stat is..."),
                                  #   br(),
                                     
                                   #  verbatimTextOutput(outputId = "aovTest"),
                                    # br(),
                                     #br(),
                                     #p("At the $$\\alpha = .05$$ level this F-stat corresponds to a p-value that suggests there is",
                                    #  textOutput(outputId = "concl"))))
                            )
                        )
                          
                 ),
                 
                 tabPanel("Glossary", value = 3,
                          h1("Vocabulary"),
                          p("contents"),
                          br(),
                          
                          h1("Equations"),
                          p("contents")
                 ),
                 
                 ##---------------------------------------------------------quiz on boxplots comparing F-stats  
                 tabPanel("Quiz", value = 4,
                          img(src = "...png", width = "200", height = "100"),
                          p("Explain what image is showing. i.e. what the boxplots represent"),
                          br(),
                          
                          radioButtons(inputId = "q1",
                                       label = "1. Write question here",
                                       choices = c("True", "False"),
                                       selected = "_None"),
                          br(),
                          
                          radioButtons(inputId = "q2",
                                       label = "2. Write question here",
                                       choices = c("True", "False"),
                                       selected = "_None"),
                          br(),                     
                          
                          radioButtons(inputId = "q3",
                                       label = "3. Write question here",
                                       choices = c("True", "False"),
                                       selected = "_None"),
                          
                          actionButton(inputId = "submit",
                                       label = "Submit"),
                          textOutput(outputId = "answers")
                 )
                 
                 
)


server <- function(input, output, session) {
  ##--------------------------------------------------------------Fluid Sidebar
  ## Btw var slider
  observeEvent(input$btwsd1, {
    if(input$btwsd2 != input$btwsd1) {
      updateSliderInput(session, "btwsd2", value = input$btwsd1)
    }
  })
  observeEvent(input$btwsd2, {
    if(input$btwsd1 != input$btwsd2) {
      updateSliderInput(session, "btwsd1", value = input$btwsd2)
    }
  })
  
  ## Within var Curve 1 slider
  observeEvent(input$sd1.1, {
    if(input$sd2.1 != input$sd1.1) {
      updateSliderInput(session, "sd2.1", value = input$sd1.1)
    }
  })
  observeEvent(input$sd2.1, {
    if(input$sd1.1 != input$sd2.1) {
      updateSliderInput(session, "sd1.1", value = input$sd2.1)
    }
  })
  
  
  ## Within var Curve 2 slider
  observeEvent(input$sd1.2, {
    if(input$sd2.2 != input$sd1.2) {
      updateSliderInput(session, "sd2.2", value = input$sd1.2)
    }
  })
  observeEvent(input$sd2.2, {
    if(input$sd1.2 != input$sd2.2) {
      updateSliderInput(session, "sd1.2", value = input$sd2.2)
    }
  })
  
  ## Within var Curve 3 slider
  observeEvent(input$sd1.3, {
    if(input$sd2.3 != input$sd1.3) {
      updateSliderInput(session, "sd2.3", value = input$sd1.3)
    }
  })
  observeEvent(input$sd2.3, {
    if(input$sd1.3 != input$sd2.3) {
      updateSliderInput(session, "sd1.3", value = input$sd2.3)
    }
  })
  
  ##--------------------------------------------------------------Assumptions Tab
  dist <- function(skew, within) {
    if(skew == "norm") {
      set.seed(1)
      return(rbeta(10000, shape1 = 22, shape2 = 22) * within)
      #initial mean = 0.5002202
    } else if(skew == "rskew") {
      set.seed(1)
      return(rbeta(10000, shape1 = 13, shape2 = 31) * within)
      #initial mean = 0.2955792
    } else {
      set.seed(1)
      return(rbeta(10000, shape1 = 31, shape2 = 13) * within)
      #initial mean = 0.7044208
    }
  }
  
  d1 <- reactive({dist(skew = input$skew1, within = input$sd1.1)})
  d2 <- reactive({dist(skew = input$skew2, within = input$sd1.2)})
  d3 <- reactive({dist(skew = input$skew3, within = input$sd1.3)})
  
  df <- reactive({data.frame(d1(), d2(), d3())})
  df_long <- reactive({
    df() %>%
      gather(key = dataset, value = value)
  })
  
  ##look more into using ggplot for density curves
  output$curve <- renderPlot({
    ggplot(data = df_long(), aes(x=value, color = dataset)) +
      geom_density() +
      ggtitle("Population Distributions") +
      theme(legend.position = "none")
    # + coord_cartesian(xlim =c(0, 1))
  })  
  
  
  runTest <- reactive({aov(value ~ dataset, data = df_long())})
  output$aovTest <- renderPrint ({
    print(summary(runTest()))
  })
  
  
  output$concl <- renderText({
    if(tidy(runTest())$p.value < 0.05) {
      print("sufficient evidence to conclude that there is at least one difference between the group means.")
    } else {
      print("insufficent evidence to conclude that there is at least one difference between the group means.")
    }
  })
  
  ##--------------------------------------------------------------F-Stat Tab
  output$boxplot <- renderPlot({
    ggplot(data = df_long, aes(group = dataset, y = value)) + 
      geom_boxplot(aes(color = dataset)) +
      labs(title = "Sample Data")
  })
}

shinyApp(ui = ui, server = server)