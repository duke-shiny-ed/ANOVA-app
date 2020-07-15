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
                              h3("Population Inputs"),
                              hr(),
                              
                              h3(strong("Between Group Variance")),
                              p("Instructions", style = "color:grey"),
                              br(),
                              
                              sliderInput(inputId = "btwsd1",
                                          label = NULL,
                                          min = 0, max = 1, value = 1, step = .001),
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
                                            min = 0.25, max = 1.25, value = 1, step = 0.25),
                                sliderInput(inputId = "sd1.2",
                                            label = p("Curve 2", style = "color:green"),
                                            min = 0.25, max = 1.25, value = 1, step = 0.25),
                                sliderInput(inputId = "sd1.3",
                                            label = p("Curve 3", style = "color:blue"),
                                            min = 0.25, max = 1.25, value = 1, step = 0.25)
                                )
                            ),
                            
                            mainPanel(
                              
                              fluidRow(column(width = 12, plotOutput(outputId = "curve"))),
                              fluidRow( 
                                verbatimTextOutput(outputId = "aovTest1"),
                                br(),
                                
                                ## check this LaTeX
                                withMathJax(),
                                p("At the $$\\alpha = .05$$ level this F-stat corresponds to a p-value that suggests there is", 
                                  textOutput(outputId = "concl1")))
                            )
                          )
                 ),
                 
                 tabPanel("Relationship between ANOVA and F-Statistic", value = 2,
                          sidebarLayout(
                            
                            sidebarPanel(
                              h3("Sample Inputs"),
                              hr(),
                              
                              h3(strong("Between Group Variance")),
                              p("Instructions", style = "color:grey"),
                              br(),
                              sliderInput(inputId = "btwsd2",
                                          label = NULL,
                                          min = 0, max = 1, value = 1, step = .001),
                              br(),
                              
                              h3(strong("Within group variance")),
                              p("Instructions", style = "color:grey"),
                              br(),
                              sliderInput(inputId = "sd2.1",
                                          label = p("Curve 1", style = "color:red"),
                                          min = 0.25, max = 1.25, value = 1, step = 0.25),
                              sliderInput(inputId = "sd2.2",
                                          label = p("Curve 2", style = "color:green"),
                                          min = 0.25, max = 1.25, value = 1, step = 0.25),
                              sliderInput(inputId = "sd2.3",
                                          label = p("Curve 3", style = "color:blue"),
                                          min = 0.25, max = 1.25, value = 1, step = 0.25)
                            ),
                            
                            mainPanel(
                              fluidRow(column(width = 12, plotOutput(outputId = "boxplot"))),
                              fluidRow(
                               p("Non-reactive text explaining what F-stat is... Also indicate that below is the F-stat:"),
                                br(),
                                
                                verbatimTextOutput(outputId = "aovTest2"),
                                br(),
                                br(),
                                p("At the $$\\alpha = .05$$ level this F-stat corresponds to a p-value that suggests there is",
                                  textOutput(outputId = "concl2")))
                              
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
  ##--------------------------------------------------------------Assumptions Tab
  dist <- function(skew, within, n) {
    if(skew == "norm") {
      set.seed(n)
      return(rbeta(20000, shape1 = 22, shape2 = 22) * within)
      #initial mean = 0.5002202, 0.5009305, 0.4988691
    } else if(skew == "rskew") {
      set.seed(n)
      return(rbeta(20000, shape1 = 13, shape2 = 31) * within)
      #initial mean = 0.2955792, 0.2961759, 0.2941847
    } else {
      set.seed(n)
      return(rbeta(20000, shape1 = 31, shape2 = 13) * within)
      #initial mean = 0.7044208, 0.7038241, 0.7058153
    }
  }
  
  d1c <- reactive({dist(skew = input$skew1, within = input$sd1.1, n = 1)})
  d2c <- reactive({dist(skew = input$skew2, within = input$sd1.2, n = 2)})
  d3c <- reactive({dist(skew = input$skew3, within = input$sd1.3, n = 3)})
  
  ## effect of btwsd slider
  translc <- function(set1, set2, set3, between) {
    if(mean(set1) > mean(set2) & mean(set1) > mean(set3)) {
      return(set1 + (between * 0.1))
    } else if(mean(set1) < mean(set2) & mean(set1) < mean(set3)) {
      return(set1 - (between * 0.1))
    } else {
      #if() {
        
     # } else {
        return(set1)
      #}
    }
  }
  # difference between means detected at 0.0005 total
  
  d1_translc <- reactive({translc(set1 = d1c(), set2 = d2c(), set3 = d3c(), between = input$btwsd1)})
  d2_translc <- reactive({translc(set1 = d2c(), set2 = d1c(), set3 = d3c(), between = input$btwsd1)})
  d3_translc <- reactive({translc(set1 = d3c(), set2 = d1c(), set3 = d2c(), between = input$btwsd1)})
  
  dfc <- reactive({data.frame(d1_translc(), d2_translc(), d3_translc())})
  dfc_long <- reactive({
    dfc() %>%
      gather(key = dataset, value = value)
  })
  
  
  output$curve <- renderPlot({
    ggplot(data = dfc_long(), aes(x=value, color = dataset)) +
      geom_density() +
      coord_cartesian(xlim = c(0, 1), ylim = c(0,35)) +
      ggtitle("Population Distributions") +
      theme(legend.position = "none") 
  })
  
  
  runTest1 <- reactive({aov(value ~ dataset, data = dfc_long())})
  output$aovTest1 <- renderPrint ({
    print(summary(runTest1()))
  })
  
  
  output$concl1 <- renderText({
    if(tidy(runTest1())$p.value[1] < 0.05) {
      print("sufficient evidence to conclude that there is at least one difference between the group means.")
    } else {
      print("insufficent evidence to conclude that there is at least one difference between the group means.")
    }
  })
  
  ##--------------------------------------------------------------F-Stat Tab
  sample1 <- reactive({sample(d1_translc(), size = 100, replace = TRUE)})
  sample2 <- reactive({sample(d2_translc(), size = 100, replace = TRUE)})
  sample3 <- reactive({sample(d3_translc(), size = 100, replace = TRUE)})
  
  distb <- function(set1, set2, set3, between, within) {
      if(mean(set1) > mean(set2) & mean(set1) > mean(set3)) {
        return(set1 * within + (between * 0.1))
      } else if(mean(set1) < mean(set2) & mean(set1) < mean(set3)) {
        return(set1 * within - (between * 0.1))
      } else {
        #if() {
        
        # } else {
        return(set1 * within)
        #}
      }
    }
  
  d1b <- reactive({distb(set1 = sample1(), set2 = sample2(), set3 = sample3(), within = input$sd2.1, between = input$btwsd2)})
  d2b <- reactive({distb(set1 = sample2(), set2 = sample3(), set3 = sample1(), within = input$sd2.2, between = input$btwsd2)})
  d3b <- reactive({distb(set1 = sample3(), set2 = sample1(), set3 = sample2(), within = input$sd2.3, between = input$btwsd2)})
  
  dfb <- reactive({data.frame(d1b(), d2b(), d3b())})
  dfb_long <- reactive({
    dfb() %>%
      gather(key = dataset, value = value)
  })
  
  output$boxplot <- renderPlot({
    ggplot(data = dfb_long(), aes(group = dataset, y = value)) + 
      geom_boxplot(aes(color = dataset)) +
      #coord_cartesian(ylim =c(0.1, 1.2)) +
      labs(title = "Sample Data") +
      theme(legend.position = "none")
  })
  
  runTest2 <- reactive({aov(value ~ dataset, data = dfb_long())})
  output$aovTest2 <- renderPrint ({
    print(summary(runTest2())[[1]][["F value"]][[1]])
  })
  
  output$concl2 <- renderText({
    if(tidy(runTest2())$p.value[1] < 0.05) {
      print("sufficient evidence to conclude that there is at least one difference between the group means.")
    } else {
      print("insufficent evidence to conclude that there is at least one difference between the group means.")
    }
  })
}

shinyApp(ui = ui, server = server)