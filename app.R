library(shiny)
library(shinythemes)
library(mathjaxr)
library(tidyr)
library(ggplot2)
library(broom)

ui <- navbarPage(theme = shinytheme("lumen"),
                 
                 title = "ANOVA",
                 tabPanel("About",
                          h1("About the App"),
                          p("contents"),
                          hr(),
                          
                          h1("Acknowledgements"),
                          p("contents")),
                 
                 tabPanel("Step 1: Create the Population", value = 1,
                          sidebarLayout(
                            
                            sidebarPanel(
                              h3("Population Inputs"),
                              p("Remember to clarify that in reality we can't manipulate sample data like we are doing here", 
                                style = "color:grey"),
                              hr(),
                              
                              h3(strong("Between Group Variance")),
                              p("Instructions", style = "color:grey"),
                              br(),
                              
                              #sliderInput(inputId = "btwsd1",
                                          #label = NULL,
                                          #min = 0, max = 1, value = 1, step = .001),
                              #toggle btw group variance
                              selectInput(inputId = "btwsd1",
                                          label = NULL,
                                          choices = c("Increased Between Group Variance" = "inc",
                                                      "Reduced Between Group Variance" = "dec"),
                                          selected = "inc"),
                              br(),
                              
                              h3(strong("Shape")),
                              wellPanel(
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
                              br(),
                              
                              h3(strong("Within group variance")),
                              wellPanel(
                                p("Instructions", style = "color:grey"),
                                br(),
                                
                                sliderInput(inputId = "sd1.1",
                                            label = p("Curve 1", style = "color:red"),
                                            min = 0.75, max = 1.25, value = 1),
                                sliderInput(inputId = "sd1.2",
                                            label = p("Curve 2", style = "color:green"),
                                            min = 0.75, max = 1.25, value = 1),
                                sliderInput(inputId = "sd1.3",
                                            label = p("Curve 3", style = "color:blue"),
                                            min = 0.75, max = 1.25, value = 1)
                              )
                            ),
                            
                            mainPanel(
                              
                              fluidRow(column(width = 12, plotOutput(outputId = "curve"))),
                              fluidRow( 
                                verbatimTextOutput(outputId = "aovTest1"),
                                br(),
                                
                                ## check this LaTeX
                                withMathJax(),
                                tags$script(
                                  "MathJax.Hub.Config({
                                tex2jax: {
                                inlineMath: [['$','$'], ['\\(','\\)']],
                                processEscapes: true
                                }
                                });"
                                ),
                                p("At the $\\alpha = .05$ level this F-stat corresponds to a p-value that suggests there is", 
                                  textOutput(outputId = "concl1")))
                            )
                          )
                 ),
                 
                 tabPanel("Step 2: Use Samples to Visualize the F-Statistic", value = 2,
                          sidebarLayout(
                            
                            sidebarPanel(
                              h3("Sample Inputs"),
                              p("Remember to clarify that in reality we can't manipulate sample data like we are doing here", 
                                style = "color:grey"),
                              hr(),
                              
                              h3(strong("Between Group Variance")),
                              p("Instructions", style = "color:grey"),
                              br(),
                              #sliderInput(inputId = "btwsd2",
                                          #label = NULL,
                                          #min = 0, max = 1, value = 0, step = .001), ## <- set btwsd2 to a value that doesn't initilaly maniuplate sample data
                              selectInput(inputId = "btwsd2",
                                          label = NULL,
                                          choices = c("Increased Between Group Variance" = "inc",
                                                      "Reduced Between Group Variance" = "dec"),
                                          selected = "dec"),
                              br(),
                              
                              h3(strong("Within group variance")),
                              p("Instructions", style = "color:grey"),
                              br(),
                              sliderInput(inputId = "sd2.1",
                                          label = p("Curve 1", style = "color:red"),
                                          min = 0.75, max = 1.25, value = 1),
                              sliderInput(inputId = "sd2.2",
                                          label = p("Curve 2", style = "color:green"),
                                          min = 0.75, max = 1.25, value = 1),
                              sliderInput(inputId = "sd2.3",
                                          label = p("Curve 3", style = "color:blue"),
                                          min = 0.75, max = 1.25, value = 1)
                            ),
                            
                            mainPanel(
                              fluidRow(column(width = 12, plotOutput(outputId = "boxplot"))),
                              fluidRow(
                                p("Non-reactive text explaining what F-stat is..."),
                                br(),
                                
                                p("Indicate that below is the F-stat:"),
                                verbatimTextOutput(outputId = "aovTest2"),
                                br(),
                                br(),
                                
                                p("At the $\\alpha = .05$ level this F-stat corresponds to a p-value that suggests there is",
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
  pop_dist <- function(skew, within, n) {
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
  
  pop1 <- reactive({pop_dist(skew = input$skew1, within = input$sd1.1, n = 1)})
  pop2 <- reactive({pop_dist(skew = input$skew2, within = input$sd1.2, n = 2)})
  pop3 <- reactive({pop_dist(skew = input$skew3, within = input$sd1.3, n = 3)})
  
  ## effect of btwsd select menu
  translate_pop <- function(set1, set2, set3, between) {
    if(between == "inc") {
      if(mean(set1) > mean(set2) & mean(set1) > mean(set3)) {
        return(set1 + 0.15)
      } else if(mean(set1) < mean(set2) & mean(set1) < mean(set3)) {
        return(set1 - 0.15)
      } else {
        return(set1)
      }
    } else if(between == "dec"){
      if(mean(set1) > mean(set2) & mean(set1) > mean(set3)) {
        return(set1)
      } else if(mean(set1) < mean(set2) & mean(set1) < mean(set3)) {
        return(set1)
      } else {
        return(set1)
      }
    }
      
  }
  # difference between means detected at 0.0005 total
  
  pop1_trans <- reactive({translate_pop(set1 = pop1(), set2 = pop2(), set3 = pop3(), between = input$btwsd1)})
  pop2_trans <- reactive({translate_pop(set1 = pop2(), set2 = pop1(), set3 = pop3(), between = input$btwsd1)})
  pop3_trans <- reactive({translate_pop(set1 = pop3(), set2 = pop1(), set3 = pop2(), between = input$btwsd1)})
  
  popdf <- reactive({data.frame(pop1_trans(), pop2_trans(), pop3_trans())})
  popdf_long <- reactive({
    popdf() %>%
      gather(key = dataset, value = values)
  })
  
  output$curve <- renderPlot({
    ggplot(data = popdf_long()) +
      geom_density(aes(x=values, color = dataset)) +
      #coord_cartesian(xlim = c(0, 1), ylim = c(0,35)) +
      ggtitle("Population Distributions") +
      theme(legend.position = "none") 
  })
  
  sample1 <- reactive({
    set.seed(1)
    sample(pop1_trans(), size = 100, replace = TRUE)
  })
  sample2 <- reactive({
    set.seed(1)
    sample(pop2_trans(), size = 100, replace = TRUE)
  })
  sample3 <- reactive({
    set.seed(1)
    sample(pop3_trans(), size = 100, replace = TRUE)
  })
  
  sampledf <- reactive({data.frame(sample1(), sample2(), sample3())})
  sampledf_long <- reactive({
    sampledf() %>%
      gather(key = dataset, value = values)
  })
  
  runTest1 <- reactive({aov(values ~ dataset, data = sampledf_long())})
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
  ##recall that sample1, sample2, sample3 already defined 
  translate_sample <- function(set1, set2, set3, within, between) {
    if(between == "inc") {
      if(mean(set1) > mean(set2) & mean(set1) > mean(set3)) {
        return(set1 * within + 0.15)
      } else if(mean(set1) < mean(set2) & mean(set1) < mean(set3)) {
        return(set1 * within - 0.15)
      } else {
        return(set1 * within)
      }
    } else if(between == "dec"){
      if(mean(set1) > mean(set2) & mean(set1) > mean(set3)) {
        return(set1 * within)
      } else if(mean(set1) < mean(set2) & mean(set1) < mean(set3)) {
        return(set1 * within)
      } else {
        return(set1 * within)
      }
    }
    
  }
  
  sample1_trans <- reactive({translate_sample(set1 = sample1(), set2 = sample2(), set3 = sample3(), within = input$sd2.1, between = input$btwsd2)})
  sample2_trans <- reactive({translate_sample(set1 = sample2(), set2 = sample3(), set3 = sample1(), within = input$sd2.2, between = input$btwsd2)})
  sample3_trans <- reactive({translate_sample(set1 = sample3(), set2 = sample1(), set3 = sample2(), within = input$sd2.3, between = input$btwsd2)})
  
  trans_sampledf <- reactive({data.frame(sample1_trans(), sample2_trans(), sample3_trans())})
  trans_sampledf_long <- reactive({
    trans_sampledf() %>%
      gather(key = dataset, value = values)
  })
  
  output$boxplot <- renderPlot({
    ggplot(data = trans_sampledf_long(), aes(x = dataset, y = values)) + 
      geom_boxplot(aes(color = dataset)) +
      geom_jitter(aes(x = dataset, y = values, alpha = .2, color = dataset), position=position_jitter(0.04)) +
      #coord_cartesian(ylim =c(0.1, 1.2)) +
      geom_hline(yintercept=mean(trans_sampledf_long()$values), linetype=2, color = "black") +
      labs(title = "Sample Data") +
      theme(legend.position = "none", axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  })
  
  runTest2 <- reactive({aov(values ~ dataset, data = trans_sampledf_long())})
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