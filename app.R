library(shiny)
library(shinythemes)
library(mathjaxr)
library(tidyr)
library(ggplot2)
library(broom)
library(shinyBS)

ui <- navbarPage(theme = shinytheme("lumen"),
                 
                 title = "ANOVA",
                 tabPanel("About",
                          h1("About the App"),
                          withMathJax(),
                          tags$script(
                            "MathJax.Hub.Config({
                                tex2jax: {
                                inlineMath: [['$','$'], ['\\(','\\)']],
                                processEscapes: true
                                }
                                });"
                          ),
                          #style = "font-size:14px" for size later?
                          tags$style(
                            HTML("
                                 .tooltip > .tooltip-inner {
                                 background-color: #F1F0F0;
                                 color: #000000;
                                 border: 1px solid #F1F0F0;
                                 text-align: left;
                                 }
                                 ")),
                          fluidRow(
                            column(offset = 1, width = 6,
                                   p("Welcome! This interactive learning tool is brought to you by Duke Shiny-Ed. In this particular app 
                            you will be able to explore", tipify(strong("ANOVA", style = "color:#00B5E5"), 
                                                                 title = "Hypothesis test which analyzes variance to make inferences about means. Tests the null that all group means are equal and the alternative that at least one of the means are different, not necessarily that all are unequal", 
                                                                 placement = "right", trigger = "hover"), 
                                     ", or Analysis of Variance. This app is designed to gradually introduce you to the different aspects of ANOVA and it concludes with a quiz so that you 
                            can test what you've learned! Hopefully, after using this
                            app, you will be able to:"),
                                   p("1. Examine how violations of the", tipify(strong("assumptions of ANOVA", style = "color:#00B5E5"),
                                                                                title = "1)Independent observations 2)Approximately normal population distributions within each group 3)Approximately equal within group variances for all groups", 
                                                                                placement = "right", trigger = "hover"), 
                                     "affect the ANOVA test output"),
                                   p("2. Understand the relationship between the", tipify(strong("F-statistic", style = "color:#00B5E5"),
                                                                                          title = "text here",
                                                                                          placement = "bottom", trigger = "hover"), "and the ANOVA test output"),
                                   p("3. Predict how manipulation of between and withing group variances will affect the F-statistic
                            and the ANOVA test output"),
                                   br(),
                                   p("ANOVA is a method used in statistical anylsis to make inferences about numerical data. In particular 
                            ANOVA is used to question whether there is a meaningful difference between the groups in question. To
                            do this ANOVA tests the alternative hypothesis that at least one of the group means is truly different
                            from the others against the null hypothesis that there is no difference between the group means. In this sense, 
                            ANOVA can be thought of as generalizing the", tipify(strong("two sample t-test", style = "color:#00B5E5"),
                                                                                 title = "define t-test i.e assess associations between variables...",
                                                                                 placement = "right", trigger = "hover"),
                                     "to more that two categories!"),
                                     ),
                            
                            column(width = 5,
                                   p("If you haven't already noticed, while interacting with this app you will routinely encounter text that
                                     is highlighted blue like this:", tipify(el = strong(em("What's happening?", style = "color:#00B5E5; font-size:13px")),
                                                                            title = "This is an example",
                                                                            placement = "top", trigger = "hover"), 
                                    "These are designed to help you navigate the material being presented. Hover over them to view definitions 
                                    of commonly used terms and observations designed to guide you through the exploration!")
                                   ),
                          ),
                          hr(),
                          
                          h1("Acknowledgements"),
                          p("contents"),
                          p("Created by Samantha Owusu-Antwi for 'Creating Interactive Learning Tools' Project, Summer 2020")),
                 
  ##--------------------------------------------------------tab 1
                 tabPanel("Step 1: Create the Population", value = 1,
                          fluidRow(
                            column(offset = 2, width = 8, style="background-color:#F1F0F0; padding:20px; border-radius:10px",
                                   p("ANOVA tests the following hypotheses:"),
                                   #; padding:20px; border-radius:10px"
                                   p("$H_0$: The means of all groups are equal $(\\mu_1 = \\mu_2 = ... = \\mu_K)$", 
                                     br(),
                                     "$H_1$: At least one of the means is different", style = "text-align:center"),
                                   br(),
                                   p("Below you have the opportunity to manipulate the skew,", tipify(strong("between group variance", style = "color:#00B5E5"),
                                                                                                     title = "define btw var here",
                                                                                                     placement = "top", trigger = "hover"), 
                                   ", and the", tipify(strong("within group variances", style = "color:#00B5E5"),
                                                      title = "define within var here",
                                                      placement = "top", trigger = "hover"), "of the population data we have simulated. Normally, true population parameters 
                                    are not known.", strong("Recall that ANOVA assumes each group's population density is approximately normal")) 
                      #talk about how normal assumption is robust if sample ANOVA is relatively robust against departures from Normaltiy 
                      #unless the sample sizes are are small within each group. Therefore, as long as the sample sizes in each group are l
                      #arge enough, the p-values from ANOVA are still valid even if the distribution of the response in each group is 
                      #extremely skewed or long-tailed. If asusmptions validated result may not be valid
                                )
                            #Info about first step; applicable assumption; any eqs?
                            #talk about how if assumptions are not met ANOVA conclusion may not be valid"
                          ),
                          
                          br(),
                          hr(),
                          br(),
                          
                          fluidRow(
                            
                            column(width = 3,
                              h3(strong("Between Group Variance")),
                              wellPanel(
                                p("Toggle between 'Reduced' and 'Increased' to translate the population density curves, 
                                  altering the distance between thier means", 
                                  style = "color:grey"),
                                br(),
                              #sliderInput(inputId = "btwsd1",
                              #label = NULL,
                              #min = 0, max = 1, value = 1, step = .001),
                              #toggle btw group variance
                              selectInput(inputId = "btwsd1",
                                          label = NULL,
                                          choices = c("Increased Between Group Variance" = "inc",
                                                      "Reduced Between Group Variance" = "dec"),
                                          selected = "inc")
                              ),
                              
                              h3(strong("Shape")),
                              wellPanel(
                                p("Manipulate the skew of each population density curve. Recall ANOVA assumes the 
                                  population density of each group is approximately normal", 
                                  style = "color:grey"),
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
                              
                            ),
                            
                            column(width = 6,
                              p(plotOutput(outputId = "curve")),
                              p(verbatimTextOutput(outputId = "aovTest1")),
                                br(),
                                
                                p("At the $\\alpha = .05$ level this F-stat corresponds to a p-value that suggests there is:", 
                                textOutput(outputId = "concl1")),
                                br(),
                                br(),
                            ),
                            
                            column(width = 3,
                                   h3(strong("Within group variance")),
                                   wellPanel(
                                     p("Manipulate the sliders to increase or decrease the within group variance of each curve 
                                     by a factor of the slider value. This will alter the spread of each density curve. 
                                     Recall ANOVA assumes these variances are approximately equal",
                                       style = "color:grey"),
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
                                  )
                          )
                 ),
  
  ##--------------------------------------------------------tab 2
                 tabPanel("Step 2: Use Samples to Visualize the F-Statistic", value = 2,
                          fluidRow(
                            column(offset = 2, width = 8, style="background-color:#F1F0F0; padding:20px; border-radius:10px")
                            ),
                          br(),
                          hr(),
                          fluidRow(
                            
                            column(width = 3,
                              h3(strong("Between Group Variance")),
                              wellPanel(
                                p("Toggle between 'Reduced' and 'Increased' to translate the samples, altering the distance between thier medians", style = "color:grey"), 
                                br(),
                              #sliderInput(inputId = "btwsd2",
                              #label = NULL,
                              #min = 0, max = 1, value = 0, step = .001), ## <- set btwsd2 to a value that doesn't initilaly maniuplate sample data
                              selectInput(inputId = "btwsd2",
                                          label = NULL,
                                          choices = c("Increased Between Group Variance" = "inc",
                                                      "Reduced Between Group Variance" = "dec"),
                                          selected = "dec"),
                              tipify(el = p(em(strong("What's happening?")), style = "text-align:right; color:#00B5E5; font-size:12px"),
                                      title = "Notice how increasing the between group variance increases the F-stat and decreasing it decreases the F-stat",
                                      placement = "bottom", trigger = "hover")
                              ),
                              
                              h3(strong("Within group variance")),
                              wellPanel(
                                p("Manipulate the sliders to increase or decrease the within group variance of each sample 
                                              by a factor of the slider value. This will alter the spread of the samples' data points. 
                                              Recall ANOVA assumes these variances are approximately equal", 
                                              style = "color:grey"), 
                            
                              br(),
                              sliderInput(inputId = "sd2.1",
                                          label = p("Sample from Curve 1", style = "color:red"),
                                          min = 0.75, max = 1.25, value = 1),
                              sliderInput(inputId = "sd2.2",
                                          label = p("Sample from Curve 2", style = "color:green"),
                                          min = 0.75, max = 1.25, value = 1),
                              sliderInput(inputId = "sd2.3",
                                          label = p("Sample from Curve 3", style = "color:blue"),
                                          min = 0.75, max = 1.25, value = 1),
                              fluidRow(
                                column(offset = 6, width = 6,
                                      tipify(el = p(em(strong("What's happening?")), style = "text-align:right; color:#00B5E5; font-size:12px"),
                                     title = "Notice how decreasing within group variance generally increases the F-stat and increasing it generally decreases the F-stat",
                                     placement = "top", trigger = "hover")))
                              )
                            ),
                            
                            column(width = 6,
                              br(),
                              br(),
                              fluidRow(column(width = 12, plotOutput(outputId = "boxplot"))),
                              fluidRow(
                                br(),
                                p("At the $\\alpha = .05$ level this F-stat corresponds to a p-value that suggests there is:",
                                  textOutput(outputId = "concl2"))
                                )
                             ),
                            
                            column(width = 3,
                                h3(strong("What is the F-statistic?")),
                                wellPanel(
                                  p("F-stat equation"),
                                  br(),
                                   p("Non-reactive text explaining what F-stat is..."),
                                   br(),
                                   
                                   p("Indicate that below is the F-stat:"),
                                   verbatimTextOutput(outputId = "aovTest2"),
                                   br(),
                                   br())
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
      coord_cartesian(xlim = c(-.125, 1.25), ylim = c(0,35)) +
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
      gather(key = sample, value = values)
  })
  
  output$boxplot <- renderPlot({
    ggplot(data = trans_sampledf_long(), aes(x = sample, y = values)) + 
      geom_boxplot(aes(color = sample)) +
      geom_jitter(aes(x = sample, y = values, alpha = .2, color = sample), position=position_jitter(0.04)) +
      #coord_cartesian(ylim =c(0.1, 1.2)) +
      geom_hline(yintercept=mean(trans_sampledf_long()$values), linetype=2, color = "black") +
      labs(title = "Sample Data") +
      theme(legend.position = "none", axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  })
  
  runTest2 <- reactive({aov(values ~ sample, data = trans_sampledf_long())})
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