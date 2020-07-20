library(shiny)
library(shinythemes)
library(mathjaxr)
library(tidyr)
library(ggplot2)
library(broom)
library(shinyBS)

# add text at end for discussion of step down tests and bonferroni correction?
# add text at beginning explaining why we don't just do pairwise test (family-wise error increases)
# possible equations ot include, variance eqs, ANOVa model eq, reading ANOVA test table output

# input to control sample size ?
# button that leads straight to "insuff evidence" conclusion ?

# maybe reiterate vocab on resources page

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
                                                                                          title = "The F-stat is a ratio of the between and within groups variances. When F is large we are prompted to reject the null hypothesis and conclude that there is at least one difference between th egroup means",
                                                                                          placement = "bottom", trigger = "hover"), "and the ANOVA test output"),
                                   p("3. Predict how manipulation of between and withing group variances will affect the F-statistic
                            and the ANOVA test output"),
                                   br(),
                                   p("ANOVA is a method used in statistical anylsis to make inferences about numerical data. In particular 
                            ANOVA is used to question whether there is a meaningful difference between the groups in question. To
                            do this ANOVA tests the alternative hypothesis that at least one of the group means is truly different
                            from the others against the null hypothesis that there is no difference between the group means. In this sense, 
                            ANOVA can be thought of as generalizing the two sample", tipify(strong("t-test", style = "color:#00B5E5"),
                                                                                            title = "define t-test i.e assess associations between variables...",
                                                                                            placement = "bottom", trigger = "hover"),
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
                          br(), br(), br(), br(), br(), br(),
                          p("Created by Samantha Owusu-Antwi for Duke University 'Creating Interactive Learning Tools' Project, Summer 2020", style = "text-align:center")),
                 
                 ##--------------------------------------------------------tab 1
                 tabPanel("Step 1: Create the Population", value = 1,
                          fluidRow(
                            column(offset = 2, width = 8, style="background-color:#F1F0F0; padding:20px; border-radius:10px",
                                   p("ANOVA tests the following hypotheses:"),
                                   #; padding:20px; border-radius:10px"
                                   p("$H_0$: The means of all groups are equal $(\\mu_1 = \\mu_2 = ... = \\mu_K)$", 
                                     br(),
                                     "$H_1$: At least one of the means $(\\mu_i)$ is not equal to the others", style = "text-align:center"),
                                   br(),
                                   p("Below you have the opportunity to manipulate the skew,", tipify(strong("between group variance", style = "color:#00B5E5"),
                                                                                                      title = "ANOVA is concerend with two variances. This refers to how group means vary around the overall mean",
                                                                                                      placement = "top", trigger = "hover"), 
                                     ", and the", tipify(strong("within group variances", style = "color:#00B5E5"),
                                                         title = "ANOVA is concerend with two variances. This refers to how the individual observations of a group vary around the mean of that group",
                                                         placement = "top", trigger = "hover"), "of the population data we have simulated. These two variances are estimates of the population variance, $\\sigma^2$, 
                                    if the null hypothesis is true. Normally, true population parameters are not known.", strong("ANOVA assumes each group's population density is approximately normal")) 
                                   
                            )
                            
                          ),
                          
                          br(),
                          hr(),
                          
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
                                   ## put w/in groups var input group here
                                   h3(strong("Within group variance")),
                                   wellPanel(
                                     p("Manipulate the sliders to increase or decrease the within group variance of each curve 
                                     by a factor of the slider value. This will alter the spread of each density curve. 
                                     Recall ANOVA assumes these variances are approximately equal",
                                       style = "color:grey"),
                                     br(),
                                     
                                     sliderInput(inputId = "sd1.1",
                                                 label = p("Group 1", style = "color:red"),
                                                 min = 0.75, max = 1.25, value = 1),
                                     sliderInput(inputId = "sd1.2",
                                                 label = p("Group 2", style = "color:green"),
                                                 min = 0.75, max = 1.25, value = 1),
                                     sliderInput(inputId = "sd1.3",
                                                 label = p("Group 3", style = "color:blue"),
                                                 min = 0.75, max = 1.25, value = 1)
                                   ),
                                   br(),
                                   
                            ),
                            
                            column(width = 6,
                                   br(),
                                   p(plotOutput(outputId = "curve")),
                                   #p(verbatimTextOutput(outputId = "aovTest")),
                                   #move assumptions static text here
                                   fluidRow(
                                     column(width = 12, style="background-color:#F7F7F7; padding:20px; border-radius:5px; border: 1px solid #BBBBBB",
                                       p(strong("ANOVA assumes:"), br(),
                                         "1. Independent Observations", br(),
                                         "2. Approximately normal population distributions within each group", br(),
                                         "3. Approximately equal within group variances for all groups"))
                                     ),
                                   #p("At the $\\alpha = .05$ level this F-stat corresponds to a p-value that suggests there is:", 
                                     #textOutput(outputId = "concl1")),
                                   br(),
                                   br(),
                            ),
                            ## put shape input group here
                            column(width = 3,
                                   h3(strong("Shape")),
                                   wellPanel(
                                     p("Manipulate the skew of each population density curve. Recall ANOVA assumes the 
                                  population density of each group is approximately normal", 
                                       style = "color:grey"),
                                     br(),       
                                     
                                     selectInput(inputId = "skew1", 
                                                 label = p("Group 1", style = "color:red"),
                                                 choices = c("Normal" = "norm",
                                                             "Right Skewed" = "rskew",
                                                             "Left Skewed" = "lskew"),
                                                 selected = "norm"),
                                     selectInput(inputId = "skew2", 
                                                 label = p("Group 2", style = "color:green"),
                                                 choices = c("Normal" = "norm",
                                                             "Right Skewed" = "rskew",
                                                             "Left Skewed" = "lskew"),
                                                 selected = "norm"),
                                     selectInput(inputId = "skew3", 
                                                 label = p("Group 3", style = "color:blue"),
                                                 choices = c("Normal" = "norm",
                                                             "Right Skewed" = "rskew",
                                                             "Left Skewed" = "lskew"),
                                                 selected = "norm")))
                          )
                 ),
                 
                 ##--------------------------------------------------------tab 2
                 tabPanel("Step 2: Use Samples to Visualize the F-Statistic", value = 2,
                          fluidRow(
                            column(offset = 2, width = 8, 
                                   p(strong("ANOVA assumes approximate normality among groups."), "However ANOVA is relatively robust against
                                departures from normality. In fact, as long as sample sizes are", tipify(el = strong("'large enough'", style = "color:#00B5E5"),
                                                                                                         title = "in this case approximately greater than 10 in each group",
                                                                                                         placement = "top", trigger = "hover"), "the conclusions
                                of ANOVA may still be valid, even if the underlying population distributions are skewed"), br(),
                                   
                                   p(strong("ANOVA also assumes that groups have roughly equal variability."), "ANOVA is not robust against violations
                                of this assumption and the results of ANOVA may not be valid if $s_{max} \\geq 2s_{min}$"),   
                                   style="background-color:#F1F0F0; padding:20px; border-radius:10px")
                            
                            #change LaTeX size with $\\___{...}$
                            
                            #If other asusmptions validated result may not be valid
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
                                                 selected = "inc"),
                                     fluidRow(
                                       column(offset = 6, width = 6, 
                                              tipify(el = p(em(strong("What's happening?")), style = "text-align:right; color:#00B5E5; font-size:12px"),
                                                     title = "Notice how increasing the between group variance increases the F-stat and decreasing it decreases the F-stat",
                                                     placement = "bottom", trigger = "hover")))
                                   ),
                                   
                                   h3(strong("Within group variance")),
                                   wellPanel(
                                     p("Manipulate the sliders to increase or decrease the within group variance of each sample 
                                              by a factor of the slider value. This will alter the spread of the samples' data points. 
                                              Recall ANOVA assumes these variances are approximately equal", 
                                       style = "color:grey"), 
                                     
                                     br(),
                                     sliderInput(inputId = "sd2.1",
                                                 label = p("Group 1", style = "color:red"),
                                                 min = 0.75, max = 1.25, value = 1),
                                     sliderInput(inputId = "sd2.2",
                                                 label = p("Group 2", style = "color:green"),
                                                 min = 0.75, max = 1.25, value = 1),
                                     sliderInput(inputId = "sd2.3",
                                                 label = p("Group 3", style = "color:blue"),
                                                 min = 0.75, max = 1.25, value = 1),
                                     fluidRow(
                                       column(offset = 6, width = 6,
                                              tipify(el = p(em(strong("What's happening?")), style = "text-align:right; color:#00B5E5; font-size:12px"),
                                                     title = "Try moving the slider from one end to the other. Notice how decreasing within group variance generally increases the F-stat and increasing it generally decreases the F-stat",
                                                     placement = "top", trigger = "hover")))
                                   )
                            ),
                            
                            column(width = 6,
                                   br(),
                                   br(),
                                   fluidRow(
                                     tabsetPanel(
                                       tabPanel("Whole Graph", column(width = 12, plotOutput(outputId = "boxplot")),
                                                fluidRow(
                                                  column(offset = 1, width = 4,
                                                         tipify(el = p(em(strong("What does this graph show?")), style = "text-align:left; color:#00B5E5; font-size:12px"),
                                                                title = "Here the between groups variance can be thought of as how the median of a boxplot varies from the overall mean, the solid black line. The within groups variance can be thought of as how the datapoints of a sample vary from the median of that sample.",
                                                                placement = "top", trigger = "hover"))
                                                )),
                                       tabPanel("Focus on Between Groups Variance", column(width = 12, plotOutput(outputId = "toggleBtw")),
                                                fluidRow(
                                                  column(offset = 1, width = 4,
                                                         tipify(el = p(em(strong("What does this graph show?")), style = "text-align:left; color:#00B5E5; font-size:12px"),
                                                                title = "Here the between groups variance can be thought of as how the median of a boxplot, represented by a point in this case, varies from the overall mean, the solid black line.",
                                                                placement = "top", trigger = "hover"))
                                                )),
                                       tabPanel("Focus on Within Groups Variance", column(width = 12, plotOutput(outputId = "toggleWin")),
                                                fluidRow(
                                                  column(offset = 1, width = 4,
                                                         tipify(el = p(em(strong("What does this graph show?")), style = "text-align:left; color:#00B5E5; font-size:12px"),
                                                                title = "Here the within groups variance can be thought of as how the datapoints of a sample vary from the median of that sample, the corresponding solid lines.",
                                                                placement = "top", trigger = "hover"))
                                                )
                                       )
                                     )
                                   ),
                                   fluidRow(
                                     p(verbatimTextOutput(outputId = "aovTest")),
                                     ##move aovTest output here
                                     p(),
                                     p("At the $\\alpha = .05$ level this F-stat corresponds to a p-value that suggests there is:",
                                       textOutput(outputId = "concl")),
                                     fluidRow(
                                       column(offset = 9, width = 3,
                                              tipify(el = p(em(strong("What's happening?")), style = "text-align:right; color:#00B5E5; font-size:12px"),
                                                     title = "If the sample means are far apart there is evidence against the null hypothesis that the mean value of response is the same for all groups. But what is considered far appart? The F-stat quantifies this",
                                                     placement = "bottom", trigger = "hover")))
                                   )
                            ),
                           
                            column(width = 3,
                                   h3(strong("What is the F-statistic?")),
                                   wellPanel(
                                     #Non-reactive text explaining what F-stat is...
                                     p("If the sample means vary around the overall mean more that the individual observations vary around
                                     the sample means we have evidence that the corresponding popualtion means are different. We formally compare
                                    these variances with the F-stat", br(), br(),
                                       "If there is", tipify(el = strong("no treatment effect", style = "color:#00B5E5"),
                                                             title = "No treatment effect implies that the null hypothesis, that the true mean is the same for all groups, is true",
                                                             placement = "top", trigger = "hover"),
                                       "the F-stat will be very close to 1"),
                                     
                                     #F-stat equation
                                     p("$\\large{F = \\frac{{{s^2}_B}/ndf}{{{s^2}_W}/ddf}}$", style = "text-align:center"),
                                     p("$\\bullet$ ${s^2}_B$ is the between groups variance", tipify(strong("*", style = "color:#00B5E5"),
                                                                                                     title = "the between groups variance is proportional to F",
                                                                                                     placement = "top", trigger = "hover"), br(),
                                       "$\\bullet$ ${s^2}_W$ is the within groups variance", tipify(strong("*", style = "color:#00B5E5"),
                                                                                                    title = "the within groups variance is inversely proportional to F",
                                                                                                    placement = "top", trigger = "hover"), br(),
                                       "$\\bullet$ $ndf$ is the", tipify(strong("numerator degrees of freedom", style = "color:#00B5E5"),
                                                                         title = "The degrees of freedom corresponding to the between groups variance, calculated as total number of groups - 1",
                                                                         placement = "left", trigger = "hover"), br(),
                                       "$\\bullet$ $ddf$ is the", tipify(strong("denominator degrees of freedom", style = "color:#00B5E5"),
                                                                         title = "The degrees of freedom corresponding to the within groups variance, calculated as total number of observations - the number of groups",
                                                                         placement = "left", trigger = "hover")),
                                     br(),
                                     
                                     p("Here the F-stat is:"),
                                     verbatimTextOutput(outputId = "FTest"),
                                     br())
                            )
                            
                          )
                          
                 ),
                 
                 tabPanel("Resources Page", value = 3,
                          
                          h1("Equations"),
                          p("contents")
                 ),
                 
                 ##---------------------------------------------------------quiz on boxplots comparing F-stats  
                 tabPanel("Quiz", value = 4,
                          fluidRow(
                            #tags$iframe(src = "shiny apps.io link to quiz",
                            #width = "1000", height = "1500",
                            #frameBorder="0")
                          )
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
      coord_cartesian(xlim = c(-.125, 1.25), ylim = c(0,8)) +
      ggtitle("Population Distributions") +
      theme(legend.position = "none", 
            axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank()) 
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
  
  ##--------------------------------------------------------------F-Stat Tab
  ##recall that sample1, sample2, sample3 already defined; also dataset with all is sampledf_long()
 
  output$boxplot <- renderPlot({
    ggplot(data = sampledf_long(), aes(x = dataset, y = values)) + 
      geom_boxplot(aes(color = dataset)) +
      geom_jitter(aes(x = dataset, y = values, alpha = .2, color = dataset), position=position_jitter(0.04)) +
      #coord_cartesian(ylim =c(0.1, 1.2)) +
      geom_hline(yintercept=mean(sampledf_long()$values), linetype=2, color = "black") +
      labs(title = "Sample Data") +
      theme(legend.position = "none", axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  })
  
  output$toggleBtw <- renderPlot({
    ggplot(data = sampledf_long(), aes(x = dataset, y = values)) +
      stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                   geom = "point", shape=16, size=5, aes(color = dataset)) +
      #coord_cartesian(ylim =c(0.1, 1.2)) +
      geom_hline(yintercept=mean(sampledf_long()$values), linetype=1, color = "black") +
      labs(title = "Sample Data") +
      theme(legend.position = "none", axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  })
  
  output$toggleWin <- renderPlot({
    ggplot(data = sampledf_long(), aes(x = dataset, y = values)) + 
      geom_jitter(aes(x = dataset, y = values, alpha = .2, color = dataset), position=position_jitter(0.04)) +
      stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                   geom = "crossbar", width = 0.5, aes(color = dataset)) +
      #coord_cartesian(ylim =c(0.1, 1.2)) +
      labs(title = "Sample Data") +
      theme(legend.position = "none", axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  })
  
  runTest <- reactive({aov(values ~ dataset, data = sampledf_long())})
  output$aovTest <- renderPrint ({
    print(summary(runTest()))
  })
  output$FTest <- renderPrint ({
    print(summary(runTest())[[1]][["F value"]][[1]])
  })
  
  output$concl <- renderText({
    if(tidy(runTest())$p.value[1] < 0.05) {
      return("sufficient evidence to conclude that there is at least one difference between the group means.")
    } else {
      return("insufficent evidence to conclude that there is at least one difference between the group means.")
    }
  })
}

shinyApp(ui = ui, server = server)