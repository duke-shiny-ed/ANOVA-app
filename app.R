library(shiny)
library(shinythemes)
library(mathjaxr)
library(tidyr)
library(ggplot2)
library(broom)
library(shinyBS)
library(plyr)

# add text at end for discussion of step down tests and bonferroni correction?
# add text at beginning explaining why we don't just do pairwise test (family-wise error increases)
# possible equations ot include, variance eqs, ANOVa model eq, reading ANOVA test table output

# button that leads straight to "insuff evidence" conclusion ?

# maybe reiterate vocab on resources page

ui <- navbarPage(theme = shinytheme("lumen"),
                 
                 title = "ANOVA",
                 tabPanel("About",
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
                            column(offset = 1, width = 5,
                                   h2("About the App")),
                            column(width = 5,
                                   h2("An Introduction to ANOVA"))
                          ),
                          fluidRow(
                            column(offset = 1, width = 5,
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
                                   ## put more general info here
                                   p("If you haven't already noticed, while interacting with this app you will routinely encounter text that
                                     is highlighted blue like this:", tipify(el = strong(em("What's happening?", style = "color:#00B5E5; font-size:13px")),
                                                                             title = "This is an example! Hover over any text highlighted in blue like this to view helpful information",
                                                                             placement = "top", trigger = "hover"), 
                                     "These are designed to help you navigate the material being presented. Hover over them to view definitions 
                                    of commonly used terms and observations designed to guide you through the exploration!")
                                   
                            ),
                            ## put intro to ANOVA info here
                            column(width = 5,
                                   p("ANOVA is a method used in statistical anylsis to make inferences about numerical data. In particular 
                            ANOVA is used to question whether there is a meaningful difference between the groups in question. To
                            do this ANOVA tests the alternative hypothesis that at least one of the group means is truly different
                            from the others against the null hypothesis that there is no difference between the group means. In this sense, 
                            ANOVA can be thought of as generalizing the two sample", tipify(strong("t-test", style = "color:#00B5E5"),
                                                                                            title = "A hypothesis test used to compare two means",
                                                                                            placement = "top", trigger = "hover"),
                                     "to more that two categories!"),
                                   p("So why don't we just use the two sample t-test multiple times? Conclusions drawn from repetitive pairwise testing 
                                     can be misleading. In particular this practice will lead to an inflated", tipify(strong("'family-wise'", style = "color:#00B5E5"),
                                                                                                                      title = "The probability that we fail to arrive at the correct conclusion at least once when we conduct multiple pairwise tests.",
                                                                                                                      placement = "bottom", trigger = "hover"), 
                                     "error rate. In other words if you test anything enough times you're bound reach the wrong conclusion! So we generally wait until 
                                     after ANOVA suggests there is at least one difference among the groups being studied before conducting these pairwise 
                                     tests, while", tipify(strong("correcting", style = "color:#00B5E5"),
                                                           title = "Refers to the Bonferroni correction where the significance level, alpha, is divided by the total number of pairwise tests to be performed before it is used."), 
                                     "for the family-wise error rate, to determine where exactly the difference lies.")
                            ),
                          ),
                          hr(),
                          
                          h2("Acknowledgements"),
                          p("contents"),
                          br(), br(), br(), br(), br(), br(), br(), br(),
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
                                                                                                      title = "ANOVA is concerned with two variances. This refers to how group means vary around the overall mean",
                                                                                                      placement = "top", trigger = "hover"), 
                                     ", and the", tipify(strong("within group variances", style = "color:#00B5E5"),
                                                         title = "ANOVA is concerend with two variances. This refers to how the individual observations of a group vary around the mean of that group",
                                                         placement = "top", trigger = "hover"), "of the population data we will be working with. These two variances are estimates of the population variance, $\\sigma^2$, 
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
                                   ## add "what does graph show?" here
                                   fluidRow(
                                     column(width = 4,
                                            tipify(el = p(em(strong("What does this graph show?")), style = "text-align:left; color:#00B5E5; font-size:12px"),
                                                   title = "These are the population distributions of the response variable of interest. The vertical dotted lines are the mean values of the response for the corresponding groups. Using ANOVA we will be exploring if there is a significant difference in these means.",
                                                   placement = "top", trigger = "hover"))
                                   ),
                                   p(),
                                   #p(verbatimTextOutput(outputId = "aovTest")),
                                   #move assumptions static text here
                                   fluidRow(
                                     column(width = 12, style="background-color:#F7F7F7; padding:20px; border-radius:5px; border: 1px solid #C6C5C5",
                                       p(strong("ANOVA assumes:"), br(),
                                         "1. Independent Observations", p(), p(),
                                         "2. Approximately normal population distributions within each group", uiOutput("assumption1"),
                                         "3. Approximately equal within group variances for all groups", uiOutput("assumption2")))
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
                                   p("Now that out population data is set, let's examine the random samples taken from the populations."),
                                   p(strong("ANOVA assumes approximate normality among groups."), "However ANOVA is relatively robust against
                                departures from normality. In fact, as long as sample sizes are", tipify(el = strong("'large enough'", style = "color:#00B5E5"),
                                                                                                         title = "in this case approximately greater than 10 in each group",
                                                                                                         placement = "top", trigger = "hover"), "the conclusions
                                of ANOVA may still be valid, even if the underlying population distributions are skewed"), br(),
                                   
                                   p(strong("ANOVA also assumes that groups have roughly equal variability."), "ANOVA is not robust against violations
                                of this assumption and the results of ANOVA may not be valid if $s_{max} \\geq 2s_{min}$, or if the maximum group variance is
                                     greater than or equal to double the minimum group variance"),   
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
                                     p("Manipulate the sliders to increase or decrease the within group variance of each sample's population 
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
                                     numericInput("n", "Choose a sample size", 
                                                  min = 2, max = 20000, value = 100, step = 1),
                                     tabsetPanel(
                                       tabPanel("Whole Graph", column(width = 12, plotOutput(outputId = "boxplot")),
                                                fluidRow(
                                                  column(offset = 1, width = 4,
                                                         tipify(el = p(em(strong("What does this graph show?")), style = "text-align:left; color:#00B5E5; font-size:12px"),
                                                                title = "These graphs all depict parts of the sample distributions. Here the between groups variance can be thought of as how the median of a boxplot varies from the overall mean, the solid black line. The within groups variance can be thought of as how the datapoints of a sample vary from the median of that sample.",
                                                                placement = "top", trigger = "hover"))
                                                )),
                                       tabPanel("Focus on Within Groups Variance", column(width = 12, plotOutput(outputId = "toggleWin")),
                                                fluidRow(
                                                  column(offset = 1, width = 4,
                                                         tipify(el = p(em(strong("What does this graph show?")), style = "text-align:left; color:#00B5E5; font-size:12px"),
                                                                title = "These graphs all depict parts of the sample distributions. Here the within groups variance can be thought of as how the datapoints of a sample vary from the median of that sample, the corresponding solid lines.",
                                                                placement = "top", trigger = "hover"))
                                                )
                                       ),
                                       
                                       tabPanel("Focus on Between Groups Variance", column(width = 12, plotOutput(outputId = "toggleBtw")),
                                                fluidRow(
                                                  column(offset = 1, width = 4,
                                                         tipify(el = p(em(strong("What does this graph show?")), style = "text-align:left; color:#00B5E5; font-size:12px"),
                                                                title = "These graphs all depict parts of the sample distributions. Here the between groups variance can be thought of as how the median of a boxplot, represented by a point in this case, varies from the overall mean, the solid black line.",
                                                                placement = "top", trigger = "hover"))
                                                ))
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
                                                     placement = "top", trigger = "hover")))
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
                                       "$\\bullet$ ${s^2}_W$ is a pooled estimate of the within groups variances", tipify(strong("*", style = "color:#00B5E5"),
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
                 
                 tabPanel("Resources", value = 3,
                          
                          h1("Equations"),
                          fluidRow(
                          column(width = 11,
                            h4("The following are some equations and other helpful resources that may not have been referenced in the app that you may come across when dealing with ANOVA."),
                            h4("You", em("DO NOT"), "need to know these equations in order to interact with this app")
                          )
                          ), br(),
                          fluidRow(
                            column(offset = 2, width = 3,
                                   h3(strong("Bonferroni correction"), style = "text-align:center"),
                                   p("$\\Large{\\alpha^{*} = \\frac{\\alpha}{k}}$", style = "text-align:center;background-color:yellow"),
                                   h4("$\\bullet$ k is the total number of pair-wise tests", style = "text-align:center")
                                  ),
                            
                            column(offset = 1, width = 4,
                                   h3(strong("The Between Groups Variance"), style = "text-align:center"),
                                   p("$\\Large{{s^{2}}_B  =  \\frac{n_1{(\\overline{y}_1 - \\overline{y})}^{2}   +   n_2{(\\overline{y}_2 - \\overline{y})}^{2}   +    n_K{(\\overline{y}_K  -  \\overline{y})}^{2}}{K - 1}}$", style = "text-align:center;background-color:yellow"),
                                   h4("$\\bullet$ $K$ is the total number of groups each with a corresponding sample size $n_1, n_2, ... n_K$"),
                                   h4("$\\bullet$ $\\overline{y}_1$, $\\overline{y}_2$, ... $\\overline{y}_K$ are the means of groups $1$, $2$, and $K$ respectively"),
                                   h4("$\\bullet$ $\\overline{y}$ is the", em("overall"), "mean of all observations")
                            )
                          ),
                          br(),
                          
                          fluidRow(
                            column(offset = 2, width = 3,
                              h3(strong("The ANOVA model"), style = "text-align:center"),
                            p("$\\Large{y_{ij} = \\mu_i + \\epsilon_{ij}}$", style = "text-align:center; background-color:yellow"),
                            h4("$\\bullet$ $y_{ij}$ is an individual observation, the response of a subject $j$ in group $i$", style = "text-align:left"),
                            h4("$\\bullet$ $\\mu_i$ is the mean of group $i$", style = "text-align:left"),
                            h4("$\\bullet$ $\\epsilon_{ij}$ is a specific random error term, the error of an individual about each group mean", style = "text-align:left")
                            ),
                            
                            column(offset = 1, width = 4,
                                   h3(strong("Pooled estimate of Within Groups Variance"), style = "text-align:center"),
                                   p("$\\Large{{s^{2}}_w  =  \\frac{(n_1  -  1){s^{2}_1}  +  (n_2  -  1){s^{2}_2}  +  ...  +  (n_K  -  1){s^{2}_K}}{n - K}}$", style = "text-align:center;background-color:yellow"),
                                   h4("$\\bullet$ $K$ is the total number of groups each with a corresponding sample size $n_1, n_2, ... n_K$"),
                                   h4("$\\bullet$ $n$ is the total number of samples, $n = n_1 + n_2 + ... + n_K$"),
                                   h4("$\\bullet$ Where each group's variance, $s_1$, $s_2$, and $s_K$, is"),
                                   p("$\\large{{s^{2}}_i = \\frac{\\sum_{j=1}^{n_i} {(y_{ij}  -  \\overline{y}_i)}^{2}}{n_i  -  1}}$", style = "text-align:center"),
                                   
                            )
                            
                          ),
                          br(),
                          br(),
                          
                          h1("Other Resources"),
                          h4("When running an ANOVA test in R you will encounter the following table. It is important to understand what information
                            is being presented"),
                          br(),
                          fluidRow(
                            column(offset = 2, width = 8,
                                   h3(strong("The ANOVA Test Output in R"))
                                   )
                              ),
                          fluidRow(
                            column(width = 12, align = "center",
                                   img(src = "output.png", width = 1000, height = 250)
                              )
                            ),
                          
                          fluidRow(
                            column(offset = 2, width = 8,
                                   h3("The 'Source' column clarifies the source of the variance in question"),
                                   h4("$\\bullet$ Sometimes the grouping variable is written in place of 
                                      'Between' and 'Residuals' is written in place of 'Within'"),
                                   br(),
                                   
                                   h3("df refers to the degrees of freedom"),
                                   h4("$\\bullet$ $ndf = K - 1$"),
                                   h4("$\\bullet$ $ddf = n - K$"),
                                   h4("Where $n$ is the total number of samples and $K$ is the total number of groups"),
                                   br(),
                                   
                                   h4("${s_B}^{2}$ is the between groups variance, the sum of the squared differences of 
                                      the observations from the overall mean"),
                                   h4("${s_W}^{2}$ is a pooled estimated of the within groups variances, the sum of the squared differences between
                                   the observations of a given grop and that group's mean."),
                                   )
                          ),
                          br(), br(), br(), br(), br()
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
  ##--------------------------------------------------------------Population Tab
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
  
  pop_means <- reactive({ddply(popdf_long(), "dataset", summarize, means = mean(values))})
  
  output$curve <- renderPlot({
    ggplot(data = popdf_long(), aes(x=values, color = dataset)) +
      geom_density() +
      coord_cartesian(xlim = c(-.125, 1.25), ylim = c(0,8)) +
      geom_vline(data = pop_means(), aes(xintercept = means, color = dataset),
                 linetype = 2, size = 0.8) +
      ggtitle("Population Distributions") +
      theme(legend.position = "none", plot.title = element_text(size = "14"),
            axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank()) 
    
  })
 
  sample1 <- reactive({
    set.seed(1)
    sample(pop1_trans(), size = input$n, replace = TRUE)
  })
  sample2 <- reactive({
    set.seed(1)
    sample(pop2_trans(), size = input$n, replace = TRUE)
  })
  sample3 <- reactive({
    set.seed(1)
    sample(pop3_trans(), size = input$n, replace = TRUE)
  })
  
  sampledf <- reactive({data.frame(sample1(), sample2(), sample3())})
  sampledf_long <- reactive({
    sampledf() %>%
      gather(key = dataset, value = values)
  })
  
  ##--------------------------------------------------------------Samples Tab
  ##recall that sample1, sample2, sample3 already defined; also dataset with all is sampledf_long()
  
  #Assumes approximate normality
  output$assumption1 <- renderUI({
    if(input$n >= 10) {
      return(p("This assumption is currently met because the selected sample size,", paste(input$n), ", is greater than or equal to 10",
                    style = "color:#20C100"))
    } else { #n < 10
      if(input$skew1 != "norm" | input$skew2 != "norm" | input$skew3 != "norm") {
        return(p("This assumption is currently NOT met because the selected sample size is less than 10 and one or more of the
               population distributions is skewed", style = "color:#C10000"))
      } else {
        return(p("This assumption is currently NOT met because the selected sample size is less than 10", style = "color:#C10000"))
      }
    }
    
  })
  
  rank <- function(s1, s2, s3) {
    if(var(s1) > var(s2) & var(s1) > var(s3) & var(s2) > var(s3)) {
      v1 <- reactive({var(s1)})
      v2 <- reactive({var(s2)})
      v3 <- reactive({var(s3)})
      
      if(v1() < (v3() * 2)) {
        return(p("This assumption is currently met because the maximum within groups variance is less 
               than double the minimum within groups variance", style = "color:#20C100"))
      } else {
        return(p("This assumption is currently NOT met because the maximum within groups variance is greater than 
               or equal to double the minimum within groups variance", style = "color:#C10000"))
      }
    }
    if(var(s1) > var(s2) & var(s1) > var(s3) & var(s3) > var(s2)) {
      v1 <- reactive({var(s1)})
      v2 <- reactive({var(s3)})
      v3 <- reactive({var(s2)})
      
      if(v1() < (v3() * 2)) {
        return(p("This assumption is currently met because the maximum within groups variance is less 
               than double the minimum within groups variance", style = "color:#20C100"))
      } else {
        return(p("This assumption is currently NOT met because the maximum within groups variance is greater than 
               or equal to double the minimum within groups variance", style = "color:#C10000"))
      }
    }
    
    
    
    if(var(s2) > var(s1) & var(s2) > var(s3) & var(s1) > var(s3)) {
      v1 <- reactive({var(s2)})
      v2 <- reactive({var(s1)})
      v3 <- reactive({var(s3)})
      
      if(v1() < (v3() * 2)) {
        return(p("This assumption is currently met because the maximum within groups variance is less 
               than double the minimum within groups variance", style = "color:#20C100"))
      } else {
        return(p("This assumption is currently NOT met because the maximum within groups variance is greater than 
               or equal to double the minimum within groups variance", style = "color:#C10000"))
      }
    }
    if(var(s2) > var(s1) & var(s2) > var(s3) & var(s3) > var(s1)) {
      v1 <- reactive({var(s2)})
      v2 <- reactive({var(s3)})
      v3 <- reactive({var(s1)})
      
      if(v1() < (v3() * 2)) {
        return(p("This assumption is currently met because the maximum within groups variance is less 
               than double the minimum within groups variance", style = "color:#20C100"))
      } else {
        return(p("This assumption is NOT met because the maximum within groups variance is greater than 
               or equal to double the minimum within groups variance", style = "color:#C10000"))
      }
    }
    
    
    
    if(var(s3) > var(s1) & var(s3) > var(s2) & var(s1) > var(s2)) {
      v1 <- reactive({var(s3)})
      v2 <- reactive({var(s1)})
      v3 <- reactive({var(s2)})
      
      if(v1() < (v3() * 2)) {
        return(p("This assumption is currently met because the maximum within groups variance is less 
               than double the minimum within groups variance", style = "color:#20C100"))
      } else {
        return(p("This assumption is NOT met because the maximum within groups variance is greater than 
               or equal to double the minimum within groups variance", style = "color:#C10000"))
      }
    }
    if(var(s3) > var(s1) & var(s3) > var(s2) & var(s2) > var(s1)) {
      v1 <- reactive({var(s3)})
      v2 <- reactive({var(s2)})
      v3 <- reactive({var(s1)})
      
      if(v1() < (v3() * 2)) {
        return(p("This assumption is currently met because the maximum within groups variance is less 
               than double the minimum within groups variance", style = "color:#20C100"))
      } else {
        return(p("This assumption is NOT met because the maximum within groups variance is greater than 
               or equal to double the minimum within groups variance", style = "color:#C10000"))
      }
    }
    
  }
  
  output$assumption2 <- renderUI ({
    rank(s1 = sample1(), s2 = sample2(), s3 = sample3())
  })
  
  
  output$boxplot <- renderPlot({
    ggplot(data = sampledf_long(), aes(x = dataset, y = values)) + 
      geom_boxplot(aes(color = dataset)) +
      geom_jitter(aes(x = dataset, y = values, alpha = .2, color = dataset), position=position_jitter(0.04)) +
      #coord_cartesian(ylim =c(0.1, 1.2)) +
      geom_hline(yintercept=mean(sampledf_long()$values), linetype=2, color = "black") +
      labs(title = "Sample Distributions", x = "Sample", y = "Values") +
      theme(legend.position = "none",  plot.title = element_text(size = "14"),
            axis.text.x=element_blank(), axis.ticks.x=element_blank())
  })
  
  output$toggleBtw <- renderPlot({
    ggplot(data = sampledf_long(), aes(x = dataset, y = values)) +
      stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                   geom = "point", shape=16, size=5, aes(color = dataset)) +
      #coord_cartesian(ylim =c(0.1, 1.2)) +
      geom_hline(yintercept=mean(sampledf_long()$values), linetype=1, color = "black") +
      labs(title = "Sample Distributions", x = "Sample", y = "Values") +
      theme(legend.position = "none", plot.title = element_text(size = "14"),
            axis.text.x=element_blank(), axis.ticks.x=element_blank())
  })
  
  output$toggleWin <- renderPlot({
    ggplot(data = sampledf_long(), aes(x = dataset, y = values)) + 
      geom_jitter(aes(x = dataset, y = values, alpha = .2, color = dataset), position=position_jitter(0.04)) +
      stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                   geom = "crossbar", width = 0.5, aes(color = dataset)) +
      #coord_cartesian(ylim =c(0.1, 1.2)) +
      labs(title = "Sample Distributions", x = "Sample", y = "Values") +
      theme(legend.position = "none", plot.title = element_text(size = "14"),
            axis.text.x=element_blank(), axis.ticks.x=element_blank())
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