library(shiny)
library(shinythemes)
library(mathjaxr)
library(tidyr)
library(ggplot2)
library(broom)
library(shinyBS)
library(plyr)
library(DT)
library(knitr)

# Consider adding button that leads straight to "insuff evidence" conclusion?

ui <- navbarPage(
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
  ),
  
  theme = shinytheme("lumen"),
  
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
           style = "font-size:20px",
           tags$style(
             HTML("
                                 .tooltip > .tooltip-inner {
                                 background-color: #000000;
                                 color: #FFFFFF;
                                 border: 1px solid #000000;
                                 text-align: left;
                                 }
                                 ")),
           fluidRow(
             column(offset = 1, width = 10,
                    h2("About the App")),
             
           ),
           fluidRow(
             column(offset = 1, width = 10,
                    p("Welcome! This interactive learning tool is brought to you by Duke Shiny-Ed. In this particular app 
                            you will be able to explore", tipify(strong("ANOVA", style = "color:#00B5E5"), 
                                                                 title = "Hypothesis test which analyzes variance to make inferences about means. Tests the null that all group means are equal and the alternative that at least one of the means are different, not necessarily that all are unequal", 
                                                                 trigger = "hover"), 
                      ", or Analysis of Variance. This app is designed to gradually introduce you to the different aspects of ANOVA and it concludes with a quiz so that you 
                            can test what you've learned! Hopefully, after using this
                            app, you will be able to:"),
                    p("1. Examine how violations of the", tipify(strong("assumptions of ANOVA", style = "color:#00B5E5"),
                                                                 title = "1)Independent observations 2)Approximately normal population distributions for all groups 3)Approximately equal within group variances for all groups", 
                                                                 trigger = "hover"), 
                      "affect the ANOVA test output"),
                    p("2. Understand the relationship between the", tipify(strong("F-statistic", style = "color:#00B5E5"),
                                                                           title = "The F-stat is a ratio of the between and within groups variances. When F is large we are prompted to reject the null hypothesis and conclude that there is at least one difference between the group means",
                                                                           placement = "bottom", trigger = "hover"), "and the ANOVA test output"),
                    p("3. Predict how manipulation of between and within group variances will affect the F-statistic
                                     and the ANOVA test output"),
                    br(),
                    ## put more general info here
                    p("If you haven't already noticed, while interacting with this app you will routinely encounter text that
                                     is highlighted blue like this:", tipify(el = strong(em("What's happening?", style = "color:#00B5E5; font-size:20px")),
                                                                             title = "This is an example! Hover over any text highlighted in blue like this to view helpful information",
                                                                             placement = "bottom", trigger = "hover"), 
                      "These are designed to help you navigate the material being presented. Hover over them to view definitions 
                                    of commonly used terms and other information meant to guide you through the exploration!")
                    
                    
             ),
             
             
             
           ),
           
           br(), br(), br(), br(), br(), br(),
           
           hr(),
           
           h2("Acknowledgements"),
           p("Portions of this app may utilize information from the following sources:"), br(),
           fluidRow(
             column(offset = 1, width = 3,
                    p(strong("''Unit 4: Inference for numerical data - 4. ANOVA''"), br(),
                      em("Presentation by Dr. Abrahamsen"), br(), 
                      em("Duke University, Department of Statistical Science")
                    )
             ),
             column(width = 3,
                    p(strong("''ANOVA''"), br(),
                      em("Presentation by Dr. Jiang"), br(),
                      em("Duke University, Department of Statistical Science"))
             ),
             column(width = 3,
                    p(strong("''Analysis of Variance - ANOVA''"), br(), 
                      em("Presentation by Dr. Tackett"), br(), 
                      em("Duke University, Department of Statistical Science")
                    )
             )
           ),
           br(), br(),
           
           p("Special thank you to Dr. Yue Jiang for introducing me to ANOVA and describing it in a way that would eventually inspire this app."),
           br(), br(), br(), br(), br(), br(),
           br(), br(), br(), br(), br(),
           
           p("Created by Samantha Owusu-Antwi for Duke University 'Creating Interactive Learning Tools' Project, Summer 2020", style = "text-align:center")
  ),
  
  tabPanel("Background",
           style = "font-size:20px",
           column(offset = 1, width = 10,
                  h2("An Introduction to ANOVA")),
           ## put intro to ANOVA info here
           column(offset = 1, width = 10,
                  p("ANOVA is a method used in statistical analysis to make inferences about numerical data. In particular, 
                            ANOVA is used to question whether there is a meaningful difference between groups being studied. To
                            do this, ANOVA tests the alternative hypothesis that at least one of the group means is truly different
                            from the others against the null hypothesis that there is no difference between the group means. In this sense, 
                            ANOVA can be thought of as generalizing the two sample", tipify(strong("t-test", style = "color:#00B5E5"),
                                                                                            title = "A hypothesis test used to compare two means",
                                                                                            placement = "top", trigger = "hover"),
                    "to more that two categories!"),
                  p("So why don't we just use the two sample t-test multiple times? Conclusions drawn from such repetitive pairwise testing 
                                     can be misleading. More specifically, this practice will lead to an inflated", tipify(strong("family-wise error", style = "color:#00B5E5"),
                                                                                                                           title = "The probability that we fail to arrive at the correct conclusion at least once when we conduct multiple pairwise tests. This probability will generally increase as we increase the number of tests we are conducting",
                                                                                                                           placement = "bottom", trigger = "hover"), 
                    "rate. After all if you test anything enough times you're bound reach the wrong conclusion. So, to mitigate this error we generally wait until 
                                     after ANOVA suggests there is at least one difference among groups before conducting pairwise 
                                     tests, while", tipify(strong("correcting", style = "color:#00B5E5"),
                                                           title = "Refers to the Bonferroni correction where the significance level, alpha, is adjusted before it is used. The adjusted alpha value is equal to the original alpha value divided by the total number of pairwise tests to be performed"), 
                    "for the family-wise error rate, to determine where exactly the difference lies.")
           ),
           column(offset = 1, width = 10,
                  h2("ANOVA Test")),
           
           column(offset = 1, width = 10,
                  h2("ANOVA Assumptions")),
  ),
  
  
  ##--------------------------------------------------------tab 1
  tabPanel("Explore",
           tabsetPanel(
             tabPanel("Step 1: Identify the Population",
                      style = "font-size:15px",
                      fluidRow(
                        column(offset = 2, width = 8, style="background-color:#F1F0F0; padding:20px; border-radius:10px",
                               p("ANOVA tests the following hypotheses:"),
                               p("$H_0$: The means of all groups are equal $(\\mu_1 = \\mu_2 = ... = \\mu_K)$", 
                                 br(),
                                 "$H_1$: At least one of the means $(\\mu_i)$ is not equal to the others", style = "text-align:center"),
                               br(),
                               p("Below you have the opportunity to manipulate the", tipify(strong("skew", style = "color:#00B5E5"),
                                                                                            title = "Skew refers to the asymmetry of a distribution that is ideally symmetrical, in this case bell curves",
                                                                                            placement = "top", trigger = "hover"), 
                                 ",", tipify(strong("between group variance", style = "color:#00B5E5"),
                                             title = "ANOVA is concerned with two variances. This refers to how group means vary around the overall mean",
                                             placement = "top", trigger = "hover"), 
                                 ", and the", tipify(strong("within group variances", style = "color:#00B5E5"),
                                                     title = "ANOVA is concerned with two variances. This refers to how the individual observations of a group vary around the mean of that group",
                                                     placement = "top", trigger = "hover"), "of the population data we will be working with.
                                     Normally, true population parameters are not known.", strong("Remember, ANOVA assumes each group's population density is approximately normal.")), 
                               br(),
                               p(strong("DISCLAIMER: It is unrealistic to choose the population, but for the purposes of understanding the material, you are able to do so in this app.")),
                        )
                        
                      ),
                      
                      br(),
                      #hr(),
                      
                      fluidRow(
                        
                        column(width = 3,
                               h3(strong("Between Group Variance")),
                               wellPanel(
                                 p("Toggle between 'Reduced' and 'Increased' to translate the population density curves, 
                                  altering the distance between their means", 
                                   style = "color:grey"),
                                 br(),
                                 
                                 selectInput(inputId = "btwsd1",
                                             label = NULL,
                                             choices = c("Increased Between Group Variance" = "inc",
                                                         "Reduced Between Group Variance" = "dec"),
                                             selected = "inc"),
                                 br(),
                                 
                                 p("The estimated between groups variance for this data is:"),
                                 verbatimTextOutput(outputId = "between.group"),
                                 fluidRow(
                                   column(offset = 6, width = 6, 
                                          tipify(el = p(em(strong("What's happening?")), style = "text-align:right; color:#00B5E5; font-size:12px"),
                                                 title = "Try toggling between “Reduced” and “Increased” group variance, and use the plot and summary statistics to compare the distributions. How does the distance between group means differ when the between group variance differs?",
                                                 placement = "bottom", trigger = "hover"))),
                                 #actionButton(inputId = "gobtw", label = "Update")
                               ),
                               
                               h3(strong("Shape")),
                               wellPanel(
                                 p("Manipulate the skew of each population density curve. Recall ANOVA assumes the 
                                  population density of each group is approximately normal", 
                                   style = "color:grey"),
                                 br(),       
                                 
                                 selectInput(inputId = "skew1", 
                                             label = p("Group 1", style = "color:red"),
                                             choices = c("Normal" = "norm",
                                                         "Right Skewed (Positive)" = "rskew",
                                                         "Left Skewed (Negative)" = "lskew"),
                                             selected = "norm"),
                                 selectInput(inputId = "skew2", 
                                             label = p("Group 2", style = "color:green"),
                                             choices = c("Normal" = "norm",
                                                         "Right Skewed (Positive)" = "rskew",
                                                         "Left Skewed (Negative)" = "lskew"),
                                             selected = "norm"),
                                 selectInput(inputId = "skew3", 
                                             label = p("Group 3", style = "color:blue"),
                                             choices = c("Normal" = "norm",
                                                         "Right Skewed (Positive)" = "rskew",
                                                         "Left Skewed (Negative)" = "lskew"),
                                             selected = "norm"),
                                 #actionButton(inputId = "goshape", label = "Update")
                               ),
                               br(), br()
                               
                        ),
                        
                        
                        column(width = 6,
                               br(),
                               
                               p(plotOutput(outputId = "curve")),
                               fluidRow(
                                 column(width = 4,
                                        tipify(el = p(em(strong("What does this graph show?")), style = "text-align:left; color:#00B5E5; font-size:12px"),
                                               title = "This graph shows the population distributions for the variable of interest. The vertical dotted lines show the mean of each distribution.",
                                               placement = "bottom", trigger = "hover")),
                                 #reset manipulations to default
                                 column(width = 4, offset = 4,
                                        actionButton("reset", label = "Click to reset settings"))
                                 
                               ),
                               
                               h3(strong("Summary Statistics")),
                               #wellPanel(
                               p(strong("Group 1"), style = "text-align:left; color:red; font-size:15px"),
                               
                               htmlOutput(
                                 outputId = "summary1"
                                 
                               ),
                               
                               
                               p(strong("Group 2"), style = "text-align:left; color:green; font-size:15px"),
                               
                               htmlOutput(
                                 outputId = "summary2"
                                 
                               ),
                               
                               p(strong("Group 3"), style = "text-align:left; color:blue; font-size:15px"),
                               
                               htmlOutput(
                                 outputId = "summary3"
                                 
                               ),
                               
                               #),
                               
                        ),
                        
                        
                        column(width = 3,
                               h3(strong("Within group variance")),
                               wellPanel(
                                 p("Manipulate the sliders to increase or decrease the within group variance of each curve 
                                     by a factor of the slider value. This will alter the spread of each density curve. 
                                     Recall ANOVA assumes these variances are approximately equal",
                                   style = "color:grey"),
                                 br(),
                                 
                                 sliderInput(inputId = "sd1",
                                             label = p("Group 1", style = "color:red"),
                                             min = 0.25, max = 2.25, value = 1),
                                 sliderInput(inputId = "sd2",
                                             label = p("Group 2", style = "color:green"),
                                             min = 0.25, max = 2.25, value = 1),
                                 sliderInput(inputId = "sd3",
                                             label = p("Group 3", style = "color:blue"),
                                             min = 0.25, max = 2.25, value = 1),
                                 br(),
                                 
                                 # p("The pooled estimate of the within groups variance for this data is:"),
                                 # verbatimTextOutput(outputId = "within.group"),
                                 
                                 # fluidRow(
                                 #   column(offset = 6, width = 6,
                                 #          tipify(el = p(em(strong("What's happening?")), style = "text-align:right; color:#00B5E5; font-size:12px"),
                                 #                 title = "Try moving the sliders from one end to the other. Based on the distributions that result, how does this relate to how different the means are?",
                                 #                 placement = "bottom", trigger = "hover"))),
                                 #actionButton(inputId = "gowithin", label = "Update")
                               ))
                      )
             ),
             ##--------------------------------------------------------tab2
             tabPanel("Step 2: Draw the Samples",
                      
                      style = "font-size:15px",
                      fluidRow(
                        column(offset = 2, width = 8, 
                               p("Now that our population data is set, let's examine the random samples taken from the populations."),
                               p(strong("Recall ANOVA assumes group distributions are approximately normal."), "However ANOVA is relatively robust against
                                departures from normality. In fact, as long as sample sizes are", tipify(el = strong("large enough", style = "color:#00B5E5"),
                                                                                                         title = "in this case approximately greater than 30 in each group",
                                                                                                         placement = "top", trigger = "hover"), "the conclusions
                                of ANOVA may still be valid, even if the underlying population distributions are skewed"),
                               
                               p(strong("ANOVA also assumes that groups have roughly equal variability."), "ANOVA is not robust against violations
                                of this assumption and the results of ANOVA may not be valid if", tipify(el = strong("$s_{max} \\geq 2s_{min}$", style = "color:#00B5E5"),
                                                                                                         title = "in other words, if the maximum group standard deviation is greater than or equal to double the minimum group standard deviation")
                               ),   
                               style="background-color:#F1F0F0; padding:20px; border-radius:10px")
                      ),
                      br(),
                      #hr(),
                      
                      fluidRow(
                        column(width = 3,
                               br(), br(), br(), br(),
                               h3(strong("ANOVA Assumptions")),
                               wellPanel(
                                 "1. Independent Observations", uiOutput("assumption1"),
                                 "2. Approximately normal population distributions for all groups", uiOutput("assumption2"),
                                 "3. Approximately equal within group variances for all groups", uiOutput("assumption3")
                               )
                        ),
                        
                        column(width = 6,
                               numericInput("n", label = "Choose a sample size between 2 and 200",
                                            min = 2, max = 200, value = 100, step = 1),
                               #tabsetPanel(
                               #tabPanel("Whole Graph", 
                               column(width = 12, plotOutput(outputId = "boxplot")),
                               fluidRow(
                                 column(offset = 1, width = 4,
                                        tipify(el = p(em(strong("What does this graph show?")), style = "text-align:left; color:#00B5E5; font-size:12px"),
                                               title = "These graphs all depict parts of the sample distributions. Here the between groups variance can be thought of as how the medians of the boxplots vary from the overall mean, the solid black line. The within groups variance can be thought of as how the datapoints of a sample vary from the median of that sample.",
                                               placement = "top", trigger = "hover"))
                               ),
                               br(), br(),
                               #)
                               # tabPanel("Focus on Within Groups Variance", column(width = 12, plotOutput(outputId = "toggleWin")),
                               #          fluidRow(
                               #            column(offset = 1, width = 4,
                               #                   tipify(el = p(em(strong("What does this graph show?")), style = "text-align:left; color:#00B5E5; font-size:12px"),
                               #                          title = "These graphs all depict parts of the sample distributions. Here the within groups variance is visualized as how the datapoints of a sample vary from the median of that sample, the corresponding solid lines.",
                               #                          placement = "top", trigger = "hover"))
                               #          ),
                               #          br(), br(), br(), br()
                               # ),
                               # 
                               # tabPanel("Focus on Between Groups Variance", column(width = 12, plotOutput(outputId = "toggleBtw")),
                               #          fluidRow(
                               #            column(offset = 1, width = 4,
                               #                   tipify(el = p(em(strong("What does this graph show?")), style = "text-align:left; color:#00B5E5; font-size:12px"),
                               #                          title = "These graphs all depict parts of the sample distributions. Here the between groups variance is visualized as how the median of a boxplot, represented by a point in this case, varies from the overall mean, the solid black line.",
                               #                          placement = "top", trigger = "hover"))
                               #          ),
                               #          br(), br(), br(), br()
                               # )
                               #),
                               
                               h3(strong("Sample Observations")),
                               p("Filter the dataset to view the values within each sample."),
                               DTOutput(outputId = "table")
                        ),
                        
                        column(width = 3,
                               br(), br(), br(), br(),
                               h3(strong("Summary Statistics")),
                               htmlOutput("sumstats"),
                               br(), br()
                        ),
                        
                      )
             ),
             
             
             ##--------------------------------------------------------tab 3
             tabPanel("Step 3: ANOVA Test",
                      style = "font-size:15px",
                      fluidRow(
                        column(offset = 2, width = 8, style="background-color:#F1F0F0; padding:20px; border-radius:10px",
                               p("Below you will find the results of the ANOVA test run on the data you manipulated in the previous
                                     steps"),
                               p(strong("Recall ANOVA tests the following hypotheses:")),
                               p("$H_0$: The means of all groups are equal $(\\mu_1 = \\mu_2 = ... = \\mu_K)$", 
                                 br(),
                                 "$H_1$: At least one of the means $(\\mu_i)$ is not equal to the others", style = "text-align:center")
                               
                        )
                      ),
                      br(),
                      #hr(),
                      
                      fluidRow(
                        column(width = 3,
                               h3(strong("What is the F-statistic?")),
                               wellPanel(
                                 p("If the sample means vary around the overall mean more that the individual observations vary around
                                     their sample means, we have evidence that the corresponding population means are different. We formally compare
                                    these variances with the F-statistic. Relating back to Step 1, we see that increasing the between group variance 
                                       and decreasing the within group variance increases the F-stat, while doing the opposite decreases the F-stat.", br(), br(),
                                   "If there is", tipify(el = strong("no treatment effect", style = "color:#00B5E5"),
                                                         title = "No treatment effect implies that the null hypothesis is true, the means of all groups are truly equal",
                                                         placement = "top", trigger = "hover"),
                                   "the F-stat will be very close to 1"),
                                 
                                 
                                 p("$\\large{F = \\frac{{{s_B}^{2}}/ndf}{{{s_W}^{2}}/ddf}}$", style = "text-align:center"),
                                 p("$\\bullet$ ${s_B}^{2}$ is the between groups variance", tipify(strong("*", style = "color:#00B5E5"),
                                                                                                   title = "the between groups variance is proportional to F",
                                                                                                   placement = "top", trigger = "hover"), br(),
                                   "$\\bullet$ ${s_W}^{2}$ is a pooled estimate of the within groups variances", tipify(strong("*", style = "color:#00B5E5"),
                                                                                                                        title = "the within groups variance is inversely proportional to F",
                                                                                                                        placement = "right", trigger = "hover"), br(),
                                   "$\\bullet$ $ndf$ is the", tipify(strong("numerator degrees of freedom", style = "color:#00B5E5"),
                                                                     title = "The degrees of freedom corresponding to the between groups variance, calculated as the total number of groups - 1",
                                                                     placement = "top", trigger = "hover"), br(),
                                   "$\\bullet$ $ddf$ is the", tipify(strong("denominator degrees of freedom", style = "color:#00B5E5"),
                                                                     title = "The degrees of freedom corresponding to the within groups variance, calculated as the total number of observations - the number of groups",
                                                                     placement = "bottom", trigger = "hover")),
                                 br(),
                                 
                                 p("The F-stat for our data is:"),
                                 verbatimTextOutput(outputId = "FTest")
                               )
                        ),
                        
                        column(width = 9,
                               br(),
                               br(),
                               plotOutput(outputId = "boxplot2"),
                               
                               #p(verbatimTextOutput(outputId = "aovTest")),
                               p(htmlOutput(outputId = "aovTest")),
                               fluidRow(
                                 column(offset = 9, width = 3,
                                        tipify(el = p(em(strong("What's happening?")), style = "text-align:right; color:#00B5E5; font-size:12px"),
                                               title = "This is the output of our ANOVA test when run in R. If the sample means are far apart there is evidence against the null hypothesis that the mean value of response is the same for all groups. But what is considered far apart? The F-stat quantifies this",
                                               placement = "top", trigger = "hover"))),
                               
                               
                               h3(strong("Conclusion")),
                               wellPanel(
                                 p("At the $\\alpha = .05$ level the F-stat corresponds to a p-value that suggests there is",
                                   textOutput(outputId = "concl", inline = TRUE)),
                                 fluidRow(
                                   column(offset = 5, width = 7,
                                          tipify(el = p(em(strong("What's happening?")), style = "text-align:right; color:#00B5E5; font-size:12px"),
                                                 title = "When the p-value is less than alpha we reject the null in favor of the alternative hypothesis. When the p-value is greater than alpha we fail to reject the the null hypothesis",
                                                 placement = "bottom", trigger = "hover")
                                   )
                                 ),
                                 uiOutput("valid"))
                               
                        ),
                        tags$style(type = 'text/css', 
                                   '#concl {
                              background-color:yellow;
                              }'),
                        
                        
                      )
                      
             )
           )
  ),
  
  
  ##---------------------------------------------------------quiz on boxplots comparing F-stats  
  tabPanel("Quiz",
           fluidRow(
             tags$iframe(src = "https://samanthaowusu.shinyapps.io/ANOVA-quiz/",
                         width = "1500", height = "1000",
                         frameBorder="0")
           )
  ),
  
  
  
  tabPanel("Resources",
           style = "font-size:20px",
           
           h1("Equations"),
           fluidRow(
             column(width = 11,
                    h4("The following are some equations that may not have been referenced in the app that you may come across when dealing with ANOVA. You", em("DO NOT"),
                       "need to know these"),
                    h4("equations in order to interact with this app")
             )
           ), br(),
           
           fluidRow(
             column(offset = 2, width = 3,
                    h3(strong("Bonferroni correction"), style = "text-align:center"),
                    p("$\\Large{\\alpha^{*} = \\frac{\\alpha}{k}}$", style = "text-align:center;background-color:yellow"),
                    h4("$\\bullet$ $k$ is the total number of pair-wise tests"),
                    h4("$\\bullet$ $\\alpha^{*}$ is the adjusted alpha value"),
                    h4("$\\bullet$ $\\alpha$ is the original, unadjusted alpha value")
             ),
             
             column(offset = 1, width = 4,
                    h3(strong("The Between Groups Variance"), style = "text-align:center"),
                    p("$\\Large{{s_B}^{2}  =  \\frac{n_1{(\\overline{y}_1 - \\overline{y})}^{2}   +   n_2{(\\overline{y}_2 - \\overline{y})}^{2}   + ... +   n_K{(\\overline{y}_K  -  \\overline{y})}^{2}}{K - 1}}$", style = "text-align:center;background-color:yellow"),
                    h4("$\\bullet$ $K$ is the total number of groups, each with a corresponding sample size equal to $n_1, n_2, ... n_K$"),
                    h4("$\\bullet$ $\\overline{y}_1$, $\\overline{y}_2$, ... $\\overline{y}_K$ are the means of groups $1$, $2$, and $K$ respectively"),
                    h4("$\\bullet$ $\\overline{y}$ is the", em("overall"), "mean, the mean of all observations")
             )
           ),
           br(),
           
           fluidRow(
             column(offset = 2, width = 3,
                    h3(strong("The ANOVA model"), style = "text-align:center"),
                    p("$\\Large{y_{ij} = \\mu_i + \\epsilon_{ij}}$", style = "text-align:center; background-color:yellow"),
                    h4("$\\bullet$ $y_{ij}$ is an individual observation, the response of a subject $j$ in group $i$"),
                    h4("$\\bullet$ $\\mu_i$ is the mean of group $i$"),
                    h4("$\\bullet$ $\\epsilon_{ij}$ is a specific random error term, the error of an individual about its group's mean")
             ),
             
             column(offset = 1, width = 4,
                    h3(strong("Pooled estimate of Within Groups Variance"), style = "text-align:center"),
                    p("$\\Large{{s_W}^{2}  =  \\frac{(n_1  -  1){s_1}^{2} +  (n_2  -  1){s_2}^{2}  +  ...  +  (n_K  -  1){s_K}^{2}}{n - K}}$", style = "text-align:center;background-color:yellow"),
                    h4("$\\bullet$ $K$ is the total number of groups, each with a corresponding sample size equal to $n_1, n_2, ... n_K$"),
                    h4("$\\bullet$ $n$ is the total number of samples, $n = n_1 + n_2 + ... + n_K$"),
                    h4("$\\bullet$ Each group's squared variance, ${s_1}^{2}$, ${s_2}^{2}$, ... ${s_K}^{2}$, is"),
                    p("$\\Large{{s_i}^{2} = \\frac{\\sum_{j=1}^{n_i} {(y_{ij}  -  \\overline{y}_i)}^{2}}{n_i  -  1}}$", style = "text-align:center")
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
             column(offset = 2, width = 4,
                    p(em("Values in the 'Source' column clarify the source of the variance in question"), style = "font-size:20px"),
                    h4("$\\bullet$ The name of the grouping variable may be used in place of the 'Between' label"),
                    h4("$\\bullet$ The word 'Residuals' may be used in place of the 'Within' label")
             ),
             
             column(width = 4, 
                    p(em("The Mean Sq Between is an estimate of the variance of the group means
                                        from the overall mean"), style = "font-size:20px"),
                    p(em("The Mean Sq Within is an estimate of $\\sigma^{2}$, the inherent variability in 
                                          each group's population"), style = "font-size:20px"),
                    h4("$\\bullet$ ${s_B}^{2}$ is the between groups variance"),
                    h4("$\\bullet$ ${s_W}^{2}$ is a pooled estimated of the within groups variances")
                    
             )
           ),
           br(), br(),
           
           fluidRow(
             column(offset = 2, width = 4,
                    p(em("'df' refers to the degrees of freedom"), style = "font-size:20px"),
                    h4("$\\bullet$ $ndf = K - 1$"),
                    h4("$\\bullet$ $ddf = n - K$"),
                    h4("Where $n$ is the total number of samples and $K$ is the total number of groups")
             ),
             column(width = 4,
                    br(),
                    h4("$\\bullet$ $F_{obs}$ is equal to the ratio of the 'Between' Mean Sq value 
                                      to the 'Within' Mean Sq value"),
                    h4("$\\bullet$ The null hypothesis is rejected if $p_{obs}$, the p-value corresponding to $F_{obs}$,
                                         is less than $\\alpha$")
             )
           ),
           
           
  )
  
  
)


server <- function(input, output, session) {
  ##--------------------------------------------------------------Population Tab
  #Preserve mean when manipulate skew
  #Preserve mean when manipulate variance with similar procedure?
  pop_dist <- function(skew, within, n) {
    mean.initial <- reactive({
      set.seed(n)
      mean(rbeta(20000, shape1 = 22, shape2 = 22) * 1)
    })
    
    if(skew == "norm") {
      mean.norm <- reactive ({
        set.seed(n)
        mean(rbeta(20000, shape1 = 22, shape2 = 22) * (within + .5))
      })
      set.seed(n)
      return(rbeta(20000, shape1 = 22, shape2 = 22) * (within + .5) - (mean.norm() - mean.initial()))
      
      
    } else if(skew == "rskew") {
      mean.rskew <- reactive({
        set.seed(n)
        mean(rbeta(20000, shape1 = 4, shape2 = 31) * (within + .5))
      })
      
      set.seed(n)
      return(rbeta(20000, shape1 = 4, shape2 = 31) * (within + .5) - (mean.rskew() - mean.initial()))
      
      
    } else {
      mean.lskew <- reactive({
        set.seed(n)
        mean(rbeta(20000, shape1 = 31, shape2 = 4) * (within + .5))
      }) 
      
      set.seed(n)
      return(rbeta(20000, shape1 = 31, shape2 = 4) * (within + .5) - (mean.lskew() - mean.initial()))
    }
  }
  
  pop1 <- reactive({pop_dist(skew = input$skew1, within = input$sd1, n = 1)})
  pop2 <- reactive({pop_dist(skew = input$skew2, within = input$sd2, n = 2)})
  pop3 <- reactive({pop_dist(skew = input$skew3, within = input$sd3, n = 3)})
  
  
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
      return(set1)
    }
    
  }
  
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
      coord_cartesian(xlim = c(-0.3, 1.3), ylim = c(0, 6.8)) +
      geom_vline(data = pop_means(), aes(xintercept = means, color = dataset),
                 linetype = 2, size = 0.8) +
      ggtitle("Population Distributions") +
      theme_bw()+
      theme(legend.position = "none", plot.title = element_text(size = "20", face = "bold"),
            axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank()) 
    
  })
  
  
  output$summary1 <- renderText({
    popdf() %>%
      summarise(Mean = mean(pop1_trans()),
                Median = median(pop1_trans()),
                SD = sd(pop1_trans()),
                Variance = var(pop1_trans()),
                Min = min(pop1_trans()),
                Max = max(pop1_trans()))%>%
      knitr::kable(format = "html", digits = 3) %>%
      kableExtra::kable_styling("striped", font_size = 20)
  })
  
  output$summary2 <- renderText({
    popdf() %>%
      summarise(Mean = mean(pop2_trans()),
                Median = median(pop2_trans()),
                SD = sd(pop2_trans()),
                Variance = var(pop2_trans()),
                Min = min(pop2_trans()),
                Max = max(pop2_trans()))%>%
      knitr::kable(format = "html", digits = 3) %>%
      kableExtra::kable_styling("striped", font_size = 20)
  })
  
  output$summary3 <- renderText({
    popdf() %>%
      summarise(Mean = mean(pop3_trans()),
                Median = median(pop3_trans()),
                SD = sd(pop3_trans()),
                Variance = var(pop3_trans()),
                Min = min(pop3_trans()),
                Max = max(pop3_trans()))%>%
      knitr::kable(format = "html", digits = 3) %>%
      kableExtra::kable_styling("striped", font_size = 20)
  })
  
  
  sample1 <- reactive({
    validate(
      need(!((input$n > 200) | (input$n < 2)), 'Please enter a number between 2 and 200.')
    )
    set.seed(1)
    sample(pop1_trans(), size = input$n, replace = TRUE)
  })
  sample2 <- reactive({
    validate(
      need(!((input$n > 200) | (input$n < 2)), 'Please enter a number between 2 and 200.')
    )
    set.seed(1)
    sample(pop2_trans(), size = input$n, replace = TRUE)
  })
  sample3 <- reactive({
    validate(
      need(!((input$n > 200) | (input$n < 2)), 'Please enter a number between 2 and 200.')
    )
    set.seed(1)
    sample(pop3_trans(), size = input$n, replace = TRUE)
  })
  
  sampledf <- reactive({data.frame(sample1(), sample2(), sample3())})
  sampledf_long <- reactive({
    sampledf() %>%
      gather(key = dataset, value = values)
  })
  
  #Between groups variance eq
  all.samples <- reactive({
    rbind(sample1(), sample2(), sample3())
  })
  
  overall.mean <- reactive({mean(all.samples())})
  total.size <- reactive({input$n * 3})
  
  m1 <- reactive({mean(sample1())})
  m2 <- reactive({mean(sample2())})
  m3 <- reactive({mean(sample3())})
  
  var1 <- reactive({var(sample1())})
  var2 <- reactive({var(sample2())})
  var3 <- reactive({var(sample3())})
  
  #between groups variance eq
  output$between.group <- renderPrint(
    (
      (input$n * (m1() - overall.mean())^(2)) + (input$n * (m2() - overall.mean())^(2)) + (input$n * (m3() - overall.mean())^(2)))/2
  )
  
  #Pooled estimate of within groups variance eq
  # output$within.group <- renderPrint((((input$n - 1)*(var1())^(2)) + ((input$n - 1)*(var2())^(2)) + ((input$n - 1)*(var3())^(2)))/(total.size() - 3))
  
  #Reset manipulations
  observeEvent(input$reset, {
    updateNumericInput(session, "n", value = 100)
    updateSelectInput(session, "btwsd1", selected = "inc")
    updateSelectInput(session, "skew1", selected = "norm")
    updateSelectInput(session, "skew2", selected = "norm")
    updateSelectInput(session, "skew3", selected = "norm")
    updateSliderInput(session, "sd1", value = 1)
    updateSliderInput(session, "sd2", value = 1)
    updateSliderInput(session, "sd3", value = 1)
  })
  
  ##--------------------------------------------------------------Samples Tab
  ##recall that sample1, sample2, sample3 already defined; also dataset with all is sampledf_long()
  
  #Assumes Independence
  output$assumption1 <- renderUI({
    validate(
      need(!((input$n > 200) | (input$n < 2)), 'Please enter a number between 2 and 200.')
    )
    return(p("This assumption is currently met because the sample size is less than 10% of the population", style = "color:#20C100"))
  })
  
  #Assumes approximate normality
  output$assumption2 <- renderUI({
    validate(
      need(!((input$n > 200) | (input$n < 2)), 'Please enter a number between 2 and 200.')
    )
    if(input$n > 10) {
      return(p("This assumption is currently met because the selected sample size,", paste(input$n), ", is greater than 30",
               style = "color:#20C100"))
    } else { #n =< 10
      if(input$skew1 != "norm" | input$skew2 != "norm" | input$skew3 != "norm") {
        return(p("This assumption is currently NOT met because the selected sample size is 30 or less and one or more of the
               population distributions is skewed", style = "color:#C10000"))
      } else {
        return(p("This assumption is currently met, though the selected sample size is not greater than 30, 
                 because the underlying population distributions are normal", style = "color:#20C100"))
      }
    }
    
  })
  
  rank <- function(s1, s2, s3) {
    if(var(s1) > var(s2) & var(s1) > var(s3) & var(s2) > var(s3)) {
      v1 <- reactive({var(s1)})
      v2 <- reactive({var(s2)})
      v3 <- reactive({var(s3)})
      
      if(v1() < (v3() * 2)) {
        return(p("This assumption is currently met because the maximum within groups standard deviation is less 
               than double the minimum within groups standard deviation", style = "color:#20C100"))
      } else {
        return(p("This assumption is currently NOT met because the maximum within groups standard deviation is greater than 
               or equal to double the minimum within groups standard deviation", style = "color:#C10000"))
      }
    }
    if(var(s1) > var(s2) & var(s1) > var(s3) & var(s3) > var(s2)) {
      v1 <- reactive({var(s1)})
      v2 <- reactive({var(s3)})
      v3 <- reactive({var(s2)})
      
      if(v1() < (v3() * 2)) {
        return(p("This assumption is currently met because the maximum within groups standard deviation is less 
               than double the minimum within groups standard deviation", style = "color:#20C100"))
      } else {
        return(p("This assumption is currently NOT met because the maximum within groups standard deviation is greater than 
               or equal to double the minimum within groups standard deviation", style = "color:#C10000"))
      }
    }
    
    
    
    if(var(s2) > var(s1) & var(s2) > var(s3) & var(s1) > var(s3)) {
      v1 <- reactive({var(s2)})
      v2 <- reactive({var(s1)})
      v3 <- reactive({var(s3)})
      
      if(v1() < (v3() * 2)) {
        return(p("This assumption is currently met because the maximum within groups standard deviation is less 
               than double the minimum within groups standard deviation", style = "color:#20C100"))
      } else {
        return(p("This assumption is currently NOT met because the maximum within groups standard deviation is greater than 
               or equal to double the minimum within groups standard deviation", style = "color:#C10000"))
      }
    }
    if(var(s2) > var(s1) & var(s2) > var(s3) & var(s3) > var(s1)) {
      v1 <- reactive({var(s2)})
      v2 <- reactive({var(s3)})
      v3 <- reactive({var(s1)})
      
      if(v1() < (v3() * 2)) {
        return(p("This assumption is currently met because the maximum within groups standard deviation is less 
               than double the minimum within groups standard deviation", style = "color:#20C100"))
      } else {
        return(p("This assumption is currently NOT met because the maximum within groups standard deviation is greater than 
               or equal to double the minimum within groups standard deviation", style = "color:#C10000"))
      }
    }
    
    
    
    if(var(s3) > var(s1) & var(s3) > var(s2) & var(s1) > var(s2)) {
      v1 <- reactive({var(s3)})
      v2 <- reactive({var(s1)})
      v3 <- reactive({var(s2)})
      
      if(v1() < (v3() * 2)) {
        return(p("This assumption is currently met because the maximum within groups standard deviation is less 
               than double the minimum within groups standard deviation", style = "color:#20C100"))
      } else {
        return(p("This assumption is currently NOT met because the maximum within groups standard deviation is greater than 
               or equal to double the minimum within groups standard deviation", style = "color:#C10000"))
      }
    }
    if(var(s3) > var(s1) & var(s3) > var(s2) & var(s2) > var(s1)) {
      v1 <- reactive({var(s3)})
      v2 <- reactive({var(s2)})
      v3 <- reactive({var(s1)})
      
      if(v1() < (v3() * 2)) {
        return(p("This assumption is currently met because the maximum within groups standard deviation is less 
               than double the minimum within groups standard deviation", style = "color:#20C100"))
      } else {
        return(p("This assumption is currently NOT met because the maximum within groups standard deviation is greater than 
               or equal to double the minimum within groups standard deviation", style = "color:#C10000"))
      }
    }
    
  }
  
  output$assumption3 <- renderUI ({
    rank(s1 = sample1(), s2 = sample2(), s3 = sample3())
  })
  
  
  output$boxplot <- renderPlot({
    ggplot(data = sampledf_long(), aes(x = dataset, y = values)) + 
      geom_boxplot(aes(color = dataset)) +
      geom_jitter(aes(x = dataset, y = values, alpha = .2, color = dataset), position=position_jitter(0.04)) +
      #coord_cartesian(ylim =c(0.1, 1.2)) +
      geom_hline(yintercept=mean(sampledf_long()$values), linetype=2, color = "black") +
      labs(title = "Sample Distributions", x = "Sample", y = "Values") +
      theme_bw() +
      theme(legend.position = "none",  plot.title = element_text(size = "20", face = "bold"),
            axis.text.x=element_blank(), axis.ticks.x=element_blank())
  })
  
  # output$toggleBtw <- renderPlot({
  #   ggplot(data = sampledf_long(), aes(x = dataset, y = values)) +
  #     stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
  #                  geom = "point", shape=16, size=5, aes(color = dataset)) +
  #     #coord_cartesian(ylim =c(0.1, 1.2)) +
  #     geom_hline(yintercept=mean(sampledf_long()$values), linetype=1, color = "black") +
  #     labs(title = "Sample Distributions", x = "Sample", y = "Values") +
  #     theme_bw()+
  #     theme(legend.position = "none", plot.title = element_text(size = "20", face = "bold"),
  #           axis.text.x=element_blank(), axis.ticks.x=element_blank())
  # })
  # 
  # output$toggleWin <- renderPlot({
  #   ggplot(data = sampledf_long(), aes(x = dataset, y = values)) + 
  #     geom_jitter(aes(x = dataset, y = values, alpha = .2, color = dataset), position=position_jitter(0.04)) +
  #     stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
  #                  geom = "crossbar", width = 0.5, aes(color = dataset)) +
  #     #coord_cartesian(ylim =c(0.1, 1.2)) +
  #     labs(title = "Sample Distributions", x = "Sample", y = "Values") +
  #     theme_bw()+
  #     theme(legend.position = "none", plot.title = element_text(size = "20", face = "bold"),
  #           axis.text.x=element_blank(), axis.ticks.x=element_blank())
  # })
  
  output$table <- renderDT(
    datatable(sampledf_long(), filter = 'top', options = list(
      pageLength = 5, autoWidth = TRUE)
    ))
  
  stats <- reactive({c(m1(), var1(),
                       m2(), var2(),
                       m3(), var3())})
  
  output$sumstats <- renderText({
    matrix <- matrix(stats(), ncol = 2, byrow = TRUE)
    rownames(matrix) <- c("Sample 1 (Red)", "Sample 2 (Green)", "Sample 3 (Blue)")
    colnames(matrix) <- c("mean", "variance")
    
    matrix %>%
      knitr::kable(format = "html", digits = 3)%>%
      kableExtra::kable_styling("striped", full_width = F, font_size = 20)
    
  })
  
  ##--------------------------------------------------------------ANOVA Test Tab
  output$boxplot2 <- renderPlot({
    ggplot(data = sampledf_long(), aes(x = dataset, y = values)) + 
      geom_boxplot(aes(color = dataset)) +
      geom_jitter(aes(x = dataset, y = values, alpha = .2, color = dataset), position=position_jitter(0.04)) +
      #coord_cartesian(ylim =c(0.1, 1.2)) +
      geom_hline(yintercept=mean(sampledf_long()$values), linetype=2, color = "black") +
      labs(title = "Sample Distributions", x = "Sample", y = "Values") +
      theme_bw() +
      theme(legend.position = "none",  plot.title = element_text(size = "20", face = "bold"),
            axis.text.x=element_blank(), axis.ticks.x=element_blank())
  })
  
  runTest <- reactive({aov(values ~ dataset, data = sampledf_long())})
  # output$aovTest <- renderPrint({
  #   print(summary(runTest()))
  # })
  
  output$aovTest <- renderText(
    tidy(runTest())%>%
      knitr::kable(format = "html", digits = 3, col.names = c("Term", "Degrees Freedom", "Sum of Squares", "Mean Square", "F-Stat", "P value")) %>%
      kableExtra::kable_styling("striped", full_width = F, font_size = 24) 
  )
  
  output$FTest <- renderPrint ({
    print(summary(runTest())[[1]][["F value"]][[1]])
  })
  
  
  output$concl <- renderText({
    if(tidy(runTest())$p.value[1] < 0.05) {
      return("sufficient evidence to conclude there is at least one difference between the group means.")
    } else {
      return("insufficient evidence to conclude there is at least one difference between the group means.")
    }
  })
  
  output$valid <- renderUI({
    if(grepl(pattern = ".*is currently met", 
             x = rank(s1 = sample1(), s2 = sample2(), s3 = sample3())) == FALSE & 
       (input$n > 10 | (input$skew1 == "norm" & input$skew2 == "norm" & input$skew3 == "norm"))) {
      return(p("CAUTION: Since the assumption that the within group variances are approximately equal is not currently met,
             the output of this ANOVA test may not be valid.", style = "color:#C10000"))
    }
    
    if(grepl(pattern = ".*is currently met", 
             x = rank(s1 = sample1(), s2 = sample2(), s3 = sample3())) == FALSE & 
       (input$n <= 10 & (input$skew1 != "norm" | input$skew2 != "norm" | input$skew3 != "norm"))) {
      return(p("CAUTION: Since the assumptions of approximate normality and approximate equality of within group variances are not currently met,
             the output of this ANOVA test may not be valid.", style = "color:#C10000"))
    }
    
    if(grepl(pattern = ".*is currently met", 
             x = rank(s1 = sample1(), s2 = sample2(), s3 = sample3())) == TRUE & 
       (input$n <= 10 & (input$skew1 != "norm" | input$skew2 != "norm" | input$skew3 != "norm"))) {
      return(p("CAUTION: Since the assumption that all groups are approximately normal is not currently met,
             the output of this ANOVA test may not be valid.", style = "color:#C10000"))
    }
    
    
  })
}

shinyApp(ui = ui, server = server)

