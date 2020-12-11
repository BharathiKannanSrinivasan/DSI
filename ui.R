library(shiny)
shinyUI(fluidPage(
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "radial",
    direction = c("top", "left")
  ),
  useShinyjs(),
  titlePanel("Assignment 1 - Bharathi Kannan Srinivasan"),
  tabsetPanel(
    tabPanel("SUMMARY VALUES",
             h3("SUMMARIES"),
             tabsetPanel(
               tabPanel("Summary",withSpinner(verbatimTextOutput(outputId = "Summary1"))),
               tabPanel("Descriptive Statistics",verbatimTextOutput(outputId = "Summary2")),
               tabPanel("Glimpse",verbatimTextOutput(outputId = "Summary3")),
               tabPanel("Skim",verbatimTextOutput(outputId = "Summary4")),
               tabPanel("dfSummary",col.widths=5,style="grid", withSpinner(htmlOutput(outputId = "Summary5")))
               
             )),
    tabPanel("DATA TABLE",
             h3("DATA"),
             tabsetPanel(
               tabPanel("Discrete Values",DT::dataTableOutput(outputId = "discrete")),
               tabPanel("Continuous Values",DT::dataTableOutput(outputId = "continuous")),
               tabPanel("Mixed Data",DT::dataTableOutput(outputId = "all_data"))
               
             )),
    tabPanel("VISUALISATION",
             h3("DATA VISULAISATION METHODS"),
             tabsetPanel(
               tabPanel("Mosaic",
                        helpText("Select atleast 1 and atmost 4 variables"),
                        selectizeInput(inputId = "mosaic_choice",label="Select variables",choices=mosaic_columns,multiple=TRUE,selected=c("Priority","Speed","State","Duration"),options = list(maxItems=4)),
                        withSpinner(plotOutput(outputId = "Mosaic"))),
               tabPanel("Missing VALUES",
                        withSpinner(plotOutput(outputId = "Missing"))),
               tabPanel("Rising Value",
                        helpText("Select variables you want to include in the chart"),
                        selectizeInput(inputId = "risingvalue",label="Select variables",choices=continuous_columns,multiple=TRUE,selected=(c("sensor7","sensor1","sensor2","sensor3","sensor4","Y"))),
                        withSpinner(plotOutput(outputId = "rising"))),
               tabPanel("Correlation",
                        checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
                        selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                        selectInput(inputId = "Group", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO"),
                        withSpinner(plotOutput(outputId = "Corrgram"))),
               tabPanel("Box Plot",
                        checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE),
                        sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 3, step = 0.1, value = 1.5),
                        withSpinner(plotOutput(outputId = "Boxplot"))),
               tabPanel("GG Pairs Plot",
                        selectInput(inputId = "ggplot", label = "GG PAIRS method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                        withSpinner(plotOutput(outputId = "ggpairs1")),
                        hr(),
                        withSpinner(plotOutput(outputId = "ggpairs2")),
                        hr(),
                        withSpinner(plotOutput(outputId = "ggpairs3")),
                        withSpinner(plotOutput(outputId = "ggpairs4"))),
               tabPanel("Tabplot",
                        radioButtons(inputId = "tab",label = "SELECT PREFFERED VARIABLE",list("Numeric","Catagorical"),"Numeric"),
                        withSpinner(plotOutput(outputId="tabplot")))
               
               
               
             ))
  )
  
  
))
