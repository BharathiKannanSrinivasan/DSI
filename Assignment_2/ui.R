library(shiny)
library(shinydashboard)
b64 <- base64enc::dataURI(file="E:/Masters/Data Science/DSI/Assignment/Assignment2/www/Coronavirus.png", mime="image/png")
shinyUI(fluidPage(
    dashboardPage(
        skin = "purple",
        dashboardHeader(title="CORONA DATASET ANALYSIS", titleWidth = 350,
                        dropdownMenu(type = "messages",
                                     messageItem(
                                         from = "Assignment 2",
                                         message = "BHARATHI KANNAN SRINIVASAN"
                                     )
                                   
                        )),
        dashboardSidebar(width = 350,
                         sidebarMenu(
                             img(src=b64,height=210,width=350) ,
                             menuItem("Home", tabName = "home", icon = icon("home"),
                                      menuSubItem("Corona Dataset",tabName="cdata"),
                                      menuSubItem("Train Data",tabName="train_data"),
                                      menuSubItem("Test Data",tabName="test_data")),
                             menuItem("EDA VISUALISATIONS", tabName = "charts", icon = icon("stats", lib = "glyphicon"),
                                      menuSubItem("Missing Value",tabName="Miss"),
                                      menuSubItem("Miss Upset",tabName="Miss2"),
                                      menuSubItem("Correlation",tabName="Cor"),
                                      menuSubItem("Histogram",tabName="hist"),
                                      menuSubItem("Box Plot",tabName="boxes"),
                                      menuSubItem("GG Pairs Plot",tabName="scatter_plot"),
                                      menuSubItem("Scatter Plot",tabName="scatter_plot2"),
                                      menuSubItem("Missing Value Analysis",tabName="pred_missing")),
                             
                             
                             menuItem("GLM PREDICTOR", tabName = "releases", icon = icon("tasks"),
                                      menuSubItem("Random Forest",tabName="randomf"),
                                      menuSubItem("Residuals",tabName="residuals"))
                                      
                             
                         )),
        dashboardBody(
            tags$head(tags$style(HTML('
                                
                .content-wrapper, .right-side {
                    background-color: #FFFFFF;
                }
                                '))),
            tabItems(
                tabItem(tabName = "Miss",
                        h3(strong("MISSING VALUE DISTRIBUTION")),
                        withSpinner(plotOutput(outputId = "Missing"))),
                
                tabItem(tabName = "Miss2",
                        h3(strong("PATTERNS IN MISSING VARIABLE")),
                        numericInput(inputId="nset",label="Enter the nsets for missing combination",value=14,min = 2,max = 20),
                        withSpinner(plotOutput(outputId = "Missing2"))),
                tabItem(tabName = "Cor",
                        h3(strong("CORRELATION FOR CORONA DATASET")),
                        checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
                        selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                        selectInput(inputId = "Group", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO"),
                        withSpinner(plotOutput(outputId = "Corrgram"))),
                tabItem(tabName = "scatter_plot2",
                        h3(strong("SCATTER PLOT")),
                        selectInput(inputId = "scat1", label = "Select X Variable", choices = colnames(as.data.frame(dataset[ ,c(3:10,13:14)]))),
                        selectInput(inputId = "scat2", label = "Select Y Variable", choices = colnames(as.data.frame(dataset[ ,c(3:10,13:14)])),selected = "VAXRATE"),
                        withSpinner(plotlyOutput(outputId = "scatter2"))),
                
                tabItem(tabName = "hist",
                        h3(strong("HISTOGRAM FOR EACH VARIABLE")),
                        selectInput(inputId = "hist", label = "Select Variable", choices = colnames(as.data.frame(dataset[ ,c(3:10,13:14)]))),
                        sliderInput("bin","2.Select the BIN value for histogram",min=50,max=100,value=55),
                        withSpinner(plotlyOutput(outputId = "Histogram"))),
                tabItem(tabName = "boxes",
                        h3("BOXPLOT FOR EACH VARIABLE"),
                        selectInput(inputId = "box", label = "Select Variable", choices = colnames(as.data.frame(dataset[ ,c(3:10,13:14)]))),
                        sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 3, step = 0.1, value = 1.5),
                        withSpinner(plotOutput(outputId = "Boxplot"))),
                tabItem(tabName = "scatter_plot",
                        h3(strong("GG PAIRS FOR DATASET")),
                        withSpinner(plotOutput(outputId = "Scatter"))),
                tabItem(tabName = "pred_missing",
                        tabPanel("miss_pred",(
                            tabsetPanel(
                                tabPanel("Rpart",
                                         h3(strong("PREDICTING NUMBER OF MISSING VALUES IN AN OBSERVATION")),
                                         withSpinner(plotOutput(outputId = "Missing_Pattern"))),
                                tabPanel("Missing Correlation",
                                         h3(strong("MISSING VALUE CORRELATION")),
                                         withSpinner(plotOutput(outputId = "miscor")))
                )))),
                tabItem(tabName = "randomf",
                        h3(strong("PREDICTING IMPORTANT VARIABLES USING RANDOM FOREST")),
                        withSpinner(plotOutput(outputId = "random"))),
                tabItem(tabName = "model",
                        withSpinner(plotOutput(outputId = "model_based"))),
                tabItem(tabName = "residuals",
                        tabPanel("residual",
                                 tabsetPanel(
                                     tabPanel("Residual Plot",
                                              h3(strong("RESIDUAL PLOT FOR OUTLIERS")),
                                              withSpinner(plotOutput(outputId = "Residual_graph"))),
                                     tabPanel("Residual Histogram",
                                              h3(strong("HISTOGRAM FOR OUTLIERS")),
                                              sliderInput("bin2","Select the BIN value for histogram",min=0,max=3,step=0.2,value=0.2),
                                              withSpinner(plotOutput(outputId = "hist2"))),
                                     tabPanel("Residual BoxPlot",
                                              h3(strong("BOXPLOT FOR OUTLIERS")),
                                              withSpinner(plotOutput(outputId = "Rbox"))))),
                ),
                tabItem(tabName = "cdata",
                        h3(strong("CORONA DATASET")),
                        withSpinner(DT::dataTableOutput(outputId = "Summary1"))),
                tabItem(tabName = "train_data",
                        h3(strong("SPLITTED TRAIN DATASET")),
                        withSpinner(DT::dataTableOutput(outputId = "Summary2"))),
                tabItem(tabName = "test_data",
                        h3(strong("SPLITTED TEST DATASET")),
                        withSpinner(DT::dataTableOutput(outputId = "Summary3")))
                
                
                
            )))))
