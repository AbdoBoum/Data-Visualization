library(markdown)
library(shiny)
library(plotly)
library(rpart)
packageVersion('plotly')

navbarPage("Data Visualization",
           tabPanel("Table",
                    sidebarLayout(
                      sidebarPanel(style = "position:fixed;width:inherit;","Inputs",
                        fileInput("file", "Upload your Data File",
                                  multiple = TRUE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        
                        tags$hr(),
                        
                        # Input: Checkbox if file has header
                        checkboxInput("header", "Header", TRUE),
                        
                        # Input: Select separator
                        radioButtons("sep", "Separator",
                                     choices = c(Comma = ",",
                                                 Semicolon = ";",
                                                 Tab = "\t"),
                                     selected = ","),
                        
                        # Input: Select quotes
                        radioButtons("quote", "Quote",
                                     choices = c(None = "",
                                                 "Double Quote" = '"',
                                                 "Single Quote" = "'"),
                                     selected = '"'),
                        
                        tags$hr(),
                        
                        radioButtons("disp", "Display",
                                     choices = c(Head = "head",
                                                 All = "all"),
                                     selected = "head")
                      ),
                      mainPanel(
                        DT::dataTableOutput("table")
                      )
                    )
                    
  
           ),
#2D Plot table
           tabPanel("Plot",
                    sidebarLayout(
                      sidebarPanel(
                        # Input: the variable to plot
                        selectInput("xcol","xcol", choices = NULL),
                        selectInput("ycol","ycol", choices = NULL),
                        radioButtons("plotType", "Plot type",
                                     c("Scatter"='scatter', "Bar"= 'bar',"Box"= 'box', "Histogram"='histogram')
                        )
                      ),
                      mainPanel(
                        plotlyOutput("plot", height = 500,width="100%"),
                        verbatimTextOutput("cov"),
                        verbatimTextOutput("cor")
                        
                      )
                    )
           ),
          # summarising data
           tabPanel("Summary",
                    verbatimTextOutput("summary")
           ),
          #Clustering (K-Means)
          tabPanel("Clusters",fluidRow(style = "padding-bottom: 20px;",
                   column(4, selectInput('xcol5', 'X Variable', choices=NULL)),
                   column(4, selectInput('ycol5', 'Y Variable', choices=NULL)),
                   column(4, numericInput('clusters', 'Cluster count', 3,
                                          min = 1, max = 9))
          ),
          mainPanel(
            plotOutput('kmeans', height = "400px")
          )
          ),
          #Decision tree
          tabPanel("Classification Tree",fluidRow(style = "padding-bottom: 20px;",
                                                  column(4, selectInput('xcol6', 'Variable 1', choices=NULL)),
                                                  column(4, selectInput('ycol6', 'Variable 2', choices=NULL)),
                                                  column(4, selectInput('zcol6', 'Variable 3', choices=NULL))
                   
          ),      
          mainPanel(
                   plotOutput("decisionTree")
          )
          ),
        
        tabPanel("Scatterplot Matrix",
                 plotOutput("pair"),
                 plotOutput("analytics"),
                 verbatimTextOutput("corre")
        ),
        # #3D plot
        # tabPanel("3D PLOT",sidebarPanel(
        #   # Input: the variable to plot ----
        #   selectInput("xcol1","Column 1", choices = NULL),
        #   selectInput("ycol1","Column 2", choices = NULL),
        #   selectInput("zcol1","Column 3", choices = NULL)
        # ),
        # mainPanel(
        #   plotOutput("DPlot")
        # )
        #),
        #3D Scatterplot
        tabPanel("3D Scatterplot",sidebarPanel(
          # Input: the variable to plot
          selectInput("xcol2","Column 1", choices = NULL),
          selectInput("ycol2","Column 2", choices = NULL),
          selectInput("zcol2","Column 3", choices = NULL)
          
        ),
        mainPanel(
          plotlyOutput("Scatterplot", height = 800,width="100%")
          #plotOutput("DPlot")
        )
        ),
        #Multiple variable plot
        
        tabPanel("Multiple Variable Plot",sidebarPanel(
          # Input: the variable to plot ----
          selectInput("xcol3","Column 1 (x)", choices = NULL),
          selectInput("ycol3","Column 2 (y)", choices = NULL),
          selectInput("zcol3","Column 3", choices = NULL),
          selectInput("acol3","Column 4", choices = NULL)
          # selectInput("bcol3","Column 5", choices = NULL)
        ),
        mainPanel(
          #column(width = 12,
          #plotOutput("MultipleVarPlot", height=600)
          column(width = 12,
                 plotlyOutput("MultipleVarPlot", height = 1000,width="100%")
                 )
          )
         
        
        )
          
         
  )
                      
