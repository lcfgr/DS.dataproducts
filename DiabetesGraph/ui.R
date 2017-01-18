

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Ploting Glucose and Insulin data"),
  
  # Sidebar with all the parameters
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload', accept=c( 'text/csv',
                                                            'text/comma-separated-values',
                                                            'text/tab-separated-values',
                                                            'text/plain',
                                                            '.csv',
                                                            '.tsv')
      ),
      tags$hr(),
      p('Sample file: ', a(href = 'diabetes_data.csv', 'diabetes_data.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
     
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      actionButton("ProcessFile", "Process the uploaded File"),

tags$hr(),
      radioButtons('WhichData','Plot: ', c("Glucose"=1,"Insulin"=2)
      ),
      radioButtons('Daytime','Time of day', c("Breakfast"=1,"Lunch"=2,"Dinner"=3)
      ),
      numericInput('PlotTime','Days to plot: (0:All)',7,min=0
                   #radioButtons('PlotTime','Plot the last:',c("3 days"=3,"7 days"=7,"All"=30),inline = TRUE
      ),
      
      actionButton("PlotButton","Plot")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      strong("Disclaimer:"),"This application is used for", strong("Demonstration purpose only"),"and the data may be inaccurate or false.",br(),
      strong("Please do not use for medical or any other purpose."),"except for demonstration.",br(),
      "Please read the documentation first: ", a(href="DiabetesGraphDoc.html",'Documentation'),
      plotOutput('plot1'),
      tableOutput('contents')
      
    )
  )
))
