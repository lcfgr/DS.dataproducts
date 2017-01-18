
# Load Libraries
library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)

# Server function
shinyServer(
  function(input, output) {
    
    # Read Data when Process button is pressed
    filedata <- eventReactive(input$ProcessFile,{
      inFile <- input$file1
      if (is.null(inFile)){return(NULL)}
      data.table::fread(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
    })
    
    # Plot Graph when Plot Button is pressed
    observeEvent(input$PlotButton,{
      if(is.null(filedata())){return(NULL)}
      All_data<- filedata()
      
      # Write column names, in case they are different, or no header is used
      names(All_data) <- c("Date", "BreakfastGlucose", "BreafkastInsulin", "LunchGlucose", "LunchInsulin", "DinnerGlucose", "DinnerInsulin")
      
      #Seperate Glucose data and Insulin data 
      Glucose_data <- select(All_data,c(contains("Date"),contains("Glucose")))
      Insulin_data <- select(All_data,c(contains("Date"), contains("Insulin")))
      
      # Process data and give times on morning, lunch, dinner
      Glucose_data<- melt(Glucose_data,id.vars = "Date", variable.name = "Time")
      levels(Glucose_data$Time) <- c("08:00","13:00","20:00")
      Glucose_data[,datetime:=as.POSIXct(paste(All_data$Date, as.character(Glucose_data$Time)))]
      
      Insulin_data<- melt(Insulin_data,id.vars = "Date", variable.name = "Time")
      levels(Insulin_data$Time) <- c("08:00","13:00","20:00")
      Insulin_data[,datetime:=as.POSIXct(paste(All_data$Date, as.character(Insulin_data$Time)))]
      
      # the numeric casting is not used here, but is a good precaution in case we change the input into something with string
      PlotTime <- as.numeric(input$PlotTime)
      
      # Prepare the filter for the days requested
      diff_time <- last(Glucose_data$datetime)-as.difftime(PlotTime, units="days")
      
      # Prepare Glucose Data
      if (input$WhichData==1){
        
        if (PlotTime == 0){plot_data <- Glucose_data}
        else {plot_data <- subset(Glucose_data, Glucose_data$datetime > diff_time, units="days")}
        
        # Morning Graph selected
        if (input$Daytime ==1){
          plot_data<- subset(plot_data, plot_data$Time =='08:00')
        }
        # Lunch Graph selected
        else if (input$Daytime == 2) {
          plot_data<- subset(plot_data, plot_data$Time =='13:00')
        }
        # Dinner Graph selected
        else if (input$Daytime == 3){
          plot_data<- subset(plot_data, plot_data$Time =='20:00')
        }
        # Prepare the plot
        p <- ggplot(plot_data, aes(x= plot_data$datetime, y=plot_data$value)) + geom_point()
        p <- p + stat_smooth(method = "loess", formula = y ~ x, size=1)
        p <- p + ggtitle("Glucose/time Graph") + labs(x="Time",y="Glucose(mg/dl)")
      }
      
      # Prepare Insulin Data
      if (input$WhichData==2){
        if (PlotTime == 0){plot_data <- Insulin_data}
        else {plot_data <- subset(Insulin_data, Insulin_data$datetime > diff_time, units="days")}
        
        # Morning Graph selected
        if (input$Daytime ==1){
          plot_data<- subset(plot_data, plot_data$Time =='08:00')
        }
        # Lunch Graph selected
        else if (input$Daytime == 2) {
          plot_data<- subset(plot_data, plot_data$Time =='13:00')
        }
        # Dinner Graph selected
        else if (input$Daytime == 3){
          plot_data<- subset(plot_data, plot_data$Time =='20:00')
        }
        # Prepare the plot
        p <- ggplot(plot_data, aes(x= plot_data$datetime, y=plot_data$value)) + geom_point()
        p <- p + stat_smooth(method = "loess", formula = y ~ x, size=1)  
        p <- p + ggtitle("Insulin dosage/time Graph") + labs(x="Time",y="Insulin dose (units)")
      }
      # Plot the graph
      output$plot1 <- renderPlot(p)  
    })
    
    
    
  })