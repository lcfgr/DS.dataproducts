library(shiny)
library(dplyr)
library(data.table)
library(plotly)
library(forecast)



# Read the data
All_data<- data.table::fread("diabetes_data.csv")

Glucose_data <- select(All_data,c(contains("Date"),contains("Glucose")))
Insulin_data <- select(All_data,c(contains("Date"), contains("Insulin")))

Glucose_data<- melt(Glucose_data,id.vars = "Date", variable.name = "Time")
Insulin_data<- melt(Insulin_data,id.vars = "Date", variable.name = "Time")

Glucose_data[,datetime:=as.POSIXct(paste(All_data$Date, as.character(Glucose_data$Time)))]
Insulin_data[,datetime:=as.POSIXct(paste(All_data$Date, as.character(Insulin_data$Time)))]

setorder(Glucose_data,datetime)
setorder(Insulin_data,datetime)

a<- data.table(datetime= Glucose_data$datetime, y=Glucose_data$value,
               x1=lag(Insulin_data$value,1),x2=lag(Insulin_data$value,2),x3=lag(Insulin_data$value,3),
               y1=lag(Glucose_data$value,1),y2=lag(Glucose_data$value,2),y3=lag(Glucose_data$value,3))
nnet(y~.,data=a, size=4)
nnet(y~.,data=a, size=4,maxit=5000,linout =TRUE, decay=0.0001)

PlotTime <- as.numeric(PlotTime)
diff_time <- last(Glucose_data$datetime)-as.difftime(PlotTime, units="days")





set.seed(695847)

if (input$WhichData==1){
  
  if (PlotTime == 0){plot_data <- Glucose_data}
  else {plot_data <- subset(Glucose_data, Glucose_data$datetime > diff_time, units="days")}
  
  
  if (input$Daytime ==1){
    plot_data<- subset(plot_data, plot_data$Time =='08:00')
  }
  else if (input$Daytime == 2) {
    plot_data<- subset(plot_data, plot_data$Time =='13:00')
  }
  else if (input$Daytime == 3){
    plot_data<- subset(plot_data, plot_data$Time =='20:00')
  }
  p <- ggplot(plot_data, aes(x= plot_data$datetime, y=plot_data$value)) + geom_point()
  p <- p + stat_smooth(method = "loess", formula = y ~ x, size=1)
  p <- p + ggtitle("Glucose/time Graph") + labs(x="Time",y="Glucose(mg/dl)")
}

if (input$WhichData==2){
  if (PlotTime == 0){plot_data <- Insulin_data}
  else {plot_data <- subset(Insulin_data, Insulin_data$datetime > diff_time, units="days")}
  
  if (input$Daytime ==1){
    plot_data<- subset(plot_data, plot_data$Time =='08:00')
  }
  else if (input$Daytime == 2) {
    plot_data<- subset(plot_data, plot_data$Time =='13:00')
  }
  else if (input$Daytime == 3){
    plot_data<- subset(plot_data, plot_data$Time =='20:00')
  }
  p <- ggplot(plot_data, aes(x= plot_data$datetime, y=plot_data$value)) + geom_point()
  p <- p + stat_smooth(method = "loess", formula = y ~ x, size=1)  
  p <- p + ggtitle("Insulin dosage/time Graph") + labs(x="Time",y="Insulin dose (units)")
}

output$plot1 <- renderPlot(p)  
})



})