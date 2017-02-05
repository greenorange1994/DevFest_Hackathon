#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(dplyr)
library(lubridate)
fn = list.files("./Data/",pattern = ".csv")
#setwd("C:\\Users\\green\\Desktop\\ccrs\\")
df = read.csv(paste("./Data/",fn,sep=""), stringsAsFactors = FALSE)
names(df) <- c("Friday", "Monday", "Saturday", "Thursday", "Tuesday", "Wednesday", "building", "call", "name", "number", "room")
for(i in 1:dim(df)[1]) {
  if (is.na(df[i, "Saturday"])) {
    df[i, "Saturday"] = ""    
  } 
}

# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Columbia Classroom Resources Service"),
  
  column(4
         , wellPanel(
           dateInput('date',
                     label = 'Date input: yyyy-mm-dd',
                     value = Sys.Date()
           ),
           
           textInput('range',
                     label = paste('Time range input : HH:MM-HH:MM'),
                     value = "08:00-11:00"
           ),
           
           selectInput("b",
                       "building:",
                       unique(as.character(df$building))),
           
           selectInput("r",
                       "room:",
                       unique(as.character(df$room)))
           
         )),
  
  mainPanel(
    
    tabsetPanel(type = "tabs", 
                tabPanel("Available classrooms", verbatimTextOutput("classroom")),
                tabPanel("Timetable for a classroom", verbatimTextOutput("table"))))
  
)

# Define server logic required to draw a histogram
for (i in 1:length(df$room)){
  if (nchar(df$room[i])<4){
    df$room[i] <- paste(df$room[i]," ",sep="")
  }
  if (nchar(df$room[i])<5){
    df$room[i] <- paste(df$room[i]," ",sep="")
  }
}

server <- function(input, output, session) {

  

  
  # input$date and others are Date objects. When outputting
  # text, we need to convert to character; otherwise it will
  # print an integer rather than a date.
  output$classroom <- renderText({
  
    L = list()
    num = 1
    day = weekdays(input$date)
    
    time_to_num <- function(t_str) {
      t_list = strsplit(t_str, '-')
      vec = c(0, 0)
      for (i in 1:2) {
        t_ele = t_list[[1]][i]
        t_ele = as.numeric(strsplit(t_ele, ':')[[1]][1]) + as.numeric(strsplit(t_ele, ':')[[1]][2])/60
        vec[i] = t_ele
      }
      list("start" = vec[1], "end" = vec[2])
    }
    range = time_to_num(input$range)
    range_start = range$start
    range_end = range$end
    
    if (day == "Sunday") {
      paste("No classes on Sunday! Go out and have some fun!")
    } else {
      for (i in 1:dim(df)[1]) {
        t = df[, day][i]
        # if there is no information about course time or location, we ignore this course
        if ((t == "") | (df[,"room"][i] == "") | (df[, "room"][i] == "To  ")) {
          next
        }
        
        # change time strings into numerical values
        t_start = strsplit(t, '-')[[1]][1]
        t_end = strsplit(t, '-')[[1]][2]
        
        if (str_detect(t_start, "am")) {
          t_start = sub("am", "", t_start)
          t_start = as.numeric(strsplit(t_start, ':')[[1]][1]) + as.numeric(strsplit(t_start, ':')[[1]][2])/60
        } else if (strsplit(sub("pm", "", t_start), ':')[[1]][1] == 12) {
          t_start = sub("pm", "", t_start)
          t_start = as.numeric(strsplit(t_start, ':')[[1]][1]) + as.numeric(strsplit(t_start, ':')[[1]][2])/60
        } else {
          t_start = sub("pm", "", t_start)
          t_start = as.numeric(strsplit(t_start, ':')[[1]][1]) + 12 + as.numeric(strsplit(t_start, ':')[[1]][2])/60
        }
        
        if (str_detect(t_end, "am")) {
          t_end = sub("am", "", t_end)
          t_end = as.numeric(strsplit(t_end, ':')[[1]][1]) + as.numeric(strsplit(t_end, ':')[[1]][2])/60
        } else if (strsplit(sub("pm", "", t_end), ':')[[1]][1] == 12) {
          t_end = sub("pm", "", t_end)
          t_end = as.numeric(strsplit(t_end, ':')[[1]][1]) + as.numeric(strsplit(t_end, ':')[[1]][2])/60
        } else {
          t_end = sub("pm", "", t_end)
          t_end = as.numeric(strsplit(t_end, ':')[[1]][1]) + 12 + as.numeric(strsplit(t_end, ':')[[1]][2])/60
        }
        
        # if during the input time range, there are no classes in this location, we record this location
        if ((t_start >= range_end) | (t_end <= range_start)) {
          L[num] <- paste(df[, "room"][i], " ", df[, "building"][i], "\n")
          num <- num + 1
        }
      }
      if (length(L) == 0) {
        paste("No available classrooms")
      } else {
        L = unique(L[1:(num-1)])
        L[1] = paste("", L[1])
        paste(L[1:length(L)])
      }
    }})
  
  output$table <- renderPrint({
    day = weekdays(input$date)
    a = df[which(df$building == input$b & df$room == input$r & df[,day] != "") , c(day, "name")]
    b = str_split_fixed(a[,1],"-",2)[,1]
    for (i in 1:length(b)){
      if (nchar(b[i])<7){
        b[i] <- paste("0",b[i],sep="")
      }
    }
    b = paste(substr(b,1,5),":00 ", substr(b,6,7),sep="")
    b=parse_date_time(b,"%I:%M:%S %p")
    print(a[order(b),],row.names = F)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

