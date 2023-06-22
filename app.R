# install.packages("jsonlite")
# install.packages("ggplot2")

library(shiny)
library(jsonlite)
library(ggplot2)

ui <- fluidPage(
  dateInput(
    inputId = "startDate",
    label = "시작일",
    value = Sys.Date() - 30
  ),
  dateInput(
    inputId = "endDate",
    label = "종료일",
    value = Sys.Date()
  ),
  
  selectInput(
    inputId = "timeUnit",
    label = "구간 단위",
    choices = c("일간" = "date", "주간" = "week", "월간" = "month")
  ),
  
  textInput(
    inputId = "groupName",
    label = "주제어",
    value = "한끼통살"
  ),
  
  textInput(
    inputId = "groupName2",
    label = "주제어2",
    value = "랩노쉬"
  ),
  
  actionButton(inputId = "submit", label = "입력 확인"),
  
  plotOutput(outputId = "graph"),
)

server <- function(input, output, session) {
  source("api_call.R")
  
  observeEvent(input$submit, {
    startDate <- input$startDate
    endDate <- input$endDate
    timeUnit <- input$timeUnit
    groupName <- input$groupName
    keyword <- input$keyword
    
    data <- list(
      startDate = as.character(startDate),
      endDate = as.character(endDate),
      timeUnit = timeUnit,
      keywordGroups = list(list(
        groupName = groupName,
        keywords = list(groupName)
      ))
      
    )
    
    jsonData <- toJSON(data, auto_unbox = TRUE)
    
    response <- send_post_request(jsonData)
    
    parsed_data <- fromJSON(response)
    parsed_data_frame <- as.data.frame(parsed_data$results$data)
    parsed_data_frame$period <- as.Date(parsed_data_frame$period)
    
    ggplot_object <-
      ggplot(parsed_data_frame, aes(x = period, y = ratio)) + geom_line() +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))
    
    output$graph <- renderPlot({
      ggplot_object
    })
  })
}

shinyApp(ui = ui, server = server)