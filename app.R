library(shiny)

source("api_call.R")
source("single_analysis.R")
source("compare_analysis.R")
source("utils.R")

common_inputs <- list(
  dateInput(
    inputId = "start_date",
    label = "시작일",
    value = Sys.Date() - 365
  ),
  dateInput(
    inputId = "end_date",
    label = "종료일",
    value = Sys.Date()
  ),
  selectInput(
    inputId = "time_unit",
    label = "구간 단위",
    choices = c("일간" = "date", "주간" = "week", "월간" = "month")
  ),
  textInput(
    inputId = "group_name_single",
    label = "주제어",
    value = "한끼통살"
  ),
  textInput(
    inputId = "group_name_compare_1",
    label = "주제어",
    value = "소고기"
  ),
  textInput(
    inputId = "group_name_compare_2",
    label = "주제어",
    value = "돼지고기"
  ),
  radioButtons(
    inputId = "device",
    label = "기기",
    choices = c("전체", "pc", "mo"),
    selected = "전체"
  ),
  radioButtons(
    inputId = "gender",
    label = "성별",
    choices = c("전체", "남성", "여성"),
    selected = "전체"
  ),
  radioButtons(
    inputId = "ages",
    label = "나이",
    choices = c("전체", "미성년", "20대", "30대", "40대", "50대", "60대 이상"),
    selected = "전체"
  )
  
)

ui <- fluidPage(navbarPage(
  "분석 앱",
  tabPanel("단일분석",
           sidebarLayout(
             sidebarPanel(common_inputs[-c(5,6)],
                          actionButton(inputId = "submit", label = "입력 확인")),
             mainPanel(plotOutput("single_plot"),
                       textOutput("single_analysis"))
           )),
  tabPanel("비교분석",
           sidebarLayout(
             sidebarPanel(
               common_inputs[-4],
               actionButton(inputId = "submit_2", label = "입력 확인")
               
             ),
             mainPanel(
               plotOutput("compare_plot"),
               plotOutput("scatter_plot"),
               textOutput("cor")
             )
           ))
))

server <- function(input, output, session) {
  observeEvent(input$submit, {
    params <- process_data_with_params(input)
    
    single_analysis <-
      single_process_data(params)
    
    output$single_analysis <- renderText({
      single_analysis$trend_analysis_result
    })
    output$single_plot <- renderPlot({
      single_analysis$graph
    })
    
    
  })
  
  observeEvent(input$submit_2, {
    params <- process_data_with_params(input)
    
    compare_analysis <-
      compare_process_data(params)
    
    output$scatter_plot <- renderPlot({
      compare_analysis$scatter_plot
    })
    output$compare_plot <- renderPlot({
      compare_analysis$graph
    })
    output$cor <- renderText({
      compare_analysis$cor
    })
  })
}



shinyApp(ui = ui, server = server)
