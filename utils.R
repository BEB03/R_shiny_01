process_data_with_params <- function (input) {

  start_date <- input$start_date
  end_date <- input$end_date
  time_unit <- input$time_unit
  group_name <- input$group_name_single
  group_name_compare_1 <- input$group_name_compare_1
  group_name_compare_2 <- input$group_name_compare_2
  
  print(input)
  
  params <-
    list(
      start_date = start_date,
      end_date = end_date,
      time_unit = time_unit,
      group_name = group_name,
      group_name_compare_1 = group_name_compare_1,
      group_name_compare_2 = group_name_compare_2
    )
  
  if (input$device == "pc") {
    params$device <- "pc"
  } else if (input$device == "mo") {
    params$device <- "mo"
  }
  
  if (input$gender == "남성") {
    params$gender <- "m"
  } else if (input$gender == "여성") {
    params$gender <- "f"
  }
  
  if (input$ages == "미성년") {
    params$ages <- list("1", "2")
  } else if (input$ages == "20대") {
    params$ages <- list("3", "4")
  } else if (input$ages == "30대") {
    params$ages <- list("5", "6")
  } else if (input$ages == "40대") {
    params$ages <- list("7", "8")
  } else if (input$ages == "50대") {
    params$ages <- list("9", "10")
  } else if (input$ages == "60대 이상") {
    params$ages <- list("11")
  }
  return(params)
}