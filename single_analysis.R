library(tseries)

source("api_call.R")

single_process_data <-

  
  function (params) {
    print(params)
    data <- list(

      startDate = as.character(params$start_date),
      endDate = as.character(params$end_date),
      timeUnit = params$time_unit,
      keywordGroups = list(list(
        groupName = params$group_name,
        keywords = list(params$group_name)
      ))
    )
    
    if (!is.null(params$device)) {
      data$device <- params$device
    }
    
    if (!is.null(params$gender)) {
      data$gender <- params$gender
    }
    
    if (!is.null(params$ages)) {
      data$ages <- params$ages
    }
    
    jsonData <- toJSON(data, auto_unbox = TRUE)
    
    response <- send_post_request(jsonData)
    
    parsed_data <- fromJSON(response)
    parsed_data_frame <- as.data.frame(parsed_data$results$data)
    parsed_data_frame$period <- as.Date(parsed_data_frame$period)
    
    adf_result <-
      adf.test(parsed_data_frame$ratio,
               alternative = "stationary",
               k = 4)
    
    graph <-
      ggplot(parsed_data_frame, aes(x = period, y = ratio)) + geom_line() +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))
    
    if (adf_result$p.value < 0.05 && adf_result$statistic < 0) {
      trend_analysis_result <- "검정 결과 추세가 존재하지 않는 것으로 판단됩니다."
      
    } else {
      trend_analysis_result <-
        "검정 결과 추세가 존재하는 것으로 판단됩니다. 추세선이 그래프에 나타납니다."
      graph <- graph + geom_smooth(method = "lm", se = FALSE)
    }
    
    
    return(list(trend_analysis_result = trend_analysis_result, graph = graph))
  }