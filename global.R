accrcy <- function(valueene){
  if(0.020001 < valueene & valueene < 0.050001){
    return(acc <- 0.03)
  }else if(valueene <= 0.020001){
    return(acc <- 0.02)
  }else if(valueene <= 0.010001){
    return(acc <- 0.01)
  }else{
    return(acc <- 0.1)
  }
}

num_round <- function(number){
  round(number, digits = 4)
}

show_table_handsontable <- function(data_frame){
  rhandsontable(data_frame, useTypes = TRUE)%>%
    hot_table(highlightCol = TRUE, highlightRow = TRUE,
              allowRowEdit = FALSE) %>%
    hot_col('Nominal', readOnly = T)
}

render_reactable <- function(data_kalibrasi){
  reactable(data_kalibrasi,
            striped = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            bordered = TRUE,
            wrap = FALSE,
            columns = list(
              V1 = colDef(name = "Mean"),
              V2 = colDef(name = "S"),
              V3 = colDef(name = "Error"),
              V4 = colDef(name = "Um"),
              V5 = colDef(name = "Ug"),
              V6 = colDef(name = "Ur"),
              V7 = colDef(name = "Ugab"),
              V8 = colDef(name = "U")
            ),
            defaultColDef = colDef(minWidth = 100,align = "center"))
}

cal_mean_one <- function(df,x,y){
  carr = as.character(y)
  depth1 = paste(carr, 1, sep = "")
  depth2 = paste(carr, 2, sep = "")
  depth3 = paste(carr, 3, sep = "")
  convert_to_numeric <- as.numeric(lapply(list(df[x,depth1],df[x,depth2],df[x,depth3]), as.numeric))
  total_mean <- round(mean(convert_to_numeric), digits = 4)
}

cal_sd_one <- function(df,x,y){
  carr = as.character(y)
  depth1 = paste(carr, 1, sep = "")
  depth2 = paste(carr, 2, sep = "")
  depth3 = paste(carr, 3, sep = "")
  sd_rslt <- sd(c(df[x,depth1],df[x,depth2],df[x,depth3]))
  #rounds value to specified number of decimal places
  sd_result <- round(sd_rslt, digits = 4)
}
accuracy <- function(deviation){
  if(deviation <= 0.010001){
    return(ac_val <- 0.01)
  }else if(deviation <= 0.020001){
    return(ac_val <- 0.02)
  }else if(deviation < 0.050001){
    return(ac_val <- 0.05)
  }else{
    return(ac_val <- 0.1)
  }
  print(ac_val)
}

#Value dalam sample 1 s.d sample 3 dan list nominal
rerata_3sampel_data <- function(rtable_col1,rtable_col2,rtable_col3,cal){
  convert_to_numeric <- as.numeric(lapply(list(rtable_col1,
                                               rtable_col2,
                                               rtable_col3), as.numeric))
  sdl <- round(sd(convert_to_numeric), digits = 4)
  if(cal == 'Mean'){
    return(   
      total_mean <- round(mean(convert_to_numeric), digits = 4)
    )
  }else if(cal == 'Sd'){
    return(   
      sd <- round(sd(convert_to_numeric), digits = 4)
    )
  }else if(cal == 'Um'){
    return(   
      um <- round((sdl / sqrt(3)), digits = 4)
    )
  }
  
}

hitung_kombinasi_ketidakpastian <- function(list_mean,list_sd,partmeasure){
  partmeasure <- as.character(partmeasure)
  if(partmeasure == 'In'){
    nominal <- c(10,20,50,100)
    repetisi <- 4
    rep_list <- 4
    uncertainty_calibrator_tool_ug = c((0.0001/1), (0.0006/2), (0.0007/2), (0.0005/2))
  }else if(partmeasure == 'Out'){
    nominal <- c(20,50,100,150)
    repetisi <- 4
    rep_list <- 4
    uncertainty_calibrator_tool_ug = c((0.0001/2), (0.0006/2), (0.0007/2), (0.0005/2))
  }else if(partmeasure == 'DepthSteps'){
    repetisi <- 6
    rep_list <- 6
    nominal <- seq(25,150,by=25)
    uncertainty_calibrator_tool_ug = c((0.0001/2), (0.0006/2), (0.0007/2), (0.0005/2),
                                       (0.0001/2), (0.0001/2))
  }else if(partmeasure == 'Hg'){
    repetisi <- 10
    rep_list <- 10
    nominal <- c(0,20,seq(50,150,by=25),seq(200,300,by=50))
    uncertainty_calibrator_tool_ug = c(0.0001,0.0001,0.0005,0.0003,0.0002,
                                       0.0005,0.0003,0.0005,0.0005,0.0005)
  }
  
  error = round((list_mean - nominal), digits = 4)
  uncertainty_measurement_um = round((list_sd / sqrt(3)), 4)
  
  #========================================== Uncertainty tool resolution
  resolution_measurement = 0.01
  
  #for the 95% confidence internal
  ur = round(resolution_measurement/sqrt(3), digits = 4)
  combine_uncertainty_ugab = round(sqrt(uncertainty_measurement_um^2 +
                                          uncertainty_calibrator_tool_ug^2 + ur^2), digits = 4)
  
  #===============U
  final_uncertainty = round((combine_uncertainty_ugab * 2), digits = 4)
  
  #list_num_mean and sd
  my_list <- list(list_mean, list_sd, error,
                  uncertainty_measurement_um, uncertainty_calibrator_tool_ug,
                  rep(ur, rep_list), combine_uncertainty_ugab, final_uncertainty)
  
  return(final_result <- t(matrix(unlist(my_list), ncol = repetisi, byrow = T)))
}

hasil_kalibrasi <- function(list_mean,list_sd,partmeasure){
  partmeasure <- as.character(partmeasure)
  if(partmeasure == 'In'){
    rep_list <- 4
    nominal <- c(10,20,50,100)
    uncertainty_calibrator_tool_ug = c((0.0001/1), (0.0006/2), (0.0007/2), (0.0005/2))
  }else if(partmeasure == 'Out'){
    rep_list <- 4
    nominal <- c(20,50,100,150)
    uncertainty_calibrator_tool_ug = c((0.0001/2), (0.0006/2), (0.0007/2), (0.0005/2))
  }else if(partmeasure == 'DepthSteps'){
    nominal <- seq(25,150,by=25)
    rep_list <- 6
    uncertainty_calibrator_tool_ug = c((0.0001/2), (0.0006/2), (0.0007/2), (0.0005/2),
                                       (0.0001/2), (0.0001/2))
  }else if(partmeasure == 'Hg'){
    rep_list <- 10
    nominal <- c(0,20,seq(50,150,by=25),seq(200,300,by=50))
    uncertainty_calibrator_tool_ug = c(0.0001,0.0001,0.0005,0.0003,0.0002,
                                       0.0005,0.0003,0.0005,0.0005,0.0005)
  }
  
  error = round((list_mean - nominal), digits = 4)
  uncertainty_measurement_um = round((list_sd / sqrt(3)), 4)
  
  #========================================== Uncertainty tool resolution
  resolution_measurement = 0.01
  
  #for the 95% confidence internal
  ur = round(resolution_measurement/sqrt(3), digits = 4)
  combine_uncertainty_ugab = round(sqrt(uncertainty_measurement_um^2 +
                                          uncertainty_calibrator_tool_ug^2 + ur^2), digits = 4)
  
  #===============U
  final_uncertainty = round((combine_uncertainty_ugab * 2), digits = 4)
  
  if(max(error) <= 0.010001){
    acc <- 0.01
  }else if(max(error) <= 0.020001){
    acc <- 0.04
  }else if(max(error) <= 0.050001){
    acc <- 0.04
  }else{
    acc <- 0.1
  }
  cal_list <- c(mean(final_uncertainty),max(error),acc)
  
  return(final_result <- t(matrix(unlist(cal_list), ncol = 1, byrow = T)))
}