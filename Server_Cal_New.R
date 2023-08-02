#Memeriksa apakah SN sudah pernah diinput
observe({
  # if(isTruthy(input$info_part_of_cal)){
  #   if(input$info_part_of_cal == 'Digital Caliper'){
  #     shinyjs::show("max_range")
  #   }
  # }
  # if(isTruthy(input$info_serial_number)){ 
  #   #Memeriksa tool dengan SN dan calibration order terpilih ada atau tidak
  #   is_data_exists <- sqlInterpolate(con,"SELECT EXISTS(SELECT 1 FROM 
  #                                         calibration_properties
  #                                         WHERE serial_number = ?sn 
  #                                         AND entry_frst_order::date = ?order);",
  #                                    sn = toupper(input$info_serial_number),
  #                                    order = input$cal_date
  #   )
  #   is_data_exists <- dbGetQuery(con, is_data_exists)
  # 
  #   if(is_data_exists == TRUE){
  #     showModal(modalDialog(
  #       title = "Oops!!",
  #       paste0("Serial Number",input$info_serial_number,"with",input$cal_date," is already registered! "),
  #       easyClose = TRUE,
  #       footer = NULL
  #     ))
  #     return(NA)
  #   }
  # }
})

observeEvent(input$create_table,{ 
  if(!isTruthy(input$info_serial_number)){
    showModal(modalDialog(
      title = "Haloo",
      paste0("Ayo segera lengkapi datanya !"),
      easyClose = TRUE,
      footer = NULL
    ))
    return(NA)
  }else{
    tryCatch({
      
      # history_calibration_order <- sqlInterpolate(con,"SELECT EXISTS(SELECT 1 FROM 
      #                                             table_name
      #                                             WHERE serial_number = ?sn 
      #                                             AND entry_frst_order::date = ?order);",
      #                                             sn = toupper(input$info_serial_number),
      #                                             order = input$cal_date
      # )
      # history_calibration_order <- dbGetQuery(con, history_calibration_order)
      
        today <- input$cal_date
        scnd_calibration_date <- today %m+% months(6)
        
        if(input$max_measurement_range == '300'){
          nom_out <- c(20,seq(50,300,by=50))
        }else if(input$max_measurement_range == '150'){
          nom_out <- c(20,50,100,150)
        }else if(input$max_measurement_range == '200'){
          nom_out <- c(20,50,100,150,200)
        }else{
          showModal(modalDialog(
            title = "Oops!!",
            'Maximum Measuring Range is empty!',
            easyClose = TRUE,
            footer = NULL
          ))
          return(NA)
        }
        
        nom_in <- c(10,20,50,100)
        depth_step <- seq(25,150,by=25)

          # insert_first_calpro <- paste0("INSERT INTO table_name
          #                               (serial_number, calibration_date, scnd_calibration_date, 
          #                               tool_name, relative_humidity, temp_env,
          #                               cal_sop, max_measurement_range, manufacturer,
          #                               tools_alocation, entry_frst_order)
          #                               VALUES ('",input$info_serial_number,"',
          #                                       '",input$cal_date,"', 
          #                                       '",scnd_calibration_date,"',
          #                                       '",input$info_part_of_cal,"',
          #                                       '",as.numeric(input$info_humidity),"', 
          #                                       '",as.numeric(input$info_temp),"',
          #                                       '",toupper(input$info_sop),"',
          #                                       '",input$max_measurement_range,"',
          #                                       '",input$info_manufacturer,"',
          #                                       '",input$tool_alocation,"',
          #                                       '",Sys.time(),"');")
          # dbExecute(con, insert_first_calpro)
  
        #depth_step <- seq(25,150,by=25)
        if(input$max_measurement_range == '300'){
          nom_out <- c(20,seq(50,300,by=50))
          empty_value <- c(NA,NA,NA,NA,NA,NA,NA)
        }else if(input$max_measurement_range == '150'){
          nom_out <- c(20,50,100,150)
          empty_value <- c(NA,NA,NA,NA)
        }else if(input$max_measurement_range == '200'){
          nom_out <- c(20,50,100,150,200)
          empty_value <- c(NA,NA,NA,NA,NA)
        }else{
          showModal(modalDialog(
            title = "Oops!!",
            'Maximum Measuring Range is empty!',
            easyClose = TRUE,
            footer = NULL
          ))
          return(NA)
        }
        
        nom_in <- c(10,20,50,100)
        depth_step <- seq(25,150,by=25)
 
        data_outside <- data.frame(
          Nominal = nom_out,
          Outside1 = as.numeric(empty_value),
          Outside2 = as.numeric(empty_value),
          Outside3 = as.numeric(empty_value)
        )
    
        output$outsidetable <- renderRHandsontable({
          show_table_handsontable(data_outside)
        })
        #Select data INSIDE untuk menampilkan pada rhandsontable
        data_inside <- data.frame(
          Nominal = nom_in,
          Inside1 = as.numeric(c(NA,NA,NA,NA)),
          Inside2 = as.numeric(c(NA,NA,NA,NA)),
          Inside3 = as.numeric(c(NA,NA,NA,NA))
        )
        
        output$dtinsdtable <- renderRHandsontable({
          show_table_handsontable(data_inside)
        })
        
        #Select data DEPTH untuk menampilkan pada rhandsontable
        data_depth <- data.frame(
          Nominal = depth_step,
          Depth1 = as.numeric(c(NA,NA,NA,NA,NA,NA)),
          Depth2 = as.numeric(c(NA,NA,NA,NA,NA,NA)),
          Depth3 = as.numeric(c(NA,NA,NA,NA,NA,NA))
        )
        
        #Select data STEP untuk menampilkan pada rhandsontable
        data_step <- data.frame(
          Nominal = depth_step,
          Step1 = as.numeric(c(NA,NA,NA,NA,NA,NA)),
          Step2 = as.numeric(c(NA,NA,NA,NA,NA,NA)),
          Step3 = as.numeric(c(NA,NA,NA,NA,NA,NA))
        )
        join_data <- data_depth %>% mutate(data_step)
      
        output$depthsteptable <- renderRHandsontable({
          show_table_handsontable(join_data)
        })
        
        shinyjs::hide("heightgauge_cal")
        shinyjs::show("caliper_cal")
        shinyjs::hide("digitalscales_cal")
        shinyjs::show("submit_btn")
    
    },
    error=function(cond) {
      showModal(modalDialog(
        title = "Oops!!",
        paste0(cond),
        easyClose = TRUE,
        footer = NULL
      ))
      return(NA)
    },
    warning=function(cond) {
      showModal(modalDialog(
        title = "Oops!!",
        paste0(cond),
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    })
  }
})