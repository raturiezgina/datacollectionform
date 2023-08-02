CoCTableData <- reactiveValues()

#=====================================================Certificate of Calibration
observe({
  tryCatch({
    
    shinyjs::show("check_toolsid")
    output$avai_toolsid <- DT::renderDT({
      # show_id_tools <- dbGetQuery(con,paste0('SELECT DISTINCT(serial_number) AS "ID Number"
      #                                        FROM tablename ORDER BY serial_number ASC;'))
      show_id_tools <- data.frame(
        ID_Number = c("99-A",
                      "A1-002",
                      "A1-073",
                      "A2-002",
                      "A2-003")
      )
      datatable(show_id_tools,
                escape = F, 
                options = list(pageLength = 5, autoWidth = TRUE,
                               columnDefs = list(list(className = 'dt-left', width = '180px', targets = 1))))
    }, server = FALSE)
    
    if(isTruthy(input$tool_id_number)){
      # is_data_exists <- sqlInterpolate(con,"SELECT EXISTS(SELECT 1 FROM 
      #                                         tablename
      #                                         WHERE serial_number = ?sn);",
      #                                  sn = toupper(input$tool_id_number)
      # )
      # is_data_exists <- dbGetQuery(con, is_data_exists)
      # 
      # if(is_data_exists == TRUE){
      #   select_existing_data <- sqlInterpolate(con,"SELECT calibration_date
      #                                                 FROM tablename
      #                                                 WHERE serial_number = ?sn;",
      #                                          sn = toupper(input$tool_id_number))
      #   select_existing_data <- dbGetQuery(con, select_existing_data)
      #   
      #   # Can also set the label and select items
      #   output$inselect_calibration <- renderUI({
      #     selectInput("cal_date_history", label = tagList(icon("shapes"), "Calibration Date"),choices = c(select_existing_data$calibration_date), selected = "")
      #   })
      #   shinyjs::show("panel_cal")
      # }
    }
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
  
})
observeEvent(input$ShowCalResult,{
  tryCatch({
    shinyjs::show("caliper table")
    shinyjs::show("accuracy caliper table")
    shinyjs::show("cocal_button")
    
    if(!isTruthy(input$tool_id_number) | !isTruthy(input$cal_date_history)){
      showModal(modalDialog(
        title = "Heyy!!",
        paste0("These field can't be left empty"),
        easyClose = TRUE,
        footer = NULL
      ))
      return(NA)
    }else{
      #Select accuracy, deviation and u95    outside, inside, depth, step
      # data_accuracy <- sqlInterpolate(con,"SELECT u95, accuracy, deviation
      #                                        FROM tablename
      #                                        WHERE serial_number = ?sn AND entry = ?entry;",
      #                                 sn = toupper(input$tool_id_number), 
      #                                 entry = input$cal_date_history
      # )
      # data_accuracy <- dbGetQuery(con, data_accuracy)
      
      if(nrow(data_accuracy) == 0){
        showModal(modalDialog(
          title = "Sorry!!",
          paste0("Serial Number ",input$tool_id_number," didn't exists! "),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NA)
      }
      # }else{
      #   first_column <- list("Outside,Inside,Depth,Step")
      #   fc <- (str_split_fixed(first_column, ",",4))
      #   unc <- gsub("\\{|\\}", "", (str_split_fixed(data_accuracy$u95, ",",4)))
      #   acc <- gsub("\\{|\\}", "", (str_split_fixed(data_accuracy$accuracy, ",",4)))
      #   devi <- gsub("\\{|\\}", "", (str_split_fixed(data_accuracy$deviation, ",",4)))
      #   combine <- cbind(fc,unc,acc,devi)
      #   
      #   mat_result <- matrix(combine, nrow = 4, ncol = 4)
      #   
      #   output$Preview_summary <- renderReactable({
      #     reactable(mat_result,
      #               striped = TRUE, showPageSizeOptions = TRUE,
      #               highlight = TRUE, resizable = TRUE,
      #               bordered = TRUE, wrap = FALSE,
      #               columns = list(V1 = colDef(name = "Measurement",width = 100), 
      #                              V2 = colDef(name = "U95"),
      #                              V3 = colDef(name = "Accuracy"), 
      #                              V4 = colDef(name = "Deviation")
      #               ),
      #               defaultColDef = colDef(minWidth = 90, align = "center"))
      #   })
      #   
      #   coc_table_head_tool <- sqlInterpolate(con,"SELECT * FROM table_name
      #                                           WHERE serial_number = ?sn AND calibration_date = ?date;",
      #                                         sn = toupper(input$tool_id_number),
      #                                         date = input$cal_date_history
      #   )
      #   coc_table_head_tool <- dbGetQuery(con, coc_table_head_tool)
      #   current_date <- as.character(coc_table_head_tool$calibration_date)
      #   next_cal <- as.character(coc_table_head_tool$scnd_calibration_date)
      #   temp_env <- paste(coc_table_head_tool$temp_env, "C")
      #   humidity <- paste(coc_table_head_tool$relative_humidity, "%")
      #  
      #   coc_table_head_tool_df <- data.frame(text_var = c("Tool Name", "Measuring Range", "Resolution of measurement", "Manufacturer", "ID Number"),
      #                                        colon = c(":",":",":",":",":"),
      #                                        text_val = c(coc_table_head_tool$tool_name,
      #                                                     paste("0 -",coc_table_head_tool$max_measurement_range,"mm"), 
      #                                                     '0.01 mm', #3
      #                                                     coc_table_head_tool$manufacturer,
      #                                                     coc_table_head_tool$serial_number),
      #                                        text_var1 = c("Calibration Date", "Next Calibration Date", "Calibration Temperature", "Relative Humidity", "Calibration Procedure"),
      #                                        colon1 = c(":",":",":",":",":"),
      #                                        text_val1 = c(current_date, next_cal, temp_env, humidity, coc_table_head_tool$cal_sop)
      #   )
      #   CoCTableData$df <- coc_table_head_tool_df
      #   data_inside <- sqlInterpolate(con,"SELECT nom_in, d_inside1, d_inside2, d_inside3,
      #                                   mean, st_dev, error, u_measuring,
      #                                   ug_u_tool, ur_resolution, ugab_combined,
      #                                   u_effective FROM table_name
      #                                   WHERE serial_number = ?sn AND entry = ?entry;",
      #                                 sn = toupper(input$tool_id_number),
      #                                 entry = input$cal_date_history
      #   )
      #   data_inside <- dbGetQuery(con, data_inside)
      #   
      #   CoCTableData$inside <- data_inside
      #   CoCTableData$inside <-  rename(data_inside,
      #                                  "Nominal" = nom_in, "VoM 1" = d_inside1,
      #                                  "VoM 2" = d_inside2, "VoM 3" = d_inside3,
      #                                  "Mean" = mean, "S" = st_dev,
      #                                  "Error" = error, "Um" =  u_measuring,
      #                                  "Ug" = ug_u_tool,  "Ur" = ur_resolution,
      #                                  "Ugab" =  ugab_combined, "Ueff" =  u_effective)
      #   data_inside <- sqlInterpolate(con,"SELECT nom_in, d_inside1, d_inside2, d_inside3,
      #                                 mean, st_dev, error, u_measuring, ug_u_tool,
      #                                 ur_resolution, ugab_combined, u_effective FROM table_name
      #                                 WHERE serial_number = ?snin AND entry = ?entry;",
      #                                 snin = toupper(input$tool_id_number),
      #                                 entry = input$cal_date_history
      #   )
      #   data_inside <- dbGetQuery(con, data_inside)
      #   
      #   output$Preview_inside <- renderReactable({
      #     reactable(data_inside,
      #               striped = TRUE, showPageSizeOptions = TRUE,
      #               highlight = TRUE, resizable = TRUE,
      #               bordered = TRUE, wrap = FALSE,
      #               columns = list(
      #                 nom_in = colDef(name = "Nominal"),
      #                 d_inside1 = colDef(name = "VoM 1"),
      #                 d_inside2 = colDef(name = "VoM 2"),
      #                 d_inside3 =  colDef(name = "VoM 3"),
      #                 mean = colDef(name = "Mean", width = 70),
      #                 st_dev = colDef(name = "S"), error = colDef(name = "Error"),
      #                 u_measuring = colDef(name = "Um"),
      #                 ug_u_tool = colDef(name = "Ug", width = 70), ur_resolution = colDef(name = "Ur"),
      #                 ugab_combined = colDef(name = "Ugab"), u_effective = colDef(name = "U")
      #               ),
      #               defaultColDef = colDef(minWidth = 40, align = "center")
      #     )
      #   })
      #   
      #   data_outside <- sqlInterpolate(con,"SELECT nom_out, d_outside1, d_outside2,
      #                                      d_outside3, mean, st_dev, error, u_measuring,
      #                                      ug_u_tool, ur_resolution, ugab_combined,
      #                                      u_effective FROM table_name
      #                                      WHERE serial_number = ?snout AND entry = ?entry;",
      #                                  snout = toupper(input$tool_id_number),
      #                                  entry = input$cal_date_history)
      #   
      #   data_outside <- dbGetQuery(con, data_outside)
      #   output$Preview_outside <- renderReactable({
      #     reactable(data_outside,
      #               striped = TRUE, showPageSizeOptions = TRUE,
      #               highlight = TRUE, resizable = TRUE,
      #               bordered = TRUE, wrap = FALSE,
      #               columns = list(
      #                 nom_out = colDef(name = "Nominal"), d_outside1 = colDef(name = "VoM 1"),
      #                 d_outside2 = colDef(name = "VoM 2"), d_outside3 =  colDef(name = "VoM 3"),
      #                 mean = colDef(name = "Mean", width = 70), st_dev = colDef(name = "S"), error = colDef(name = "Error"),
      #                 u_measuring = colDef(name = "Um"), ug_u_tool = colDef(name = "Ug", width = 70), ur_resolution = colDef(name = "Ur"),
      #                 ugab_combined = colDef(name = "Ugab"), u_effective = colDef(name = "U")
      #               ),
      #               defaultColDef = colDef(minWidth = 40, align = "center")
      #     )
      #   })
      #   data_depth <- sqlInterpolate(con,"SELECT nom_depth, d_depth1, d_depth2,
      #                                    d_depth3, mean, st_dev, error, u_measuring,
      #                                    ug_u_tool, ur_resolution, ugab_combined,
      #                                    u_effective FROM table_name
      #                                    WHERE serial_number = ?sn AND entry = ?entry;",
      #                                sn = toupper(input$tool_id_number),
      #                                entry = input$cal_date_history)
      #   data_depth <- dbGetQuery(con, data_depth)
      #   output$Preview_Depth <- renderReactable({
      #     reactable(data_depth,
      #               striped = TRUE, showPageSizeOptions = TRUE,
      #               highlight = TRUE, resizable = TRUE,
      #               bordered = TRUE, wrap = FALSE,
      #               columns = list(
      #                 nom_depth = colDef(name = "Nominal"),
      #                 d_depth1 = colDef(name = "VoM 1"),
      #                 d_depth2 = colDef(name = "VoM 2"),
      #                 d_depth3 =  colDef(name = "VoM 3"),
      #                 mean = colDef(name = "Mean", width = 70),
      #                 st_dev = colDef(name = "S"), error = colDef(name = "Error"),
      #                 u_measuring = colDef(name = "Um"),
      #                 ug_u_tool = colDef(name = "Ug", width = 70), ur_resolution = colDef(name = "Ur"),
      #                 ugab_combined = colDef(name = "Ugab"), u_effective = colDef(name = "U")
      #               ),
      #               defaultColDef = colDef(minWidth = 40, align = "center")
      #     )
      #   })
      #   
      #   data_step <- sqlInterpolate(con,"SELECT nom_step, d_step1, d_step2, d_step3,
      #                                   mean, st_dev, error, u_measuring,
      #                                   ug_u_tool, ur_resolution, ugab_combined,
      #                                   u_effective FROM table_name
      #                                   WHERE serial_number = ?sn AND entry = ?entry;",
      #                               sn = toupper(input$tool_id_number),
      #                               entry = input$cal_date_history)
      #   data_step <- dbGetQuery(con, data_step)
      #   
      #   output$Preview_Step <- renderReactable({
      #     reactable(data_step,
      #               striped = TRUE,
      #               showPageSizeOptions = TRUE,
      #               highlight = TRUE,
      #               resizable = TRUE,
      #               bordered = TRUE,
      #               wrap = FALSE,
      #               columns = list(
      #                 nom_step = colDef(name = "Nominal"),
      #                 d_step1 = colDef(name = "VoM 1"),
      #                 d_step2 = colDef(name = "VoM 2"),
      #                 d_step3 =  colDef(name = "VoM 3"),
      #                 mean = colDef(name = "Mean", width = 70),
      #                 st_dev = colDef(name = "S"), error = colDef(name = "Error"),
      #                 u_measuring = colDef(name = "Um"),
      #                 ug_u_tool = colDef(name = "Ug", width = 70), ur_resolution = colDef(name = "Ur"),
      #                 ugab_combined = colDef(name = "Ugab"), u_effective = colDef(name = "U")
      #               ),
      #               defaultColDef = colDef(minWidth = 40, align = "center")
      #     )
      #   })
      #   
      #   shinyjs::show("caliper table")
      #   shinyjs::show("accuracy caliper table")
      #   
      #   data_outside <- sqlInterpolate(con,"SELECT nom_out, d_outside1, d_outside2,
      #                                    d_outside3, mean, st_dev, error, u_measuring,
      #                                    ug_u_tool, ur_resolution, ugab_combined,
      #                                    u_effective FROM table_name
      #                                   WHERE serial_number = ?sn AND entry = ?entry;",
      #                                  sn = toupper(input$tool_id_number),
      #                                  entry = input$cal_date_history
      #   )
      #   
      #   data_outside <- dbGetQuery(con, data_outside)
      #   CoCTableData$outside <- data_outside
      #   CoCTableData$outside <-  rename(data_outside,
      #                                   "Nominal" = nom_out, "VoM 1" = d_outside1,
      #                                   "VoM 2" = d_outside2, "VoM 3" = d_outside3,
      #                                   "Mean" = mean, "S" = st_dev, "Error" = error,
      #                                   "Um" =  u_measuring,
      #                                   "Ug" = ug_u_tool,  "Ur" = ur_resolution,
      #                                   "Ugab" =  ugab_combined, "Ueff" =  u_effective)
      #   
      #   data_depth <- sqlInterpolate(con,"SELECT nom_depth, d_depth1, d_depth2,
      #                                    d_depth3, mean, st_dev, error, u_measuring,
      #                                    ug_u_tool, ur_resolution, ugab_combined,
      #                                    u_effective FROM table_name
      #                                   WHERE serial_number = ?sn AND entry = ?entry;",
      #                                sn = toupper(input$tool_id_number),
      #                                entry = input$cal_date_history
      #   )
      #   data_depth <- dbGetQuery(con, data_depth)
      #   
      #   CoCTableData$depth <- data_depth
      #   CoCTableData$depth <-  rename(data_depth,
      #                                 "Nominal" = nom_depth, "VoM 1" = d_depth1,
      #                                 "VoM 2" = d_depth2, "VoM 3" = d_depth3,
      #                                 "Mean" = mean, "S" = st_dev,
      #                                 "Error" = error, "Um" =  u_measuring,
      #                                 "Ug" = ug_u_tool,  "Ur" = ur_resolution,
      #                                 "Ugab" =  ugab_combined, "Ueff" =  u_effective)
      #   
      #   
      #   data_step <- sqlInterpolate(con,"SELECT nom_step, d_step1, d_step2,
      #                                d_step3, mean, st_dev, error, u_measuring,
      #                                ug_u_tool, ur_resolution, ugab_combined,
      #                                u_effective FROM table_name
      #                                WHERE serial_number = ?sn AND entry = ?entry;",
      #                               sn = toupper(input$tool_id_number),
      #                               entry = input$cal_date_history
      #   )
      #   data_step <- dbGetQuery(con, data_step)
      #   CoCTableData$step <- data_step
      #   CoCTableData$step <-  rename(data_step,
      #                                "Nominal" = nom_step, "VoM 1" = d_step1,
      #                                "VoM 2" = d_step2, "VoM 3" = d_step3,
      #                                "Mean" = mean, "S" = st_dev,
      #                                "Error" = error, "Um" =  u_measuring,
      #                                "Ug" = ug_u_tool,  "Ur" = ur_resolution,
      #                                "Ugab" =  ugab_combined, "Ueff" =  u_effective)
      #   
      #   
      #   first_column <- list("Outside,Inside,Depth,Step")
      #   fc <- (str_split_fixed(first_column, ",",4))
      #   unc <- gsub("\\{|\\}", "", (str_split_fixed(data_accuracy$u95, ",",4)))
      #   acc <- gsub("\\{|\\}", "", (str_split_fixed(data_accuracy$accuracy, ",",4)))
      #   devi <- gsub("\\{|\\}", "", (str_split_fixed(data_accuracy$deviation, ",",4)))
      #   combine <- cbind(fc,unc,acc,devi)
      #   
      #   mat_result <- matrix(combine, nrow = 4, ncol = 4)
      #   CoCTableData$result_final  <- mat_result
      #   CoCTableData$result_final <- rename(as.data.frame(mat_result),
      #                                       "Measurement" = V1,
      #                                       "U95" = V2, "Accuracy" = V3, "Deviation" = V4)
      #   
      # }
    }
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
})

output$Accuracy_table <- renderUI({
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(
      div(id = "accuracy caliper table",
          # div("Calibration Result", style = "text-align: center;
          #     background-color: #4B4A48; color:White; font-size:100%"),
          # tags$br(),
          column(12,
                 h4(strong("Tools Accuracy")),
                 reactableOutput("Preview_summary",width = "100%")
          )
      )),
    tags$br()
  )
})
#Show action button to download COA
output$Details_table_calibration_cer <- renderUI({
  tagList(
    shinyjs::useShinyjs(),
    shinyjs::hidden(
      div(id = "caliper table",
          column(6,
                 h4(strong("Inside Measurement")),
                 reactableOutput("Preview_inside",width = "100%")
          ),
          column(6,
                 h4(strong("Outside Measurement")),
                 reactableOutput("Preview_outside",width = "100%")
          ),
          column(6,
                 h4(strong("Depth Measurement")),
                 reactableOutput("Preview_Depth",width = "100%")
          ),
          column(6,
                 h4(strong("Step Measurement")),
                 reactableOutput("Preview_Step",width = "100%")
          ),
          tags$br()
      )),
    fluidRow(
      tags$br(),
      shinyjs::hidden(
        div(id = "cocal_button",
            tags$br(),
            column(6,
                   downloadButton('DownloadCoc','Download CoC')
            )
        )
      )
    )
  )
})


# # ------ downloadhandler CoC -----------
output$DownloadCoc <- downloadHandler(
  filename = function() {
    paste('CertificateofCalibration.pdf')
  },
  content = function(file) {
    src <- normalizePath('CertificateofCalibration.Rmd')
    
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'CertificateofCalibration.Rmd', overwrite = TRUE)
    
    library(rmarkdown)
    out <- render('CertificateofCalibration.Rmd', pdf_document())#
    file.rename(out, file)
  }
)
