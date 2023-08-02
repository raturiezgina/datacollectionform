Cal_table <- tabItem(
  tabName = 'table_calibration',
  HTML('<center><img src="ctls.png" width="50px" ></center>'),
  column(width = 12,
         div("TABLE", style = "text-align: center; 
          background-color: #050203; color:White; font-size:100%"),
         tabBox(width = NULL,
                tabPanel(h5(strong("CERTIFICATE OF CALIBRATION")),
                         fluidRow(
                           column(3,
                                  div(id = "check_toolsid", tagList(
                                    h5(strong("Available ID Numbers!")),
                                    DTOutput("avai_toolsid")
                                  ))
                           ),
                           column(3,
                                  div(style = "display: inline-block;vertical-align:center;",
                                      textInput("tool_id_number", placeholder="ID Number", label = tagList(icon("id-badge"), "ID Number"))
                                  ),
                                  shinyjs::hidden(
                                    div(id = "panel_cal",
                                        uiOutput("inselect_calibration"),
                                    )
                                  ),
                                  div(style="display: inline-block;vertical-align:center;",
                                      actionButton("ShowCalResult","Show Details")
                                  )
                           ),
                           column(6,
                                  uiOutput("Accuracy_table")
                                  )
                         ),
                         column(12,
                                uiOutput("Details_table_calibration_cer")
                                )
                )
         )
  )
)