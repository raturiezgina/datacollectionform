Cal_entry <- tabItem(tabName = 'calibration_entry',
                     HTML('<center><img src="ctls.png" width="50px" ></center>'),
                     column(width = 12,
                           div("Quality is never an accident, it is always the result of intelligent effort.JR",
                                style =  "text-align: center;  background-color: #050203; color: White; font:raleway; font-size: 100%"),
                       tabBox(width = NULL,
                              tabPanel(div(h5(strong("CALIBRATION")),
                                           style = "color:black"),
                                       fluidRow(
                                         column(4,
                                                tags$div(id = 'cal_entry_data',
                                                         style = "width: 300px;  max-width: 100%;
                                                                 margin: 0 auto; padding: 20px;",
                                                         # set isi dari panel, set warna background dengan style CSS
                                                         wellPanel(style = 'background: #ffff; border-radius: 12px;',
                                                                   tagList(useShinyjs(),
                                                                           selectInput("tool_alocation", label = tagList(icon("user-clock"), "User"),
                                                                                              choices = c("","Injection","Machining","QA")),
                                                                           selectInput("info_part_of_cal", label = tagList(icon("hive"), "Measurement Tools"),
                                                                                              choices = c("","Digital Caliper"), selected = "Digital Caliper"),
                                                                           # shinyjs::hidden(
                                                                           #          div(id = "max_range",style = "text-align: center;",
                                                                                        selectInput("max_measurement_range", label = tagList(icon("hive"), "Measurement Range"),
                                                                                                    choices = c("","150","200","300")),
                                                                                        dateInput('cal_date',label = tagList(icon("calendar-check"), "Calibration Date")),
                                                                                        textInput("info_serial_number", placeholder="ID", label = tagList("ID Number")),
                                                                                        textInput("info_manufacturer", placeholder="Manufacturer", label = tagList("Manufacturer")),
                                                                                        numericInput("info_temp", label = tagList(icon("step-forward"), "Temperatur (Celcius)"),value = NULL),
                                                                                        numericInput("info_humidity", label = tagList(icon("step-forward"), "Relative Humidity (%)"),value = NULL),
                                                                                        textInput("info_sop", placeholder="Cal Procedure", label = tagList("Calibration Procedure")),
                                                                                        actionButton("create_table","Create Table")
                                                                            # ))
                                                                   )
                                                                   )
                                                         )
                                                ),
                                         column(8,
                                                shinyjs::hidden(
                                                  div(id = "heightgauge_cal", style = "text-align: center;",
                                                      column(6, 
                                                             rHandsontableOutput('hgtable')
                                                      ),
                                                  )),
                                                shinyjs::hidden(
                                                  div(id = "caliper_cal",style = "text-align: center;",
                                                      column(8, 
                                                             rHandsontableOutput('dtinsdtable'),
                                                             tags$br(),
                                                             rHandsontableOutput('outsidetable'),
                                                             tags$br(),
                                                             rHandsontableOutput('depthsteptable'),
                                                             tags$br()
                                                      ),
                                                  )),
                                                shinyjs::hidden(
                                                  div(id = "digitalscales_cal",style = "text-align: center;",
                                                      column(8,
                                                             rHandsontableOutput('repeatability'),
                                                             rHandsontableOutput('departure_from_nom_value')
                                                      ),
                                                  )),
                                                ),
                                         column(4,
                                                style = 'background: #fffff;
                                                         border-radius: 12px;',
                                                tagList(
                                                  fluidRow(
                                                    # action button dengan styling CSS
                                                    shinyjs::hidden(
                                                      tags$br(),
                                                      div(id = "submit_btn",style = "text-align: center;",
                                                          actionButton("submit_calibration_data", "Submit",
                                                                       style = "color: white; background-color: #4B4A48;
                                                                                padding: 10px 20px; width: 50%;
                                                                                cursor: pointer; border-radius: 6px;
                                                                                font-family:san fransisco, sans-serif;
                                                                                font-size:15px; font-weight: 400; ",
                                                                       icon = icon('paper-plane'))
                                                      )))
                                                ))
                                       )
                              ))
                     )#tabBox
)
