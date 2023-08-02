# ===================================================== Library app ============================================================
library(shiny)
library(stringr)
library(reactable)
library(DBI)
library(shinyalert)
library(rhandsontable)
library(dplyr)
library(lubridate)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)  
library(knitr)
library(kableExtra)
library(RPostgres)
library (RODBC)
library(DT)
library (odbc)
library(pool)

# mypw<- 'fillpassword'
# myPort <- 'fillport'
# myDB <- 'filldatabasename'
# myUser <- 'fillusername'
# 
# con <- dbConnect(RPostgres::Postgres()
#                  , host ='fillIPHost'
#                  , port = myPort
#                  , dbname = myDB
#                  , user = myUser
#                  , password = mypw
#                  , timezone = "Asia/Jakarta")
source("./global.R")

# ====================================================== coding untuk ui ======================================================
ui <- dashboardPage(skin = 'black', title = "CALIBRATION",
                    dashboardHeader(
                      title = tags$img(src='logo.png',
                                       height = 20,
                                       width = 200),
                      titleWidth = 250,
                      uiOutput("logoutbtn")),
                    
                    dashboardSidebar(collapsed = TRUE, 
                                     width = 250,
                                     uiOutput('sidebarpanel')
                    ),
                    dashboardBody(
                      tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
                      useShinyjs(),
                      setBackgroundImage(
                        src = "background.jpg",
                        shinydashboard = TRUE
                      ),
                      tags$head(tags$style(
                        
                        HTML('
                    .main-header .logo {
                    font-family: san fransisco, sans-serif ;
                    font-weight: bold;
                    font-size: 15px; }
                    
                    .content-wrapper {overflow: auto;}
                    .shiny-notification {
                                    position:fixed;
                                    top: calc(75%);
                                    left: calc(50%);
                                    width: 300px;
                                    font-size: 17px;
                                    margin-left: auto;
                                    margin-right: auto;}
                    
                    /* body */                      
                    .content-wrapper, .right-side { background-color: #FFFFFF; }
                    
                    /* main sidebar */
                    .skin-black .main-sidebar { background-color: #1E2020; }
                    
                    /* navbar (rest of the header currently disabled)
                    .skin-black .main-header .navbar { background-color: #F79040; } */'
                        ))),
                      shinyjs::useShinyjs(),
                      uiOutput('body')
                    )      
                    
)

# ====================== coding untuk server ==============================
server <- function(input, output, session) {
 
  # =========== Start of render UI dashboard sidebar ===================
  
  output$sidebarpanel <- renderUI({
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      
          sidebarMenu(id = 'sidebar', style = "position: relative; overflow: visible;",
                      menuItem("CALIBRATION TOOLS", tabName = 'calibration', icon = icon('industry'),
                               menuSubItem("CALIBRATION", tabName = 'calibration_entry', icon = icon('inbox')),
                               menuSubItem("TABLE", tabName = 'table_calibration', icon = icon('table'))
                      ),
                      useShinyjs()
          )
  })
  
  source("UI_Calibration_Side.R",local=TRUE)
  source("UI_Table_Calibration.R",local=TRUE)
  
  output$body <- renderUI({
      tabItems(
        Cal_entry,
        Cal_table
      )
  })
  
  source("Server_Cal_New.R",local=TRUE)
  source("Server_TableCal.R",local=TRUE)
}

shinyApp(ui = ui, server = server) 