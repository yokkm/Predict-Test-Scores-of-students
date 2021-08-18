library(shinydashboard)
library(shiny)
library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)



myData <- read.csv('./test_scores.csv', sep = ",")
# myData <- read.csv('/Users/yokk/Desktop/test_scores.csv', sep = ",")
myData <- na.omit(myData)
y<-sapply(myData,as.numeric)
x<-sapply(myData,as.numeric)
corrdata <- cor(x)
nms <- colnames(myData)

ui <- dashboardPage(
  dashboardHeader(title = "Predictive Model")
  ,dashboardSidebar(
    sidebarMenu(menuItem("Overview", tabName = "Overview"))
    ,sidebarMenu(menuItem("Exploration", tabName = "Exploration"))
  )
  ,dashboardBody(
    tabItems(
    (tabItem(tabName = "Overview",
             fluidRow(align = "center",
                      infoBoxOutput("countrows"),tags$style("#countrows {width:max-content;}"),
                      infoBoxOutput("countcols"),tags$style("#countcols {width:max-content;}")),
             fluidRow(dataTableOutput('table'))
    ))
    ,(tabItem(tabName = "Exploration",
             fluidRow(align = "center",h2("Data Exploratory"))
             ,fluidRow(align = "center",h4('Data correlation - All'),plotlyOutput("plot"))
             ,fluidRow(checkboxGroupInput("show_vars", "select columns:",
                                          names(myData), selected = names(myData),inline = TRUE
                                          #,width = "10px"
                                          ))
             ,fluidRow(align = "center",h4('Data correlation - selected columns'),plotlyOutput('mytable1'))
             ,fluidRow(selectInput("show_vars2", "select columns:",
                                          names(myData), selected = TRUE
             ))
              ,fluidRow(align = "center",h4(textOutput("caption_onmytable2"),dataTableOutput('mytable2')))
             #,fluidRow(align = "center",h4(textOutput("caption_onmytable2"),plotlyOutput('mytable2')))
    ))
    )
  )
)
server <- function(input, output) 
{
  # -- Overview page -- #
  output$countrows <- renderInfoBox({
    infoBox(
      "Total Rows", paste0(nrow(myData)), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$countcols <- renderInfoBox({
    infoBox(
      "Total Columns", paste0(ncol(myData)), icon = icon("list"),
      color = "blue", fill = TRUE
    )
  })
  output$table <- DT::renderDataTable(
    DT::datatable(myData,options = list(pageLength = 5,searching = FALSE,scrollX = TRUE)))
  # -- Exploratory page -- # 
  output$plot <- renderPlotly({
    plot_ly(x = nms, y = nms, z = corrdata,
            key = corrdata, type = "heatmap", source = "heatplot") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  # output$mytable1 <- DT::renderDataTable({
  #   DT::datatable(myData[, input$show_vars, drop = FALSE])
  # })
  output$mytable1 <-renderPlotly({
    new_data <- myData[, input$show_vars, drop = FALSE]
    new_x<-sapply(new_data,as.numeric)
    new_corrdata <- cor(new_x)
    new_nms <- colnames(new_data)
    plot_ly(x = new_nms, y = new_nms, z = new_corrdata,
            key = new_corrdata, type = "heatmap", source = "heatplot") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  #################################################################
  formulaText <- reactive({
    paste('Number of student on ',input$show_vars2,' column.')
  })
  # Return the formula text for printing as a caption
  output$caption_onmytable2 <- renderText({formulaText()})
  #################################################################
  selected_df <-reactive({
    col_selected <- input$show_vars2
    # 2. Filter data
    new_df <- subset(myData, select=c(col_selected,'student_id'))
    # 3. Return result
    new_df
  })

  ### working showing the filtered col
  output$mytable2 <- DT::renderDataTable(
    DT::datatable(selected_df(),options = list(pageLength = 5,searching = FALSE,scrollX = TRUE)))

  

  
  
  

  
}
shinyApp(ui = ui, server = server)
