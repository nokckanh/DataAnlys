library(shiny)
ui = fluidPage(titlePanel("Uploading file in Shiny"),
               sidebarLayout(
                 sidebarPanel(
                   fileInput(
                     "myfile",
                     "Choose CSV File",
                     multiple = F,
                     accept = ".csv"
                   ),
                   
                   checkboxInput("header", "Header", TRUE),
                   
                   radioButtons(
                     "choice",
                     "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head"
                   )
                 ),
                 
                 mainPanel(tableOutput("contents"))
                 
               ))
server = function(input, output) {
  output$contents = renderTable({
    req(input$myfile)
    
    data = read.csv(input$myfile$datapath,
                    header = input$header)
    
    if (input$choice == "head") {
      return(head(data))
    }
    else {
      return(data)
    }
    
  })
}
shinyApp(ui, server)