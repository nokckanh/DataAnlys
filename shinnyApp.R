
ui =  fluidPage(titlePanel("Creating the tabs!"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput(
                      "myfile",
                      "Choose CSV File",
                      multiple = F,
                      accept = ".csv",
                      
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
                  
                  mainPanel(tabsetPanel(
                    tabPanel("Summary", tableOutput("contents")),
                    tabPanel("Boxplot", plotOutput("myplot"))
                  ))))

server = function(input, output) {
  library(ggplot2)
  library(gridExtra)
  library(arules)
  
  
  
  output$contents = renderTable({
    req(input$myfile)
    
    data = read.csv(input$myfile$datapath,
                    header = input$header)
    
    inspect(data[1:10])  
    
    
    if (input$choice == "head") {
      return(head(data))
    }
    else {
      return(data)
    }
  })
  
  output$myplot  = renderPlot({
    q1 <- ggplot(aes(x=sulphates, y=quality), data = db) +
      geom_jitter(alpha=2/3) +
      geom_smooth() +
      ggtitle("Sulphates vs Quality" )
    
    q2 <- ggplot(aes(x=sulphates, y=quality), data=subset(db, db$sulphates < 1)) +
      geom_jitter(alpha=2/3) +
      geom_smooth() +
      ggtitle("Sulphates vs Quality without outliers")
    
    
    grid.arrange(q1,q2, ncol=1)
    
  })
}
shinyApp(ui, server)