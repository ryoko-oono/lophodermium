library(shiny)

ui <- fluidPage("Fungal Endophytes",
      textInput(inputId = "ITS", label = "Paste the ITS sequence below."),
      fileInput(inputId = "ITS", label = "Or upload fasta file with ITS sequence."),
      
      plotOutput("Phylogeny")          
                )

server <- function(input, output) {
  
  as.fasta(input$ITS)
  output$Phylogeny <- renderPlot({
    title <- "100 random normal values"
    hist(rnorm(100, main = title))
    })
  
}

shinyApp(ui = ui, server = server)

#Input is the fasta sequence of ITS1, 5.8S, ITS2, LSU

#textInput(inputId = "ITS", label = "Paste the ITS sequence below")

#Output is location or most well-supported clade
