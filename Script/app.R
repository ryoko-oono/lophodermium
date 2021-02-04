#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#install.packages("here")
library(here)
#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr) 
library(rsconnect)
#rsconnect::deployApp('path/to/your/app')

# Define UI for application that draws a histogram
#lophodermium_species<- as.data.frame(read.csv("/Users/ryokooono/Dropbox/R_codes_datasets/LophodermiumPhylogeny/Lophodermium_species.csv", 
 #                                             check.names = F, header=TRUE, stringsAsFactors = F,
  #                                            strip.white=TRUE, na.strings=c("NA", "na",
   #                                                                          "nan", "inf", "", ".")))
lophodermium_species <- read.csv(here("Data", "Lophodermium_species2.csv"), stringsAsFactors = FALSE)




ui <- fluidPage(
  titlePanel("Lophodermium species identification"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("Apothecia_length_input",
                  "Apothecia length", min = 0, max = 2.5,
                  value = c(0,2.5), post = "mm"),
      sliderInput("Ascus_length_input",
                  "Ascus length", min = 0, max = 250,
                  value = c(0, 250), post = "um"),
      sliderInput("Ascus_width_input",
                  "Ascus width", min = 0, max = 45,
                  value = c(0, 45), post = "um"),
      sliderInput("Ascospore_length_input",
                  "Ascospore length", min = 0, max = 200,
                  value = c(0, 200), post = "um"),
      radioButtons("ZoneInput", "Zone line",
                   choices = c("Present", "Absent","Unknown"),
                   selected = "Unknown"),
      radioButtons("HostInput", "Host species",
                   choices = c("Pinus", "Strobus", "Unknown"),
                   selected = "Unknown"),
      radioButtons("PathogenicInput", "Pathogenic status",
                         choices = c("Pathogenic", "Endophytic"),
                         selected = "Endophytic"),
      radioButtons("LipInput", "Lips",
                         choices = c("Absent", "Present","Unknown"),
                         selected = "Unknown"),
      checkboxGroupInput("PositionInput", "Position",
                         choices = c("Subcuticular", "Partially subepidermal", "Subepidermal",
                                     "Partially subhypodermal","Subhypodermal"),
                         selected = c("Subcuticular", "Partially subepidermal", "Subepidermal",
                                      "Partially subhypodermal","Subhypodermal"))
    ),
    mainPanel(
      tableOutput("results"),
      br(), br(),
      plotOutput("coolplot")


    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #output$coolplot <- renderPlot({
   # filtered <-
  #    lophodermium_species %>%
  #    dplyr::filter(Pinus.Strobus == input$HostInput
  #    )
  #})
  
  Position <- function(x){
      a <- data.frame()
      b <- data.frame()
      c <- data.frame()
      d <- data.frame()
      e <- data.frame()
      f <- data.frame()
      g <- data.frame()
      h <- data.frame()
      i <- data.frame()
      
      if ("Subcuticular" %in% input$PositionInput){
        a <- x[which(x$position == "subcuticular"),]
      } 
      if ("Partially subepidermal" %in% input$PositionInput){
        b <- x[which(x$position=="partially subepidermal"),]
        c <- x[which(x$position=="partially subepidermal or partially subhypodermal"),]
      }
      if ("Subepidermal" %in% input$PositionInput){
        d <- x[which(x$position=="subepidermal"),]
        e <- x[which(x$position=="partially subhypodermal or subepidermal"),]
      }
      if ("Partially subhypodermal" %in% input$PositionInput){
        f <- x[which(x$position=="partially subhypodermal"),]
        g <- x[which(x$position=="partially subhypodermal or subepidermal"),]
        e <- x[which(x$position=="partially subepidermal or partially subhypodermal"),]
      } 
      if ("Subhypodermal" %in% input$PositionInput) {
      h <- x[which(x$position=="subhypodermal"),]
      i <- x[which(x$position=="partially subhypodermal or subhypodermal"),]
      } 
      if ("Unknown" %in% input$PositionInput){
        x
      }
      y <-rbind(a,b,c,d,e,f,g,h,i)
      return(y)
  }
  
  reactive_size <- reactive({
      
   lophodermium_species %>%
                dplyr::filter(Apothecia_length_max_mm >= input$Apothecia_length_input[1]) %>%
                dplyr::filter(Apothecia_length_min_mm <= input$Apothecia_length_input[2]) %>%
      
                dplyr::filter(input$Ascus_length_input[1] <= Ascus_length_max_um) %>%
                dplyr::filter(Ascus_length_min_um <= input$Ascus_length_input[2]) %>%
      
                dplyr::filter(input$Ascus_width_input[1] <= Ascus_width_max_um) %>%
                dplyr::filter(Ascus_width_min_um <= input$Ascus_width_input[2]) %>%
      
                dplyr::filter(input$Ascospore_length_input[1] <= Ascospore_length_max_um) %>%
                dplyr::filter(Ascospore_length_min_um <= input$Ascospore_length_input[2]) %>%
      
                {if (input$HostInput=="Pinus") dplyr::filter(., Pinus.Strobus=="Pinus") else .} %>%
                {if (input$HostInput=="Strobus") dplyr::filter(., Pinus.Strobus=="Strobus") else .} %>% 
    
                Position() %>% 
      
              #  {if (input$ZoneInput=="Present") dplyr::filter(., pathogenic=="present") else .} %>% 
    
                
                {if (input$LipInput=="Absent") dplyr::filter(., Lips=="absent") else .} %>%
                {if (input$LipInput=="Present") dplyr::filter(., Lips=="present") else .} %>%
                
                {if (input$PathogenicInput=="Pathogenic") dplyr::filter(., pathogenic=="pathogenic") else .} 
    
               
  })
    
    output$results <- renderTable({
      table<-reactive_size()
      table[,1:10]
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

