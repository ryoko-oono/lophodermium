#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("rgdal")
#install.packages("leaflet")
#install.packages("htmltools")
library(shiny)
library(ggmap)
library(rgdal)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(ggplot2)
#install.packages("DT")
#update.packages(ask = FALSE)
library(DT)
kmlPlugin <- htmlDependency("leaflet.esri", "1.0.3",
                             src = c(href = "https://cdn.jsdelivr.net/leaflet.esri/1.0.3/"),
                             script = "leaflet.filelayer.js"
)

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}


register_google(key = "AIzaSyBKa2bw_rlM30R0JkgiyWEkm7skvmC3Rdw")

#ogrListLayers(monticola)

lambertiana ='~/Dropbox/R_codes_datasets/Pine_kml_files/lambertiana.kml'
lambertiana.kml =readOGR(lambertiana,layer='Pinus lambertiana')
#lambertiana.trans <- spTransform(lambertiana.kml, CRS("+proj=longlat +datum=WGS84"))
#lambertiana.fort <- fortify(lambertiana.trans)


monticola ='~/Dropbox/R_codes_datasets/Pine_kml_files/monticola.kml'
monticola.kml =readOGR(monticola,layer='Pinus monticola')

#monticola.trans <- spTransform(monticola.kml, CRS("+proj=longlat +datum=WGS84"))
#monticola.fort <- fortify(monticola.trans)

#plot(monticola.kml)

lambertiana_points<- as.data.frame(read.csv("/Users/ryokooono/Dropbox/R_codes_datasets/LophodermiumGenome/Locations_lambertiana.csv", 
                                            check.names = F, header=TRUE, stringsAsFactors = T,
                                            strip.white=TRUE, na.strings=c("NA", "na",
                                                                           "nan", "inf", "", ".")))
monticola_points<- as.data.frame(read.csv("/Users/ryokooono/Dropbox/R_codes_datasets/LophodermiumGenome/Locations_monticola.csv", 
                                          check.names = F, header=TRUE, stringsAsFactors = T,
                                          strip.white=TRUE, na.strings=c("NA", "na",
                                                                         "nan", "inf", "", ".")))

lophodermium_species<- as.data.frame(read.csv("/Users/ryokooono/Dropbox/R_codes_datasets/LophodermiumPhylogeny/Lophodermium_species.csv", 
                                          check.names = F, header=TRUE, stringsAsFactors = T,
                                          strip.white=TRUE, na.strings=c("NA", "na",
                                                                         "nan", "inf", "", ".")))

# Define UI for application that draws a histogram
ui <- fluidPage(
                # Application title
                titlePanel(HTML("Open-access real-time phylogenetic analyses of pine-specific <em>Lophodermium</em>")),
                h2("Introduction"),
                "This is a working draft of a dynamic phylogeny and collection database for the pine-associated genus", 
                em("Lophodermium."), 
                "This cyber-platform integrates text, code, and data in the same document 
as a shiny web app with a webserver that is running R in the background on top of a specimen-based database 
to generate all formatted figures and analyses presented here. There are multiple advantages to this real-time 
monograph: i) it is perpetually updated with the curated database of new specimens and sequences deposited to XXX, or when there are updates to the current database (ie., specimen metadata change), R is launched on 
a computer server and all scripts are activated to re-generate new text, plots, and results; ii) all analyses are stream-lined
and reproducible for consistency and transparency of taxonomic positions and descriptions; and iii) all data are free to use, so other 
users can conduct new analyses or download the data for other purposes.
                We started conceptualizing this model in 2018, outlined in an NSF EAGER proposal with Co-PIs Katja Seltmann and Michelle O'Malley, 
                to build a comprehensive and real-time analytical platform for all the information on endophyte diversity. To begin, 
we focus on one fungal genus, but the goal is to develop a template for any study system. 
                This cyberplatform is the hub that synthesizes all the information and knowledge about a focal taxonomic group and is effectively 
                the interface to the database. Because knowledge is not static, it will keep growing and changing as we understand 
                better the biology of these endophytes. The execution of this idea in a shiny app was further inspired by 
                Felipe Zapata's electronic dynamic monograph of", em("Escallonia."), "Contributions, ideas, and suggestions are welcome, 
                please send me an email ryoko.oono@lifesci.ucsb.edu",
                
                h2("Lophodermium"),
                em("Lophodermium"), "Chevall. is a paraphyletic genus in Rhytismataceae (Rhytismatales, Leotiomycetes) 
                with more than 100 species described. They are all associated with either living or dead plants 
                as endophytes or saprophytes but can be found on a diverse range of hosts, from grasses to conifers 
                (e.g. Lantz et al. 2011). Currently, around 38 Lophodermium species, known to be endophytic with", 
                em("Pinus"),
                "(pine) species, comprise a group hypothesized to be monophyletic (Ortiz-García et al. 2003). 
                Despite the smaller number of species, the classification of pine-associated", 
                em("Lophodermium"), "is challenging 
                because the few, relatively simple morphological characters are difficult to interpret. Some morphological 
                characters, such as subcuticular vs. subepidermal ascocarps (Minter 1981), have converged, leading to 
                misidentifications and hindering the ability of mycologists to identify species without sequencing 
                (Minter 1981, Ortiz-García et al. 2003). These species form small ascomata (< 5 mm) on plant litter that 
                require multiple parallel cross-sectioning to reveal diagnostic morphological characters under the microscope 
                and culture descriptions are rare. Also, there is relatively poor sampling to identify any geographical or 
                ecological patterns that would help delineate different species, in addition to morphological characters. ",
                br(),
                h2("Geographic distribution"),
                em("Lophodermium"), "is distributed worldwide but most well-studied in regions with high densities of pine species. 
                Hence, the majority of work on",em("Lophodermium"), "come from the northern hemisphere, primarily North America, China, or Europe. However, they have 
                been found from pine plantations in the southern hemisphere, such as New Zealand. The following is a map of species ranges of pine species 
                (if available by kml) as well as known collection sites of",
                em("Lophodermium"),  "specimens deposited in GenBank or published in peer-reviewed journals. Colors of points match color of host species range. 
                Shape of point corresponds to", em("Lophodermium"), "species. Click on each point for links to GenBank page and other metadata.",
                p(), 
                
                fluidRow(
                  mainPanel( 
                  #this will create a space for us to display our map
                  leafletOutput(outputId = "mymap"), 
                  #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
                  absolutePanel(top = 60, left = 20, 
                                checkboxInput(inputId = "lambertiana", 
                                              "Pinus lambertiana", FALSE),
                                checkboxInput(inputId = "monticola", 
                                              "Pinus monticola", FALSE)
                                )
                          )
                        ), 
                
                p(),
                h2("Interactive morphological guide for species identification"),
                p("While dichotomous keys are commonly applied to narrow down species identity, not all traits are easily measured or recognizable by non-specialists.
              Here, we provide an interactive key with visual guides that does not have to follow the traditional dichotomous hierarchy in case people want to skip
                  over some traits. "),
                fluidRow(
                  column(4,
                         selectInput(inputId = "host",
                                     "Host subgenus or genus:",
                                     choice = c("All",
                                                unique(as.character(lophodermium_species$Host_subgenus))))
                        ),
                  column(4,
                         checkboxGroupInput(inputId = "ascocarp", 
                                     "Ascocarp position:",
                                     c("All", unique(as.character(lophodermium_species$Ascocarp_position))))
                        ),
                  column(4,
                         sliderInput(inputId = "Ascomata length",
                                     "Ascospore_length", min = 0, max = 100,
                                     value = c(25, 1200), post = "um")
                  ),
                  column(4,
                         sliderInput(inputId = "Ascospore_width",
                                     "Ascospore_width", min = 0, max = 100,
                                     value = c(25, 40), post = "um")
                        )
                  ),
                
# Create a new row for the table.
                dataTableOutput(outputId = "table"),
                h2("Phylogenetic history"),
                p("The internal transcribed spacer and large 16S rDNA subunit are the most commonly sequenced markers for Fungi and "),
                em("Lophodermium."), "We provide here the majority-rule consensus Bayesian phylogeny published in Salas Lizana & Oono (2017). ",
                # Sidebar with a slider input for number of bins 
                h2("Microbiome of groups of pine individuals"),

p("Analysis of microbial communities requires the interpretation of one or more high-dimensional abundance matrices and its relationship with other datasets, using a complex and emerging suite of methods from ecology, genetics, phylogenetics, multivariate statistics, visualization and testing. Filtering, custom curation and transformation of the abundance data are also required usually, but the precisely reproducible workflow from raw data to final analyses is often difficult or impossible to reproduce exactly. Ideally, published scientific analyses are completely reproducible in as easy a fashion as possible; and anything less represents an impediment to both progress and peer review - Peng, 2011, McMurdie & Holmes 2015"),
"Here, we take advantage of the Shiny-phyloseq app and present various analyses that can be performed interface curation of OTU tables after selecting for host samples of interest by the user."

                  
)
                


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  #create the map
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- mpg
    if (input$man != "All") {
      data <- data[data$manufacturer == input$man,]
    }
    if (input$cyl != "All") {
      data <- data[data$cyl == input$cyl,]
    }
    if (input$trans != "All") {
      data <- data[data$trans == input$trans,]
    }
    data
  }))
  
#   output$mymap <- renderLeaflet({
 #    leaflet(lambertiana_points) %>% 
#       setView(lng = -129, lat = 45, zoom = 3)  %>% #setting the view over ~ center of North America
#       addTiles() %>% 
#       addCircles(data = lambertiana_points, lat = ~ Lat, lng = ~ Long, 
#                  weight = 1, radius = ~sqrt(Fiss_S)*25000, popup = ~as.character(Fiss_S), 
#                  label = ~as.character(paste0("Magnitude: ", sep = " ", Fiss_S)), 
#                  #color = ~pal(mag), 
#                  fillOpacity = 0.5)
#   })
  
     
     # Filter data based on selections
   #  output$table <- renderTable({
  #     data <- lophodermium_species
  #     if (input$host != "All") {
  #       data <- data[data$Host_subgenus == input$host,]
  #     }
     #  if (input$ascocarp != "All") {
      #   data <- data[data$Ascocarp_position == input$ascocarp,]
       #}
       #if (input$Ascocarp_width != 0) {
      #   data <- data[data$Ascospore_width == input$Ascocarp_width,]
      # }
  #     data
  #   })
     
   
   
 #  observe({
  #   proxy <- leafletProxy("mymap", data = lambertiana.kml)
  #   proxy1 <- leafletProxy("mymap", data = monticola.kml)
     
   #  if(input$lambertiana) {
  #     proxy %>% 
  #       registerPlugin(kmlPlugin) %>%
  #       addPolygons(data = lambertiana.kml, # CHANGE cases by variableplot
  #                   color = "green",
  #                   dashArray = "3",
  #                   fillOpacity = 0.5) 
  #   }
  #   
  #   if (input$monticola) {
  #     proxy1 %>% 
  #       registerPlugin(kmlPlugin) %>%
  #       addPolygons(data = monticola.kml, # CHANGE cases by variableplot
  #                   color = "blue",
  #                   dashArray = "3",
  #                   fillOpacity = 0.5)                           
  #   }
  #   else {
  #     proxy %>% clearShapes() %>% clearControls()
  #   }
  #  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

