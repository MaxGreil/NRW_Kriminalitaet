#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)

data <- read.csv("CrimeStatistic.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("NRW Kriminalitaet"),
    
    sidebarLayout(
        position = "left",
        sidebarPanel(
            selectInput(
                inputId = "selectCrime",
                label = h3("Verbrechen"),
                choices = c(
                    "Einbruchdelikte",
                    "Wohnungseinbruchdiebstahl",
                    "Tageswohnungs-einbruchdiebstahl",
                    "Delikte der Straßenkriminalität",
                    "Vergewaltigung/sexuelle Nötigung/sexueller Übergriff (besonders schwere Fälle einschl. Todesfolge)",
                    "Raubüberfälle auf Straßen, Wegen oder Plätzen",
                    "Gefährliche und schwere Körperverletzung auf Straßen, Wegen oder Plätzen",
                    "Taschendiebstahl",
                    "Diebstahl von Fahrrädern",
                    "Gewaltkriminalität"
                ),
                selected = "Einbruchdelikte"
            ),
            selectInput(
                inputId = "selectYear",
                label = h3("Jahr"),
                choices = c(
                    "Oktober 2019",
                    "September 2019",
                    "August 2019",
                    "Juli 2019",
                    "Juni 2019",
                    "Mai 2019",
                    "April 2019",
                    "März 2019",
                    "Februar 2019",
                    "Dezember 2018",
                    "November 2018",
                    "Oktober 2018",
                    "Oktober 2014"
                ),
                selected = "Oktober 2019"
            )
        ),
        mainPanel(
            leafletOutput(outputId = "mymap"),
            verbatimTextOutput("summary")
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    datasetInput <- reactive({
        dplyr::filter(data,
                      Delikt == input$selectCrime & Monat == input$selectYear)
    })
    
    output$summary <- renderPrint({
        dataset <- datasetInput()
        dataset <- dplyr::select(dataset, bekanntgewordeneFaelle, davonVersuche, Aufklaerungsquote)
        summary(dataset)
    })
    
    #Create map
    output$mymap <- renderLeaflet({
        dataset <- datasetInput()
        leaflet(dataset) %>%
            setView(lng = 7.661594,
                    lat = 51.433237,
                    zoom = 7)  %>%
            addTiles() %>%
            addCircles(
                data = dataset,
                lat = ~ latitude,
                lng = ~ longitude,
                weight = 1,
                radius = 1500,
                color = 'blue',
                label = ~as.character(paste0("Bekanntgewordene Faelle: ", bekanntgewordeneFaelle, ", davon Versuche: ", davonVersuche, ", Aufklaerungsquote: ", Aufklaerungsquote, "%", sep = " "))
            )
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
