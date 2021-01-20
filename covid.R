library(shiny)

sourcedata <- read.csv("./data/RKI_COVID19-20201102.csv")

bl <- aggregate(sourcedata$AnzahlFall, list(Bundesland = sourcedata$Bundesland), sum)

laenderauswahl <- bl[1]

ui <- fluidPage(
  selectizeInput(
    inputId = "bl", 
    label = NULL,
    # placeholder is enabled when 1st choice is an empty string
    choices = c("Bundeslaender hier auswaehlen" = "", laenderauswahl), 
    multiple = TRUE
  ),
  plotlyOutput(outputId = "p")
  # Quelle: https://plotly-r.com/linking-views-with-shiny.html 17.1.2
)

server <- function(input, output, session, ...) {
  output$p <- renderPlotly({
    req(input$bl)
    if (identical(input$bl, "")) return(NULL)
    gefilterteDaten <- filter(bl, Bundesland %in% input$bl) # Quelle: https://plotly-r.com/linking-views-with-shiny.html 17.1.2
    print(gefilterteDaten)
    layout(plot_ly(gefilterteDaten,x=gefilterteDaten$Bundesland,y=gefilterteDaten$x), title="Covid Fallzahlen nach Bundesland",
           xaxis=list(title = "Bundesland"),yaxis=list(title = "Gesamtanzahl der Fälle"))
  })
}

shinyApp(ui, server)