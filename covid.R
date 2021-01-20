library(shiny)

sourcedata <- read.csv("./data/RKI_COVID19-20201102.csv", stringsAsFactors = FALSE)
# https://stackoverflow.com/questions/51867390/r-programming-filtering-data-frame-with-date-column

laenderdaten <- aggregate(sourcedata$AnzahlFall, list(Bundesland = sourcedata$Bundesland), sum)

laenderauswahl <- laenderdaten[1]

ui <- fluidPage(
  # Quelle: https://plotly-r.com/linking-views-with-shiny.html 17.1.2
  selectizeInput(
    inputId = "bl", 
    label = NULL,
    # placeholder is enabled when 1st choice is an empty string
    choices = c("Bundeslaender hier auswaehlen" = "", laenderauswahl), 
    multiple = TRUE
  ),
  # https://stackoverflow.com/questions/40908808/how-to-sliderinput-for-dates
  # https://shiny.rstudio.com/articles/sliders.html
  sliderInput("zeitraum",
              "Zeitraum:",
              min = as.Date("2020-01-01","%Y-%m-%d"),
              max = as.Date("2020-12-31","%Y-%m-%d"),
              value=c(as.Date("2020-01-01"),as.Date("2020-12-31")),
              timeFormat="%Y-%m-%d"),
  plotlyOutput(outputId = "p")
)

server <- function(input, output, session, ...) {
  output$p <- renderPlotly({
    req(input$bl)
    if (identical(input$bl, "")) return(NULL)
    datenrange <- filter(sourcedata, Meldedatum>=as.Date(input$zeitraum[1]) & Meldedatum<=input$zeitraum[2])
    bl <- aggregate(datenrange$AnzahlFall, list(Bundesland = datenrange$Bundesland), sum)
    gefilterteDaten <- filter(bl, Bundesland %in% input$bl) # Quelle: https://plotly-r.com/linking-views-with-shiny.html 17.1.2
    layout(plot_ly(gefilterteDaten,x=gefilterteDaten$Bundesland,y=gefilterteDaten$x), title="Covid Fallzahlen nach Bundesland",
           xaxis=list(title = "Bundesland"),yaxis=list(title = "Gesamtanzahl der Fälle"))
  })
}

shinyApp(ui, server)