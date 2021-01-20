library(shiny)

# Datenquelle: https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/data?orderBy=Meldedatum
originalData <- read.csv("./data/RKI_COVID19-20210120.csv", stringsAsFactors = FALSE)
# stringsAsFactors zum Filtern nach Meldedatum https://stackoverflow.com/questions/51867390/r-programming-filtering-data-frame-with-date-column

einwohnerzahlen <- read.csv("./data/Einwohnerzahlen.csv")
# Quellehttps://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/bevoelkerung-nichtdeutsch-laender.html

# Daten aggegieren, um spaetere Bearbeitung zu beschleunigen (wir brauchen keine Daten zu Landkreisen/Altersgruppe/..)
sourcedata <- aggregate(originalData$AnzahlFall, list(Meldedatum = originalData$Meldedatum, Bundesland = originalData$Bundesland), sum)

laenderdaten <- aggregate(originalData$AnzahlFall, list(Bundesland = originalData$Bundesland), sum)

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
              min = as.Date("2020-01-02","%Y-%m-%d"),
              max = as.Date("2021-01-19","%Y-%m-%d"),
              value=c(as.Date("2020-01-01"),as.Date("2020-12-31")),
              timeFormat="%Y-%m-%d"),
  # https://shiny.rstudio.com/reference/shiny/latest/radioButtons.html
  radioButtons("datentyp", "Datentyp:",
               c("Absolut" = "absolut",
                 "Relativ" = "relativ")),
  plotlyOutput(outputId = "p")
)

server <- function(input, output, session, ...) {
  output$p <- renderPlotly({
    # Quelle: https://plotly-r.com/linking-views-with-shiny.html 17.1.2
    req(input$bl)
    if (identical(input$bl, "")) return(NULL)
    datenrange <- filter(sourcedata, Meldedatum>=as.Date(input$zeitraum[1]) & Meldedatum<=input$zeitraum[2])
    bl <- aggregate(datenrange$x, list(Bundesland = datenrange$Bundesland), sum)
    gefilterteFallzahlen <- filter(bl, Bundesland %in% input$bl) # Quelle: https://plotly-r.com/linking-views-with-shiny.html 17.1.2
    gefilterteEinwohner <- filter(einwohnerzahlen, Bundesland %in% input$bl)
    gefilterteDaten <- data.frame(append(gefilterteFallzahlen, gefilterteEinwohner))
    print(gefilterteDaten)
    title <- "Gesamtanzahl der Fälle"
    fallZahlen <- gefilterteDaten$x
    if(input$datentyp == "relativ") {
      title <- "Faelle pro 100.000 Einwohner"
      print(gefilterteDaten$x)
      print(gefilterteDaten$Einwohner)
      fallZahlen <- (gefilterteDaten$x/gefilterteDaten$Einwohner)*100000
    }
    layout(plot_ly(gefilterteDaten,x=gefilterteDaten$Bundesland,y=fallZahlen), title="Covid Neuerkrankungen nach Bundesland",
           xaxis=list(title = "Bundesland"),yaxis=list(title = title))
  })
}

shinyApp(ui, server)