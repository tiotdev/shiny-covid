library(plotly)
library(shiny)

# Datenquelle: https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/data?orderBy=Meldedatum
rkiDaten <- read.csv("./data/RKI_COVID19-20210120.csv", stringsAsFactors = FALSE)
# stringsAsFactors zum Filtern nach Meldedatum https://stackoverflow.com/questions/51867390/r-programming-filtering-data-frame-with-date-column

einwohnerZahlen <- read.csv("./data/Einwohnerzahlen.csv")
# Quelle https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsstand/Tabellen/bevoelkerung-nichtdeutsch-laender.html

# Daten aggegieren, um spaetere Bearbeitung zu beschleunigen (wir brauchen keine Daten zu Landkreisen/Altersgruppe/..)
faelleNachDatumUndLand <- aggregate(rkiDaten$AnzahlFall, list(Meldedatum = rkiDaten$Meldedatum, Bundesland = rkiDaten$Bundesland), sum)

# Aggregieren fuer den Dropdown zum Auswaehlen der Bundeslaender
bundesLaender <- aggregate(rkiDaten$AnzahlFall, list(Bundesland = rkiDaten$Bundesland), sum)
laenderAuswahl <- bundesLaender[1]

ui <- fluidPage(
  # Layout mit Sidebar: https://github.com/rstudio-education/shiny.rstudio.com-tutorial/blob/master/part-1-code/app.R
  headerPanel('Covid Fälle nach Bundesland'),
  sidebarPanel(
  # Dropdown zum Ausawehlen der Bundeslaender. Quelle: https://plotly-r.com/linking-views-with-shiny.html 17.1.2
  selectizeInput("bl",
                 "Bundesländer:",
    # placeholder is enabled when 1st choice is an empty string
    choices = c("Nach Bundesland filtern" = "", laenderAuswahl), 
    multiple = TRUE
  ),
  # Slider zur Auswahl des Zeitraums
  # Quelle: https://shiny.rstudio.com/articles/sliders.html
  # Quelle: https://stackoverflow.com/questions/40908808/how-to-sliderinput-for-dates
  sliderInput("zeitraum",
              "Zeitraum:",
              min = as.Date("2020-01-02","%Y-%m-%d"),
              max = as.Date("2021-01-19","%Y-%m-%d"),
              value=c(as.Date("2020-01-01"),as.Date("2021-01-19")),
              timeFormat="%Y-%m-%d"),
  # Radio Buttons zur Auswahl von Datentyp: https://shiny.rstudio.com/reference/shiny/latest/radioButtons.html
  radioButtons("datentyp", "Datentyp:",
               c("Relativ" = "relativ","Absolut" = "absolut")),
  ),
  mainPanel(
    plotlyOutput(outputId = "balkenDiagramm"),
    plotlyOutput(outputId = "linienDiagramm")
  )
)

server <- function(input, output, session, ...) {
  output$balkenDiagramm <- renderPlotly({
    # Nach gewaehltem Zeitraum filtern
    # Filtern Quelle: https://plotly-r.com/linking-views-with-shiny.html 17.1.2
    faelleImZeitraum <- filter(faelleNachDatumUndLand, Meldedatum>=as.Date(input$zeitraum[1]) & Meldedatum<=input$zeitraum[2])
    # Aggregieren nach gewaehlten Bundeslaendern
    bl <- aggregate(faelleImZeitraum$x, list(Bundesland = faelleImZeitraum$Bundesland), sum)
    # Einwohnerzahlen nach Bundesland zu Datensatz hinzufuegen
    gefilterteFallzahlen <- filter(bl, Bundesland %in% input$bl) 
    gefilterteEinwohner <- filter(einwohnerZahlen, Bundesland %in% input$bl)
    gefilterteDaten <- data.frame(append(gefilterteFallzahlen, gefilterteEinwohner))
    # Falls nicht nach Bundesland gefiltert, zeige alle Bundeslaender
    if (identical(input$bl, NULL)) {
      gefilterteDaten <- data.frame(append(bl, einwohnerZahlen))
    }
    yTitle <- "Gesamtanzahl der Fälle"
    fallZahlen <- gefilterteDaten$x
    # Concatenate Strings Quelle: https://www.math.ucla.edu/~anderson/rw1001/library/base/html/paste.html
    diagrammTitle <- paste("Kumulierte Covid Neuerkrankungen nach Bundesland von", as.Date(input$zeitraum[1], "%Y-%m-%d"), "bis", as.Date(input$zeitraum[2], "%Y-%m-%d"))
    if(input$datentyp == "relativ") {
      yTitle <- "Fälle pro 100.000 Einwohner"
      fallZahlen <- (gefilterteDaten$x/gefilterteDaten$Einwohner)*100000
    }
    # Plotly - siehe 1. Abgabe
    layout(plot_ly(gefilterteDaten,x=gefilterteDaten$Bundesland,y=fallZahlen), title=diagrammTitle,
           xaxis=list(title = "Bundesland"),yaxis=list(title = yTitle))
  })
  output$linienDiagramm <- renderPlotly({
    # Nach gewaehltem Zeitraum filtern
    # Filtern Quelle: https://plotly-r.com/linking-views-with-shiny.html 17.1.2
    faelleImZeitraum <- filter(faelleNachDatumUndLand, Meldedatum>=as.Date(input$zeitraum[1]) & Meldedatum<=input$zeitraum[2])
    # Aggregieren nach gewaehlten Bundeslaendern
    gefilterteFallzahlen <- filter(faelleImZeitraum, Bundesland %in% input$bl) 
    gefilterteEinwohner <- filter(einwohnerZahlen, Bundesland %in% input$bl)
    # https://stackoverflow.com/questions/14102498/merge-dataframes-different-lengths
    gefilterteEinwohner$variable <- rownames(gefilterteEinwohner)
    gefilterteDaten <- merge(gefilterteFallzahlen, gefilterteEinwohner)
    if (identical(input$bl, NULL)) {
      einwohnerZahlen$variable <- rownames(einwohnerZahlen)
      gefilterteDaten <- merge(faelleImZeitraum, einwohnerZahlen)
    }
    fallZahlen <- gefilterteDaten$x
    yTitle <- "Neuerkrankungen"
    diagrammTitle <- paste("Verlauf täglicher Covid Neuerkrankungen nach Bundesland von", as.Date(input$zeitraum[1], "%Y-%m-%d"), "bis", as.Date(input$zeitraum[2], "%Y-%m-%d"))
    if(input$datentyp == "relativ") {
      yTitle <- "Neuerkrankungen pro 100.000 Einwohner"
      fallZahlen <- (gefilterteDaten$x/gefilterteDaten$Einwohner)*100000
    }
    
    # Quelle: https://plotly.com/r/line-charts/ Density Plot
    fig <- plot_ly(gefilterteDaten, x=gefilterteDaten$Meldedatum,y=fallZahlen, color = gefilterteDaten$Bundesland) 
    fig <- fig %>% add_lines()
    
    layout(fig, title=diagrammTitle,
           xaxis=list(title = "Datum"),yaxis=list(title = yTitle))
  })
}

shinyApp(ui, server)