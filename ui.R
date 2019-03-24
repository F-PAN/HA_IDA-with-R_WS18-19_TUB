

library(leaflet)
library(ggvis)


fluidPage(
  # title
  titlePanel("Liederbeziehung ziwischen TIER1 und TIER2 des OEM1 in DE 2016"),

  fluidRow(
    column(8,
           # add map
           div(class="innen",

               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
               # If not using custom CSS, set height of leafletOutput to a number instead of percent
               leafletOutput("map", width="100%", height="500")
    
               )
           ),
    column(4,
           wellPanel(
             # dropdown list
             h4("Filter"),
             selectInput("Km_Hersteller", "Km_Hersteller", levels(final$Km_Hersteller)),
             
             # diagramm
             h4("Balkendiagramm"),
             #uiOutput("secondSelection"),
             wellPanel(
               plotOutput("histCentile", height = 250)
             )


             )
    )

  ),
  fluidRow(
           hr(),
           # show data
           DT::dataTableOutput("ziptable")

  )

  )




