

library(leaflet)
library(ggvis)


fluidPage(
  titlePanel("Liederbeziehung ziwischen TIER1 und TIER2 des OEM1 in DE 2016"),

  fluidRow(
    column(8,
           div(class="innen",

               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
               # If not using custom CSS, set height of leafletOutput to a number instead of percent
               leafletOutput("map", width="100%", height="500")
               # wellPanel(
               #   span("Number of movies selected:",
               #        textOutput("n_movies"),
               #        h2("ZIP explorer"),
               #        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE),
               #        conditionalPanel("input.states",
               #                         selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
               #        ),
               #        conditionalPanel("input.states",
               #                         selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
               #        ),
               #        numericInput("minScore", "Min score", min=0, max=100, value=0)
               #
               #
               #
               #
               #   ),


               )
           ),
    column(4,
           wellPanel(
             h4("Filter"),
             selectInput("Km_Hersteller", "Km_Hersteller", levels(final$Km_Hersteller)),
             h4("Balkendiagramm"),
             #uiOutput("secondSelection"),
             wellPanel(
               plotOutput("histCentile", height = 250)
               #plotOutput("scatterCollegeIncome", height = 250)
             )


             )
    )

  ),
  fluidRow(
           hr(),
           DT::dataTableOutput("ziptable")

  )

  )




