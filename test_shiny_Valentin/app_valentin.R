library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)

# import de mes ancienne données agricoles
read_csv2("subve.csv", locale = locale(encoding = "Latin1")) -> subve 

# ce que j'avais deja fais: si on fais tout d'un coup les indicateur par région:
subve %>%
  group_by(REGION) %>%
  summarise(across(c(SUBV, NBEX, RBE, PROD, POPULATION, SUPERFICIE), sum)) %>%
  mutate(subv_par_expl = SUBV / NBEX,
         subv_par_hab  = SUBV / POPULATION,
         subv_par_km2  = SUBV / SUPERFICIE,
         rbe_par_expl  = RBE / NBEX,
         prod_par_expl = PROD / NBEX) -> agri_region_indi 

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Typologie des Régions Agricoles Françaises"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #axex
      selectInput("axe_x",
                  "Variable axe X:",
                  choices = c("Subventions par exploitation" = "subv_par_expl",
                              "Subventions par habitant" = "subv_par_hab",
                              "Production par exploitation" = "prod_par_expl"),
                  selected = "subv_par_expl"),
      
      # axe y
      selectInput("axe_y",
                  "Variable axe Y:",
                  choices = c("RBE par exploitation" = "rbe_par_expl",
                              "Production par exploitation" = "prod_par_expl",
                              "Subventions par exploitation" = "subv_par_expl"),
                  selected = "rbe_par_expl"),
      
      #pour plus ou moins gerer la transparence des points pour toujours voir meme si des points se chevauchent 
      sliderInput("transparence",
                  "Transparence des points:",
                  min = 0.1,
                  max = 1,
                  value = 0.5,
                  step = 0.1),
      
      # pouvoir selection les gerions que l'on desire
      checkboxGroupInput("regions",
                         "Régions à afficher:",
                         choices = unique(agri_region_indi$REGION),
                         selected = unique(agri_region_indi$REGION))),
    
    mainPanel(
      plotOutput("region_Plot", height = "600px"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$region_Plot <- renderPlot({
    
    # Filtrer pour les régions sélectionnées
    agri_region_indi %>%
      filter(REGION %in% input$regions) -> data_filtered ### %in% donné par l'IA Claude qui veut dire: il garde seulement les lignes où la region est dans la liste des régions sélectionnées
    
   
    labels_x <- c("subv_par_expl" = "Subventions par exploitation","subv_par_hab" = "Subventions par habitant","prod_par_expl" = "Production par exploitation")
    
    labels_y <- c("rbe_par_expl" = "RBE par exploitation","prod_par_expl" = "Production par exploitation","subv_par_expl" = "Subventions par exploitation")
    
    #le graph ggplot
    data_filtered %>%
      ggplot +
        aes(x = .data[[input$axe_x]], y = .data[[input$axe_y]], color = REGION, size = NBEX) + ### ".data[[input$axe_x]]" donné par l'IA Claude qui veut dire: R cherche la colonne dont le nom est ds input$axe_x que j'ai créé dans ui
        
        geom_point(alpha = input$transparence) +
        
        scale_size_continuous(name = "Nombre d'exploitations",range = c(6, 20)) +
        scale_color_viridis_d(name = "Région") +
        
        xlab(labels_x[input$axe_x]) + 
        ylab(labels_y[input$axe_y]) + 
        ggtitle("Typologie des régions agricoles françaises") +
        
        theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)