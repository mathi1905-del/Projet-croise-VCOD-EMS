library(shiny)
library(ggplot2)

bio_medi <- read.csv("ALL_biomedical.csv")

split_data <- strsplit(as.character(bio_medi$nam), "\\.")

mois  <- as.numeric(sapply(split_data, function(x) x[3]))
annee <- as.numeric(sapply(split_data, function(x) x[4]))
pays  <- sapply(split_data, function(x) x[5])

trimestre_num <- ifelse(mois %in% 1:3, 1,
                        ifelse(mois %in% 4:6, 2,
                               ifelse(mois %in% 7:9, 3, 4)))

data_temp <- data.frame(
  pays = pays,
  annee = annee,
  trimestre = trimestre_num
)

table_evolution <- as.data.frame(
  table(data_temp$pays,
        data_temp$annee,
        data_temp$trimestre)
)

colnames(table_evolution) <- c("pays","annee","trimestre","nb_cas")

table_evolution$annee <- as.numeric(as.character(table_evolution$annee))
table_evolution$trimestre <- as.numeric(as.character(table_evolution$trimestre))

table_evolution$temps_continu <- 
  table_evolution$annee + (table_evolution$trimestre - 1) / 4

ui <- fluidPage(
  titlePanel("Evolution continue des cas par pays"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pays_select",
                  "Choisir un pays :",
                  choices = unique(table_evolution$pays))
    ),
    
    mainPanel(
      plotOutput("plot_pays")
    )
  )
)

server <- function(input, output) {
  
  data_filtre <- reactive({
    subset(table_evolution, pays == input$pays_select)
  })
  
  output$plot_pays <- renderPlot({
    ggplot(data_filtre(),
           aes(x = temps_continu,
               y = nb_cas)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "darkblue") +
      theme_minimal() +
      labs(
        title = paste("Evolution continue des cas pour", input$pays_select),
        x = "Temps (année + trimestre)",
        y = "Nombre de cas"
      )
  })
}

shinyApp(ui = ui, server = server)
