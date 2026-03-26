# ============================================================
# Shiny — Détection amorce avec tolérance (mutations)
# Optimisé : bouton "Lancer" + échantillon + vcountPattern
# + Heatmap PRO : toggle Brut / % + tri + labels
# ============================================================

library(shiny)
library(seqinr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(DT)
library(Biostrings)

# -----------------------------
# Chemin fichier
# -----------------------------
fasta_path <- "C:/Users/s2mfa/Documents/projcrois/all/all/ALL_sequences.fasta"

# -----------------------------
# Lecture UNE SEULE FOIS (au démarrage)
# -----------------------------
ALI <- read.alignment(file = fasta_path, format = "fasta")
ALI_DF <- tibble(id = ALI$nam, raw_seq = ALI$seq)

# -----------------------------
# Nettoyage : minuscules + remove gaps + remove spaces
# -----------------------------
clean_dna <- function(x, remove_gaps = TRUE) {
  x <- tolower(x)
  x <- gsub("\\s+", "", x)
  if (remove_gaps) x <- gsub("-", "", x, fixed = TRUE)
  x
}

# -----------------------------
# Parsing ID robuste (jour.mois.annee.pays)
# Exemple: HQ700017.19.6.1990.PT
# -----------------------------
parse_header <- function(id) {
  id <- trimws(id)
  m <- str_match(id, ".*\\.(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})\\.([A-Za-z]{2})$")
  
  if (all(is.na(m))) {
    return(tibble(
      id = id,
      country = NA_character_,
      year = NA_integer_,
      month = NA_integer_,
      day = NA_integer_,
      quarter = NA_character_
    ))
  }
  
  day <- as.integer(m[2])
  month <- as.integer(m[3])
  year <- as.integer(m[4])
  country <- toupper(m[5])
  
  quarter <- case_when(
    month %in% 1:3 ~ "Q1",
    month %in% 4:6 ~ "Q2",
    month %in% 7:9 ~ "Q3",
    month %in% 10:12 ~ "Q4",
    TRUE ~ NA_character_
  )
  
  tibble(id = id, country = country, year = year, month = month, day = day, quarter = quarter)
}

META <- bind_rows(lapply(ALI_DF$id, parse_header))
DATA0 <- left_join(ALI_DF, META, by = "id")

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  titlePanel("Détection d'amorce dans FASTA (tolérance mutations)"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("motif", "Motif à détecter", value = "gggccc"),
      sliderInput("tol", "Tolérance (erreurs autorisées)", min = 0, max = 4, value = 2, step = 1),
      checkboxInput("remove_gaps", "Supprimer les gaps '-'", value = TRUE),
      
      hr(),
      radioButtons(
        "heat_mode",
        "Affichage heatmap",
        choices = c("Pourcentage (%)" = "pct", "Total brut" = "raw"),
        selected = "pct",
        inline = TRUE
      ),
      
      hr(),
      checkboxInput("use_sample", "Mode rapide : analyser un échantillon", value = TRUE),
      numericInput("sample_n", "Taille échantillon", value = 300, min = 50, max = 5000, step = 50),
      
      hr(),
      actionButton("run", "Lancer l'analyse", class = "btn-primary"),
      
      hr(),
      uiOutput("country_ui"),
      uiOutput("year_ui")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Heatmap", plotOutput("heatmap", height = 560)),
        tabPanel("Top pays", plotOutput("bar_country", height = 420)),
        tabPanel("Table", DTOutput("table")),
        tabPanel("Résumé", verbatimTextOutput("summary"))
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # Filtres UI (toujours dispo, même avant run)
  output$country_ui <- renderUI({
    choices <- c("Tous"="__all__", sort(unique(na.omit(DATA0$country))))
    selectInput("country", "Filtrer pays", choices = choices, selected="__all__")
  })
  
  output$year_ui <- renderUI({
    yrs <- sort(unique(na.omit(DATA0$year)))
    choices <- c("Toutes"="__all__", yrs)
    selectInput("year", "Filtrer année", choices = choices, selected="__all__")
  })
  
  # ✅ Calcul uniquement quand on clique "Lancer"
  computed <- eventReactive(input$run, {
    
    motif <- tolower(input$motif)
    tol <- as.integer(input$tol)
    
    df <- DATA0
    
    if (isTRUE(input$use_sample)) {
      n <- min(nrow(df), as.integer(input$sample_n))
      set.seed(1)
      df <- df %>% slice_sample(n = n)
    }
    
    withProgress(message = "Calcul des occurrences...", value = 0, {
      incProgress(0.2, detail = "Nettoyage des séquences")
      seq_clean <- vapply(df$raw_seq, clean_dna, character(1),
                          remove_gaps = isTRUE(input$remove_gaps))
      
      incProgress(0.6, detail = "Comptage du motif (Biostrings)")
      seq_set <- BStringSet(seq_clean)
      
      occ <- vcountPattern(
        BString(motif),
        seq_set,
        max.mismatch = tol,
        fixed = TRUE
      )
      
      incProgress(0.2, detail = "Finalisation")
      
      df %>%
        mutate(
          seq_clean = seq_clean,
          occurrences = as.integer(occ),
          seq_len = nchar(seq_clean)
        )
    })
  }, ignoreInit = TRUE)
  
  # Application filtres après calcul
  filtered <- reactive({
    df <- computed()
    req(df)
    
    if (!is.null(input$country) && input$country != "__all__") df <- df %>% filter(country == input$country)
    if (!is.null(input$year) && input$year != "__all__") df <- df %>% filter(year == as.integer(input$year))
    df
  })
  
  # Heatmap PRO (toggle Brut / % + tri + labels)
  output$heatmap <- renderPlot({
    df <- filtered()
    req(df)
    
    validate(
      need(sum(!is.na(df$country)) > 0, "Aucun pays détecté."),
      need(sum(!is.na(df$quarter)) > 0, "Aucun trimestre détecté.")
    )
    
    d2 <- df %>%
      filter(!is.na(country), !is.na(quarter)) %>%
      group_by(country, quarter) %>%
      summarise(
        total_seq = n(),
        total_occ = sum(occurrences),
        seq_with_motif = sum(occurrences > 0),
        percent = 100 * seq_with_motif / total_seq,
        .groups = "drop"
      )
    
    d2$quarter <- factor(d2$quarter, levels = c("Q1","Q2","Q3","Q4"))
    
    if (input$heat_mode == "pct") {
      # tri par % moyen
      order_countries <- d2 %>%
        group_by(country) %>%
        summarise(m = mean(percent), .groups = "drop") %>%
        arrange(desc(m)) %>%
        pull(country)
      
      d2$country <- factor(d2$country, levels = order_countries)
      
      ggplot(d2, aes(x = quarter, y = country, fill = percent)) +
        geom_tile(color = "white") +
        geom_text(aes(label = sprintf("%.1f", percent)), size = 3) +
        scale_fill_viridis_c(name = "% séquences\navec motif") +
        labs(
          x = "Trimestre",
          y = "Pays",
          title = paste0("Heatmap — Prévalence du motif '", tolower(input$motif),
                         "' (tolérance=", input$tol, ")")
        ) +
        theme_minimal()
    } else {
      # tri par total brut
      order_countries <- d2 %>%
        group_by(country) %>%
        summarise(m = sum(total_occ), .groups = "drop") %>%
        arrange(desc(m)) %>%
        pull(country)
      
      d2$country <- factor(d2$country, levels = order_countries)
      
      ggplot(d2, aes(x = quarter, y = country, fill = total_occ)) +
        geom_tile(color = "white") +
        geom_text(aes(label = total_occ), size = 3) +
        scale_fill_viridis_c(name = "Occurrences\n(total)") +
        labs(
          x = "Trimestre",
          y = "Pays",
          title = paste0("Heatmap — Total brut d'occurrences '", tolower(input$motif),
                         "' (tolérance=", input$tol, ")")
        ) +
        theme_minimal()
    }
  })
  
  # Top pays (toujours en brut : total occurrences)
  output$bar_country <- renderPlot({
    df <- filtered()
    req(df)
    
    d2 <- df %>%
      filter(!is.na(country)) %>%
      group_by(country) %>%
      summarise(total_occ = sum(occurrences), .groups = "drop") %>%
      arrange(desc(total_occ)) %>%
      slice_head(n = 15)
    
    validate(need(nrow(d2) > 0, "Pas de pays utilisable."))
    
    ggplot(d2, aes(x = reorder(country, total_occ), y = total_occ)) +
      geom_col() +
      coord_flip() +
      labs(x="Pays", y="Occurrences totales", title="Top 15 pays") +
      theme_minimal()
  })
  
  # Table
  output$table <- renderDT({
    df <- filtered()
    req(df)
    
    datatable(
      df %>%
        select(id, country, year, month, day, quarter, seq_len, occurrences) %>%
        arrange(desc(occurrences)),
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  # Résumé
  output$summary <- renderPrint({
    df <- computed()
    req(df)
    
    cat("Motif :", tolower(input$motif), "\n")
    cat("Tolérance :", input$tol, "erreur(s)\n")
    cat("Affichage heatmap :", ifelse(input$heat_mode == "pct", "Pourcentage (%)", "Total brut"), "\n")
    cat("Supprimer gaps :", ifelse(input$remove_gaps, "OUI", "NON"), "\n")
    cat("Mode échantillon :", ifelse(input$use_sample, "OUI", "NON"), "\n")
    cat("Nb séquences analysées :", nrow(df), "\n")
    cat("Occurrences totales :", sum(df$occurrences), "\n")
    cat("Séquences avec >=1 occurrence :", sum(df$occurrences > 0), "\n\n")
    
    cat("Parsing IDs:\n")
    cat("country non-NA :", sum(!is.na(df$country)), "\n")
    cat("quarter non-NA :", sum(!is.na(df$quarter)), "\n\n")
    
    cat("Exemples d'ID:\n")
    print(head(df$id, 5))
  })
}

shinyApp(ui, server)
