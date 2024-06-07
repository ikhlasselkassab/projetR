library(shiny)
library(tidyverse)
library(readr)
library(shinythemes)

# Charger les données
data <- read_csv("owid-covid-data.csv")

# Filtrer les données pour le Maroc et calculer total_recoveries
morocco_data <- data %>%
  filter(location == "Morocco") %>%
  mutate(total_recoveries = total_cases - total_deaths) %>%
  select(iso_code, continent, date, total_cases, new_cases, total_tests, new_tests, total_deaths, new_deaths, total_recoveries, total_vaccinations)

# Calculer des statistiques descriptives
summary_stats <- morocco_data %>%
  summarise(
    mean_total_cases = mean(total_cases, na.rm = TRUE),
    sd_total_cases = sd(total_cases, na.rm = TRUE),
    min_total_cases = min(total_cases, na.rm = TRUE),
    max_total_cases = max(total_cases, na.rm = TRUE),
    
    mean_new_cases = mean(new_cases, na.rm = TRUE),
    sd_new_cases = sd(new_cases, na.rm = TRUE),
    min_new_cases = min(new_cases, na.rm = TRUE),
    max_new_cases = max(new_cases, na.rm = TRUE),
    
    mean_total_tests = mean(total_tests, na.rm = TRUE),
    sd_total_tests = sd(total_tests, na.rm = TRUE),
    min_total_tests = min(total_tests, na.rm = TRUE),
    max_total_tests = max(total_tests, na.rm = TRUE),
    
    mean_new_tests = mean(new_tests, na.rm = TRUE),
    sd_new_tests = sd(new_tests, na.rm = TRUE),
    min_new_tests = min(new_tests, na.rm = TRUE),
    max_new_tests = max(new_tests, na.rm = TRUE),
    
    mean_total_deaths = mean(total_deaths, na.rm = TRUE),
    sd_total_deaths = sd(total_deaths, na.rm = TRUE),
    min_total_deaths = min(total_deaths, na.rm = TRUE),
    max_total_deaths = max(total_deaths, na.rm = TRUE),
    
    mean_new_deaths = mean(new_deaths, na.rm = TRUE),
    sd_new_deaths = sd(new_deaths, na.rm = TRUE),
    min_new_deaths = min(new_deaths, na.rm = TRUE),
    max_new_deaths = max(new_deaths, na.rm = TRUE),
    
    mean_total_recoveries = mean(total_recoveries, na.rm = TRUE),
    sd_total_recoveries = sd(total_recoveries, na.rm = TRUE),
    min_total_recoveries = min(total_recoveries, na.rm = TRUE),
    max_total_recoveries = max(total_recoveries, na.rm = TRUE)
  )

# Définir l'interface utilisateur
ui <- navbarPage(
  theme = shinytheme("cerulean"),
  title = "Analyse des données COVID-19 au Maroc",
  
  tabPanel("À propos de ce projet",
           fluidRow(
             column(10, offset = 1,
                    h3("Contexte de la Pandémie au Maroc"),
                    p("La pandémie de COVID-19 a touché le Maroc comme le reste du monde, provoquant une crise sanitaire sans précédent. Depuis le premier cas détecté en mars 2020, le pays a enregistré des milliers de cas confirmés, de décès et de guérisons. Le gouvernement marocain a mis en place diverses mesures pour contenir la propagation du virus, notamment des confinements, des campagnes de dépistage massives et la vaccination. Malgré ces efforts, la pandémie a eu des impacts significatifs sur la santé publique, l'économie et la société."),
                    img(src = "https://maroc-diplomatique.net/wp-content/uploads/2024/01/WhatsApp-Image-2024-01-12-at-17.03.46.jpeg", height = "400px", class = "img-fluid my-4"),
                    h3("But du Projet"),
                    p("Ce projet vise à fournir une analyse détaillée et compréhensive de l'impact de la COVID-19 au Maroc. En utilisant des données provenant de diverses sources fiables, nous avons pour objectif de fournir des statistiques descriptives pour comprendre l'évolution des cas, des tests, des décès et des guérisons. Nous visualiserons les données à travers des histogrammes, des boîtes à moustaches et des séries temporelles pour identifier les tendances et les anomalies. De plus, nous examinerons les relations entre différentes variables clés via des graphiques de tendances. Enfin, nous effectuerons des analyses probabilistes et des tests d'hypothèses pour déterminer la signification statistique des relations observées entre les variables. En combinant ces approches, nous espérons offrir des insights précieux sur la dynamique de la pandémie au Maroc et aider à informer les décisions politiques et sanitaires futures.")
             )
           )
  ),
  
  tabPanel("Comprendre les données",
           fluidPage(
             titlePanel("Comprendre les données"),
             sidebarLayout(
               sidebarPanel(
                 h3("Statistiques descriptives"),
                 verbatimTextOutput("summary_stats"),
                 
                 h4("Statistiques descriptives détaillées"),
                 h5("Cas totaux"),
                 verbatimTextOutput("stat_total_cases"),
                 h5("Nouveaux cas"),
                 verbatimTextOutput("stat_new_cases"),
                 h5("Tests totaux"),
                 verbatimTextOutput("stat_total_tests"),
                 h5("Nouveaux tests"),
                 verbatimTextOutput("stat_new_tests"),
                 h5("Décès totaux"),
                 verbatimTextOutput("stat_total_deaths"),
                 h5("Nouveaux décès"),
                 verbatimTextOutput("stat_new_deaths"),
                 h5("Guérisons totales"),
                 verbatimTextOutput("stat_total_recoveries")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Histogrammes",
                            plotOutput("hist_new_cases"),
                            plotOutput("hist_new_tests"),
                            plotOutput("hist_new_deaths"),
                   ),
                   tabPanel("Boîtes à moustaches",
                            plotOutput("box_total_cases"),
                            plotOutput("box_total_tests"),
                            plotOutput("box_total_deaths"),
                            plotOutput("box_total_recoveries")
                   ),
                   tabPanel("Séries temporelles",
                            plotOutput("time_series"),
                            plotOutput("time_series_2")
                   )
                 )
               )
             )
           )),
  
  tabPanel("Examiner les données",
           fluidPage(
             titlePanel("Examiner les données"),
             sidebarLayout(
               sidebarPanel(
                 p("Examinez les relations entre les différentes variables."),
                 verbatimTextOutput("correlation_cases_deaths"),
                 verbatimTextOutput("correlation_cases_recoveries"),
                 verbatimTextOutput("correlation_cases_tests")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Graphiques de tendance",
                            plotOutput("trend_cases_deaths"),
                            plotOutput("trend_cases_recoveries"),
                            plotOutput("trend_cases_tests")
                   )
                 )
               )
             )
           )),
  
  tabPanel("Analyse probabiliste",
           fluidPage(
             titlePanel("Analyse probabiliste"),
             sidebarLayout(
               sidebarPanel(
                 p("Test d'hypothèses sur les données.")
               ),
               mainPanel(
                 verbatimTextOutput("hypothesis_tests"),
                 plotOutput("hypothesis_plot"),
                 verbatimTextOutput("wilcoxon_test_results"),
                 verbatimTextOutput("conclusions")
               )
             )
           ))
)

# Définir le serveur
server <- function(input, output) {
  output$summary_stats <- renderPrint({
    print(summary_stats)
  })
  
  output$stat_total_cases <- renderPrint({
    summary(morocco_data$total_cases)
  })
  
  output$stat_new_cases <- renderPrint({
    summary(morocco_data$new_cases)
  })
  
  output$stat_total_tests <- renderPrint({
    summary(morocco_data$total_tests)
  })
  
  output$stat_new_tests <- renderPrint({
    summary(morocco_data$new_tests)
  })
  
  output$stat_total_deaths <- renderPrint({
    summary(morocco_data$total_deaths)
  })
  
  output$stat_new_deaths <- renderPrint({
    summary(morocco_data$new_deaths)
  })
  
  output$stat_total_recoveries <- renderPrint({
    summary(morocco_data$total_recoveries)
  })
  
  output$hist_new_cases <- renderPlot({
    ggplot(morocco_data, aes(x = new_cases)) +
      geom_histogram(binwidth = 100, fill = "blue", color = "black") +
      ggtitle("Distribution des nouveaux cas au Maroc") +
      xlab("Nouveaux cas") +
      ylab("Fréquence")
  })
  
  output$hist_new_tests <- renderPlot({
    ggplot(morocco_data, aes(x = new_tests)) +
      geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
      ggtitle("Distribution des nouveaux tests au Maroc") +
      xlab("Nouveaux tests") +
      ylab("Fréquence")
  })
  
  output$hist_new_deaths <- renderPlot({
    ggplot(morocco_data, aes(x = new_deaths)) +
      geom_histogram(binwidth = 10, fill = "red", color = "black") +
      ggtitle("Distribution des nouveaux décès au Maroc") +
      xlab("Nouveaux décès") +
      ylab("Fréquence")
  })
  
  output$box_total_cases <- renderPlot({
    ggplot(morocco_data, aes(y = total_cases)) +
      geom_boxplot(fill = "blue") +
      ggtitle("Boîte à moustaches des cas totaux au Maroc") +
      ylab("Total des cas")
  })
  
  output$box_total_tests <- renderPlot({
    ggplot(morocco_data, aes(y = total_tests)) +
      geom_boxplot(fill = "blue") +
      ggtitle("Boîte à moustaches des tests totaux au Maroc") +
      ylab("Total des tests")
  })
  
  output$box_total_deaths <- renderPlot({
    ggplot(morocco_data, aes(y = total_deaths)) +
      geom_boxplot(fill = "red") +
      ggtitle("Boîte à moustaches des décès totaux au Maroc") +
      ylab("Total des décès")
  })
  
  output$box_total_recoveries <- renderPlot({
    ggplot(morocco_data, aes(y = total_recoveries)) +
      geom_boxplot(fill = "green") +
      ggtitle("Boîte à moustaches des guérisons totales au Maroc") +
      ylab("Total des guérisons")
  })
  
  output$time_series <- renderPlot({
    ggplot(morocco_data, aes(x = date)) +
      geom_line(aes(y = total_cases, color = "Cas Totaux")) +
      geom_line(aes(y = total_deaths, color = "Décès Totaux")) +
      geom_line(aes(y = total_recoveries, color = "Guérisons Totales")) +
      labs(title = "Évolution des cas, des décès et des guérisons au Maroc",
           x = "Date",
           y = "Nombre",
           color = "Légende") +
      theme_minimal()
  })
  
  output$time_series_2 <- renderPlot({
    ggplot(morocco_data, aes(x = date)) +
      geom_line(aes(y = new_cases, color = "Nouveaux Cas")) +
      geom_line(aes(y = new_deaths, color = "Nouveaux Décès")) +
      geom_line(aes(y = new_tests, color = "Nouveaux Tests")) +
      labs(title = "Évolution des nouveaux cas, décès et tests au Maroc",
           x = "Date",
           y = "Nombre",
           color = "Légende") +
      theme_minimal()
  })
  
  output$trend_cases_deaths <- renderPlot({
    ggplot(morocco_data, aes(x = total_cases, y = total_deaths)) +
      geom_point() +
      geom_smooth(method = "lm", col = "red") +
      labs(title = "Relation entre cas totaux et décès totaux au Maroc",
           x = "Cas Totaux",
           y = "Décès Totaux") +
      theme_minimal()
  })
  
  output$trend_cases_recoveries <- renderPlot({
    ggplot(morocco_data, aes(x = total_cases, y = total_recoveries)) +
      geom_point() +
      geom_smooth(method = "lm", col = "green") +
      labs(title = "Relation entre cas totaux et guérisons totales au Maroc",
           x = "Cas Totaux",
           y = "Guérisons Totales") +
      theme_minimal()
  })
  
  output$trend_cases_tests <- renderPlot({
    ggplot(morocco_data, aes(x = total_cases, y = total_tests)) +
      geom_point() +
      geom_smooth(method = "lm", col = "blue") +
      labs(title = "Relation entre cas totaux et tests totaux au Maroc",
           x = "Cas Totaux",
           y = "Tests Totaux") +
      theme_minimal()
  })
  
  # Résultats des tests de corrélation
  output$correlation_cases_deaths <- renderPrint({
    cor.test(morocco_data$total_cases, morocco_data$total_deaths)
  })
  
  output$correlation_cases_recoveries <- renderPrint({
    cor.test(morocco_data$total_cases, morocco_data$total_recoveries)
  })
  
  output$correlation_cases_tests <- renderPrint({
    cor.test(morocco_data$total_cases, morocco_data$total_tests)
  })
  
  output$hypothesis_tests <- renderPrint({
    # Test de corrélation entre cas totaux et vaccinations totales
    test_total_cases_vs_vaccinations <- cor.test(morocco_data$total_cases, morocco_data$total_vaccinations)
    
    # Test de Wilcoxon pour comparer les médianes
    wilcox_test_cases_vaccinations <- wilcox.test(morocco_data$total_cases, morocco_data$total_vaccinations)
    
    list(
      "Test de corrélation entre cas totaux et vaccinations totales" = test_total_cases_vs_vaccinations,
      "Test de Wilcoxon entre cas totaux et vaccinations totales" = wilcox_test_cases_vaccinations
    )
  })
  
  output$hypothesis_plot <- renderPlot({
    ggplot(morocco_data, aes(x = total_cases, y = total_vaccinations)) +
      geom_point() +
      geom_smooth(method = "lm", col = "blue") +
      labs(title = "Test de corrélation entre cas totaux et vaccinations totales au Maroc",
           x = "Cas Totaux",
           y = "Vaccinations Totales") +
      theme_minimal()
  })
  
  
  
  
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)
