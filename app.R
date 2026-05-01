# ==========================================
# 1. LIBRARY
# ==========================================
library(shiny)
library(bs4Dash)
library(tidyverse)
library(cluster)
library(factoextra)
library(DT)
library(plotly)
library(ggpubr)

# ==========================================
# 2. DATA INPUT & PROCESSING (Global)
# ==========================================
# Load Data
raw_data <- read.csv("DASHBOARD SEGMENTASI WISATA BALI/Destinasi Wisata Bali.csv", stringsAsFactors = FALSE)

# Data Cleaning & Feature Engineering
clean_data <- raw_data %>%
  # Konversi ke numerik & Hapus NA
  mutate(
    totalScore = as.numeric(totalScore),
    reviewsCount = as.numeric(reviewsCount)
  ) %>%
  drop_na(totalScore, reviewsCount, title) %>%
  # Hapus Duplikasi
  distinct(title, .keep_all = TRUE) %>%
  # Pastikan reviewsCount >= 0
  filter(reviewsCount >= 0) %>%
  # Feature Engineering
  mutate(
    Skor_Kenyamanan = totalScore * 20,
    Skor_Popularitas = log(reviewsCount + 1),
    city = ifelse(city == "" | is.na(city), "Other", city)
  )

# ==========================================
# 3. UI DESIGN (bs4Dash)
# ==========================================
ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Bali Tourism Clusters",
      color = "primary",
      image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
    )
  ),
  
  sidebar = dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-pie")),
      menuItem("Segmentasi Cluster", tabName = "segmentasi", icon = icon("layer-group")),
      menuItem("Analisis Popularitas", tabName = "popularitas", icon = icon("fire")),
      menuItem("Analisis Rating", tabName = "rating", icon = icon("star")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      hr(),
      # FILTERS
      h6("Filter Data", style = "margin-left: 20px; color: gray;"),
      selectInput("filter_city", "Pilih Kota:", 
                  choices = c("All", sort(unique(clean_data$city))), selected = "All"),
      selectInput("filter_cat", "Kategori Utama:", 
                  choices = c("All", sort(unique(clean_data$categoryName))), selected = "All"),
      sliderInput("filter_rating", "Range Rating:", min = 0, max = 5, value = c(0, 5), step = 0.1),
      hr(),
      # CLUSTERING SETTINGS
      h6("Pengaturan Klaster", style = "margin-left: 20px; color: gray;"),
      selectInput("linkage_method", "Metode Linkage:", 
                  choices = c("ward.D2", "complete", "average")),
      numericInput("k_cluster", "Jumlah Cluster (k):", value = 3, min = 2, max = 6)
    )
  ),
  
  body = dashboardBody(
    # Pastikan tabItems membungkus semua tabItem
    tabItems(
      
      # --- TAB 1: OVERVIEW ---
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("box_total_dest", width = 4),
          valueBoxOutput("box_avg_rating", width = 4),
          valueBoxOutput("box_total_rev", width = 4)
        ),
        fluidRow(
          column(width = 8,
                 box(
                   title = "Top 10 Destinasi Terpopuler (Reviews)",
                   status = "primary", width = 12,
                   plotlyOutput("plot_top_dest")
                 )
          ),
          column(width = 4,
                 box(
                   title = "Proporsi Cluster",
                   status = "info", width = 12,
                   plotlyOutput("plot_pie_cluster")
                 )
          )
        )
      ),
      
      # --- TAB 2: SEGMENTASI ---
      tabItem(
        tabName = "segmentasi",
        fluidRow(
          column(width = 12,
                 uiOutput("insight_box") # InfoBox otomatis
          )
        ),
        fluidRow(
          box(
            title = "Visualisasi Klaster (PCA)", 
            status = "primary", 
            width = 7, 
            solidHeader = TRUE,
            elevation = 2,
            # Gunakan withSpinner jika library shinycssloaders tersedia
            plotOutput("plot_cluster_pca", height = "500px") 
          ),
          box(
            title = "Dendrogram Hierarki", 
            status = "info", 
            width = 5, 
            solidHeader = TRUE,
            elevation = 2,
            plotOutput("plot_dendro", height = "500px")
          )
        )
      ),
      
      # --- TAB 3: ANALISIS POPULARITAS ---
      tabItem(
        tabName = "popularitas",
        box(
          title = "Hubungan Popularitas vs Kenyamanan (Rating)",
          width = 12, status = "danger", solidHeader = TRUE,
          plotlyOutput("plot_scatter")
        )
      ),
      
      # --- TAB 4: ANALISIS RATING ---
      tabItem(
        tabName = "rating",
        box(
          title = "Distribusi Rating per Cluster",
          width = 12, status = "warning", solidHeader = TRUE,
          plotlyOutput("plot_box_rating")
        )
      ),
      
      # --- TAB 5: DATA EXPLORER ---
      tabItem(
        tabName = "data",
        box(
          title = "Dataset Destinasi Wisata Bali",
          width = 12, status = "secondary",
          DTOutput("table_raw")
        )
      )
    )
  ),
  
  footer = dashboardFooter(
    left = "Analisis Segmentasi Bali",
    right = "2024 © Bali Tourism Insights"
  )
)
# ==========================================
# 4. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  # --- SERVER: Logika Visualisasi PCA ---
  output$plot_cluster_pca <- renderPlot({
    # Mengambil data hasil kalkulasi dari reactive processed_data()
    res <- processed_data()
    
    fviz_cluster(
      list(data = res$scale, cluster = res$data$Cluster),
      palette = "jco",            # Tema warna jurnal (biru, kuning, abu)
      geom = "point",             # Hanya menampilkan titik (hindari tumpang tindih teks)
      ellipse.type = "convex",    # Membungkus klaster dengan garis cembung
      repel = TRUE,               # Menghindari teks label bertumpukan (jika ada label)
      show.clust.cent = TRUE,     # Menampilkan pusat klaster
      ggtheme = theme_minimal(),
      main = "Visualisasi Segmentasi (Principal Component Analysis)"
    ) +
      theme(legend.position = "bottom")
  })
  
  # --- SERVER: Logika Visualisasi Dendrogram ---
  output$plot_dendro <- renderPlot({
    # Mengambil data hasil kalkulasi dari reactive processed_data()
    res <- processed_data()
    
    fviz_dend(
      res$hc, 
      k = input$k_cluster,        # Memotong dendrogram sesuai input jumlah k
      cex = 0.6,                  # Ukuran teks label
      k_colors = "jco",           # Warna klaster mengikuti palet PCA
      rect = TRUE,                # Memberi kotak pembatas di setiap klaster
      rect_fill = TRUE,           # Memberi warna transparan pada kotak
      rect_border = "jco",
      labels_track_height = 0.8,  # Memberi ruang untuk label di bawah
      main = "Dendrogram Pengelompokan Hierarki"
    ) +
      theme_minimal()
  })
  # --- Reactive Data Processing ---
  processed_data <- reactive({
    df <- clean_data
    
    # Apply Sidebar Filters
    if(input$filter_city != "All") df <- df %>% filter(city == input$filter_city)
    if(input$filter_cat != "All") df <- df %>% filter(categoryName == input$filter_cat)
    df <- df %>% filter(totalScore >= input$filter_rating[1], totalScore <= input$filter_rating[2])
    
    # Scaling untuk Clustering
    df_scale <- df %>%
      select(Skor_Kenyamanan, Skor_Popularitas) %>%
      scale()
    
    # Hierarchical Clustering
    dist_mat <- dist(df_scale, method = "euclidean")
    hc_res <- hclust(dist_mat, method = input$linkage_method)
    clusters <- cutree(hc_res, k = input$k_cluster)
    
    df$Cluster <- as.factor(clusters)
    return(list(data = df, hc = hc_res, scale = df_scale))
  })
  
  # --- Value Boxes ---
  output$box_total_dest <- renderValueBox({
    valueBox(nrow(processed_data()$data), "Total Destinasi", icon = icon("map-marker-alt"), color = "primary")
  })
  
  output$box_avg_rating <- renderValueBox({
    avg_r <- mean(processed_data()$data$totalScore, na.rm = TRUE)
    valueBox(round(avg_r, 2), "Rata-rata Rating", icon = icon("star"), color = "warning")
  })
  
  output$box_total_rev <- renderValueBox({
    total_r <- sum(processed_data()$data$reviewsCount, na.rm = TRUE)
    valueBox(format(total_r, big.mark=","), "Total Review", icon = icon("users"), color = "info")
  })
  
  # --- Visualizations ---
  output$plot_top_dest <- renderPlotly({
    p <- processed_data()$data %>%
      arrange(desc(reviewsCount)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(title, reviewsCount), y = reviewsCount, fill = Cluster)) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      labs(x = "", y = "Jumlah Review")
    ggplotly(p)
  })
  
  output$plot_pie_cluster <- renderPlotly({
    df_pie <- processed_data()$data %>% count(Cluster)
    plot_ly(df_pie, labels = ~Cluster, values = ~n, type = 'pie', hole = 0.4) %>%
      layout(showlegend = TRUE)
  })
  
  output$plot_cluster_pca <- renderPlot({
    fviz_cluster(list(data = processed_data()$scale, cluster = processed_data()$data$Cluster),
                 palette = "jco", geom = "point", ellipse.type = "convex", 
                 ggtheme = theme_minimal(), main = "Visualisasi Cluster Destinasi")
  })
  
  output$plot_dendro <- renderPlot({
    fviz_dend(processed_data()$hc, k = input$k_cluster, 
              cex = 0.5, k_colors = "jco", rect = TRUE, 
              main = "Dendrogram Cluster Wisata")
  })
  
  output$plot_scatter <- renderPlotly({
    p <- ggplot(processed_data()$data, aes(x = reviewsCount, y = totalScore, color = Cluster, text = title)) +
      geom_point(alpha = 0.6) +
      scale_x_log10() +
      theme_minimal() +
      labs(x = "Popularitas (Log Scale Reviews)", y = "Kenyamanan (Rating)")
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_box_rating <- renderPlotly({
    p <- ggplot(processed_data()$data, aes(x = Cluster, y = totalScore, fill = Cluster)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Distribusi Rating Berdasarkan Cluster", y = "Rating (1-5)")
    ggplotly(p)
  })
  
  # --- Data Table ---
  output$table_raw <- renderDT({
    datatable(processed_data()$data %>% select(title, categoryName, city, totalScore, reviewsCount, Cluster),
              options = list(pageLength = 10, scrollX = TRUE), filter = 'top')
  })
  
  # --- Insight Otomatis ---
  output$insight_box <- renderUI({
    df <- processed_data()$data
    
    summary_cluster <- df %>%
      group_by(Cluster) %>%
      summarise(
        avg_pop = mean(reviewsCount),
        avg_ken = mean(totalScore)
      )
    
    top_pop_cluster <- summary_cluster$Cluster[which.max(summary_cluster$avg_pop)]
    top_ken_cluster <- summary_cluster$Cluster[which.max(summary_cluster$avg_ken)]
    
    # Korelasi sederhana
    corr <- cor(df$reviewsCount, df$totalScore)
    corr_msg <- if(corr > 0.3) "Ada kecenderungan destinasi populer memiliki rating tinggi." else "Popularitas tidak selalu menjamin rating yang tinggi."
    
    fluidRow(
      infoBox("Most Popular", paste("Cluster", top_pop_cluster), icon = icon("fire"), color = "danger", width = 4, fill = TRUE),
      infoBox("Most Comfortable", paste("Cluster", top_ken_cluster), icon = icon("smile"), color = "success", width = 4, fill = TRUE),
      infoBox("Trend Insight", corr_msg, icon = icon("lightbulb"), color = "warning", width = 4, fill = TRUE)
    )
  })
}

# ==========================================
# 5. RUN APPLICATION
# ==========================================
shinyApp(ui = ui, server = server)