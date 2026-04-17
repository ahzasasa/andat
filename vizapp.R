library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(treemapify)
library(bslib)
library(shinycssloaders)



# 1.) UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", primary = "#1389CA"), 
  
  titlePanel(
    tags$div(
      icon("chart-pie", style = "color: #DE2C2D;"), 
      tags$strong("Punya ANDAT")
    )
  ),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #ffffff; box-shadow: 0px 4px 15px rgba(0,0,0,0.05); border-radius: 8px; border: none;",
      
      fileInput("file_data", tags$b("1. Unggah File Data (.csv)"), 
                accept = ".csv", buttonLabel = "Telusuri", placeholder = "Pilih file"),
      
      selectInput("kolom_survei", tags$b("2. Pilih Pertanyaan (Kolom)"), 
                  choices = c("Menunggu file diunggah" = "")),
      
      selectInput("jenis_chart", tags$b("3. Pilih Jenis Chart"), 
                  choices = c("Column", "Line", "Pie", "Bar", "Area", 
                              "X Y (Scatter)", "Map", "Stock", "Surface", 
                              "Radar", "Treemap", "Sunburst", "Histogram", 
                              "Box & Whisker", "Waterfall", "Funnel", "Combo")),
      
      sliderInput("top_n", tags$b("4. Tampilkan N Jawaban Teratas"), 
                  min = 3, max = 20, value = 10),
      
      textInput("judul_plot", tags$b("5. Ketik Judul Grafik"), 
                placeholder = "Kosongkan untuk pakai judul bawaan"),
      
      radioButtons("opsi_warna", tags$b("6. Pilih Palet Warna"),
                   choices = c("Warna-warni (Pelangi)" = "pelangi",
                               "Palet Inklusif (Aksesibilitas Visual)" = "colorblind"),
                   selected = "pelangi"),
      
      br(),
      downloadButton("download_plot", "Unduh Grafik (PNG)", 
                     class = "btn-success", 
                     style = "width: 100%; font-weight: bold; font-size: 15px; border-radius: 6px; background-color: #8CC63F; border-color: #8CC63F;"),
      
      hr(),
      helpText(
        icon("lightbulb", style = "color: #FAD201;"), 
        "Tips: Klik kanan pada gambar lalu pilih 'Copy Image' untuk menyalin grafik."
      )
    ),
    
    mainPanel(
      tags$div(
        style = "background-color: #ffffff; box-shadow: 0px 4px 15px rgba(0,0,0,0.05); border-radius: 8px; padding: 20px; min-height: 600px;",
        
        conditionalPanel(
          condition = "output.file_uploaded == false",
          tags$div(
            style = "text-align: center; color: #95a5a6; padding-top: 150px;",
            icon("inbox", class = "fa-5x"),
            tags$h3("Ruang Kerja Kosong", style = "font-weight: bold; color: #662D91; margin-top: 20px;"),
            tags$p("Mari mulai dengan mengunggah data CSV di panel sebelah kiri.")
          )
        ),
        
        conditionalPanel(
          condition = "output.file_uploaded == true",
          withSpinner(plotOutput("plot_hasil", height = "550px"), type = 8, color = "#F37121", size = 0.7)
        )
      )
    )
  )
)



# 2.) logika server
server <- function(input, output, session) {
  
  output$file_uploaded <- reactive({
    return(!is.null(input$file_data))
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  data_input <- reactive({
    req(input$file_data)
    read.csv(input$file_data$datapath, stringsAsFactors = FALSE)
  })
  
  observe({
    req(data_input())
    updateSelectInput(session, "kolom_survei", choices = names(data_input()))
  })
  
  plot_obj <- reactive({
    req(data_input(), input$kolom_survei, input$kolom_survei != "")
    
    df <- data_input()
    kolom <- input$kolom_survei
    jenis <- input$jenis_chart
    opsi_palet <- input$opsi_warna
    
    unsupported_charts <- c("Map", "Stock", "Surface", "Radar", "Sunburst", 
                            "Histogram", "Box & Whisker", "Waterfall", "Funnel", "Combo")
    validate(
      need(!(jenis %in% unsupported_charts), 
           paste("Maaf, jenis grafik", jenis, "membutuhkan format metrik khusus. Silakan diskusikan opsi visualisasi lain untuk data ini."))
    )
    
    judul_grafik <- if(is.null(input$judul_plot) || input$judul_plot == "") {
      paste("Hasil Survei:\n", kolom)
    } else {
      input$judul_plot
    }
    
    # data cleaning
    df_clean <- df %>%
      select(all_of(kolom)) %>%
      filter(.data[[kolom]] != "" & !is.na(.data[[kolom]])) %>%
      separate_rows(all_of(kolom), sep = ",") %>%
      mutate(!!kolom := str_trim(.data[[kolom]])) %>%
      count(.data[[kolom]], name = "Frekuensi") %>%
      arrange(desc(Frekuensi)) %>%
      slice_head(n = input$top_n) %>%
      mutate(Persentase = Frekuensi / sum(Frekuensi) * 100,
             Label_Persen = paste0(round(Persentase, 1), "%"),
             Label_Lengkap = paste0(Frekuensi, " (", Label_Persen, ")"))
    
    # color palette
    if (opsi_palet == "pelangi") {
      warna_custom <- c("#DE2C2D", "#F37121", "#FAD201", "#8CC63F", "#1389CA", "#662D91")
    } else {
      warna_custom <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    }
    
    palet_dinamis <- colorRampPalette(warna_custom)(nrow(df_clean))
    names(palet_dinamis) <- df_clean[[kolom]]
    
    # warna judul
    tema_teks <- theme(
      text = element_text(family = "serif"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "serif", color = "black"),
      axis.title = element_text(family = "serif", face = "bold"),
      axis.text = element_text(family = "serif", size = 11)
    )
    
    # geom visualisasi
    if (jenis == "Column") {
      p <- ggplot(df_clean, aes(x = reorder(.data[[kolom]], -Frekuensi), y = Frekuensi, fill = .data[[kolom]])) +
        geom_col(color = "white") +
        scale_fill_manual(values = palet_dinamis) +
        geom_text(aes(label = Label_Lengkap), vjust = -0.5, size = 4.5, family = "serif") +
        labs(title = judul_grafik, x = "Kategori Jawaban", y = "Jumlah (Frekuensi)") +
        theme_minimal() + tema_teks +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
      
    } else if (jenis == "Bar") {
      p <- ggplot(df_clean, aes(x = reorder(.data[[kolom]], Frekuensi), y = Frekuensi, fill = .data[[kolom]])) +
        geom_col(color = "white") +
        scale_fill_manual(values = palet_dinamis) +
        geom_text(aes(label = Label_Lengkap), hjust = -0.1, size = 4.5, family = "serif") +
        coord_flip() + labs(title = judul_grafik, x = "Kategori Jawaban", y = "Jumlah (Frekuensi)") +
        theme_minimal() + tema_teks + theme(legend.position = "none") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
      
    } else if (jenis == "Pie") {
      p <- ggplot(df_clean, aes(x = "", y = Frekuensi, fill = .data[[kolom]])) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        scale_fill_manual(values = palet_dinamis) +
        geom_text(aes(label = Label_Persen), position = position_stack(vjust = 0.5), 
                  color = "black", fontface = "bold", size = 5, family = "serif") +
        labs(title = judul_grafik, fill = "Kategori Jawaban") + theme_void() + 
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "serif", color = "black"),
              legend.text = element_text(family = "serif", size = 11),
              legend.title = element_text(family = "serif", face = "bold", size = 12))
      
    } else if (jenis == "Treemap") {
      p <- ggplot(df_clean, aes(area = Frekuensi, fill = .data[[kolom]], label = paste0(.data[[kolom]], "\n", Label_Persen))) +
        geom_treemap(color = "white") +
        scale_fill_manual(values = palet_dinamis) +
        geom_treemap_text(family = "serif", color = "black", place = "centre", grow = FALSE, size = 14, fontface = "bold") +
        labs(title = judul_grafik) + theme_minimal() + tema_teks + theme(legend.position = "none")
      
    } else {
      p_base <- ggplot(df_clean, aes(x = reorder(.data[[kolom]], -Frekuensi), y = Frekuensi)) +
        labs(title = judul_grafik, x = "Kategori Jawaban", y = "Jumlah (Frekuensi)") +
        theme_minimal() + tema_teks
      
      if (opsi_palet == "pelangi") {
        c_line1 <- "#1389CA"; c_point1 <- "#DE2C2D"
        c_area2 <- "#FAD201"; c_line2 <- "#F37121"; c_point2 <- "#662D91"
        c_scat <- "#8CC63F"
      } else {
        c_line1 <- "#0072B2"; c_point1 <- "#D55E00"
        c_area2 <- "#56B4E9"; c_line2 <- "#0072B2"; c_point2 <- "#E69F00"
        c_scat <- "#009E73"
      }
      
      if (jenis == "Line") {
        p <- p_base + geom_line(aes(group = 1), color = c_line1, linewidth = 1.2) + 
          geom_point(color = c_point1, size = 4) + 
          geom_text(aes(label = Label_Persen), vjust = -1, size = 4.5, family = "serif") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
        
      } else if (jenis == "Area") {
        p <- p_base + geom_area(aes(group = 1), fill = c_area2, alpha = 0.7) + 
          geom_line(aes(group = 1), color = c_line2, linewidth = 1) + 
          geom_point(color = c_point2, size = 3) + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else if (jenis == "X Y (Scatter)") {
        p <- p_base + geom_point(color = c_scat, size = 6, alpha = 0.9) + 
          geom_text(aes(label = Label_Persen), vjust = -1.5, size = 4.5, family = "serif") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
      }
    }
    return(p)
  })
  
  output$plot_hasil <- renderPlot({ plot_obj() })
  
  output$download_plot <- downloadHandler(
    filename = function() { paste("Visualisasi-Data-", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      ggsave(file, plot = plot_obj(), width = 10, height = 7, dpi = 300, bg = "white")
    }
  )
}



# run app
shinyApp(ui = ui, server = server)