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
  id = "halaman_utama",
  theme = bs_theme(bootswatch = "flatly", primary = "#1389CA"),
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #DE2C2D !important;
        font-size: 18px;
        font-weight: bold;
        text-align: center;
        padding-top: 50px;
      }
      body, .well, .control-label, span, div, h1, h2, h3, h4, h5, h6, .form-control {
        transition: background-color 0.4s ease-in-out, color 0.4s ease-in-out, border-color 0.4s ease-in-out !important;
      }
    ")),
    uiOutput("dynamic_css") 
  ),
  
  titlePanel(
    tags$div(
      icon("chart-pie", style = "color: #DE2C2D;"), 
      tags$strong("Punya ANDAT")
    )
  ),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      style = "box-shadow: 0px 4px 15px rgba(0,0,0,0.05); border-radius: 8px; border: none;",
      
      fileInput(
        "file_data",
        tags$b("1. Unggah File Data (.csv)"),
        accept = ".csv",
        buttonLabel = "Telusuri",
        placeholder = "Pilih file"
      ),
      
      selectInput(
        "kolom_survei",
        tags$b("2. Pilih Pertanyaan (Kolom)"),
        choices = c("Menunggu file diunggah" = "")
      ),
      
      selectInput(
        "jenis_chart",
        tags$b("3. Pilih Jenis Chart"),
        choices = c("Bar", "Column", "Pie", "Treemap", "Histogram", "Box Plot", "Line",  "Area", "Scatter Plot")
      ),
      
      sliderInput(
        "top_n", tags$b("4. Tampilkan N Jawaban Teratas"), 
        min = 3, max = 20, value = 10
      ),
      
      textInput(
        "judul_plot",
        tags$b("5. Ketik Judul Grafik"),
        placeholder = "Kosongkan untuk pakai judul bawaan"
      ),
      
      radioButtons(
        "opsi_warna",
        tags$b("6. Pilih Palet Warna"),
        choices = c("Warna-warni (Pelangi)" = "pelangi",
                    "Palet Inklusif (Aksesibilitas Visual)" = "colorblind"),
        selected = "pelangi"
      ),
      
      br(),
      downloadButton(
        "download_plot", "Unduh Grafik (PNG)", 
        class = "btn-success", 
        style = "width: 100%; font-weight: bold; font-size: 15px; border-radius: 6px; background-color: #8CC63F; border-color: #8CC63F;"
      ),
      
      hr(),
      helpText(
        icon("lightbulb", style = "color: #FAD201;"), 
        "Tips: Klik kanan pada gambar lalu pilih 'Copy Image' untuk menyalin grafik."
      ),
      
      # tombol dark mode
      hr(),
      checkboxInput(
        "dark_mode",
        tags$b("🌙 Dark Mode"),
        value = FALSE
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
            tags$h3("Ruang Kerja Kosong", style = "font-weight: bold; color: #25374C; margin-top: 20px;"),
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
  
  # UI: light/dark mode
  observe({
    if (isTRUE(input$dark_mode)) {
      session$setCurrentTheme(bs_theme(bootswatch = "darkly", primary = "#1389CA"))
    } else {
      session$setCurrentTheme(bs_theme(bootswatch = "flatly", primary = "#1389CA"))
    }
  })
  
  output$dynamic_css <- renderUI({
    if (isTRUE(input$dark_mode)) {
      tags$style(HTML("
        .control-label, b, .help-block, .radio, .checkbox { color: #ffffff !important;}
        .form-control {background-color: #2b2b2b !important; color: #ffffff !important; border: 1px solid #555 !important;}
        .selectize-input, .selectize-input.full {background-color: #2b2b2b !important; border: 1px solid #555 !important;}
        .selectize-input > input, .selectize-input .item { color: #ffffff !important; text-shadow: none !important; }
        .selectize-dropdown {background-color: #2b2b2b !important; color: #ffffff !important; border: 1px solid #555 !important;}
        .selectize-dropdown .active {background-color: #1389CA !important; color: #ffffff !important;}
      "))
    } else {
      tags$style(HTML(""))
    }
  })
  
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
    
    
    judul_grafik <- if(is.null(input$judul_plot) || input$judul_plot == "") {
      paste("Hasil Survei:\n", kolom)
    } else {
      input$judul_plot
    }
    
    # canvas view
    warna_teks <- "#000000"
    warna_bg   <- "#ffffff"
    warna_grid <- "#e5e5e5"
    warna_tunggal <- ifelse(opsi_palet == "pelangi", "#EEA903", "#4A605E")
    
    # tema: typographic golden ratio (15 : 24 : 39)
    tema_teks <- theme(
      text = element_text(family = "sans", color = warna_teks),
      plot.title = element_text(size = 39, face = "bold", hjust = 0.5, family = "sans", color = warna_teks, margin = margin(b = 20)),
      axis.title.x = element_text(family = "sans", face = "bold", size = 24, color = warna_teks, margin = margin(t = 15)),
      axis.title.y = element_text(family = "sans", face = "bold", size = 24, color = warna_teks, margin = margin(r = 15)),
      axis.text = element_text(family = "sans", size = 15, color = warna_teks),
      plot.background = element_rect(fill = warna_bg, color = NA),
      panel.background = element_rect(fill = warna_bg, color = NA),
      panel.grid.major = element_line(color = warna_grid),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = warna_bg, color = NA),
      legend.text = element_text(family = "sans", size = 15, color = warna_teks),
      legend.title = element_text(family = "sans", face = "bold", size = 24, color = warna_teks),
      plot.margin = margin(t = 20, r = 30, b = 20, l = 20)
    )
    
    # geom visualisasi: histogram, box plot
    if (jenis %in% c("Histogram", "Box Plot")) {
      
      validate(
        need(is.numeric(df[[kolom]]), "⚠️ Jenis grafik ini membutuhkan data numerik.")
      )
      
      df_num <- df |> 
        filter(!is.na(.data[[kolom]]))
      
      if (jenis == "Histogram") {
        p <- ggplot(
          df_num,
          aes(x = .data[[kolom]])
        ) +
          geom_histogram(
            fill = warna_tunggal, 
            color = "white", 
            bins = 15
          ) +
          labs(
            title = judul_grafik, 
            x = "Nilai / Rentang Angka", 
            y = "Frekuensi"
          ) +
          theme_minimal() + 
          tema_teks
        
      } else if (jenis == "Box Plot") {
        p <- ggplot(
          df_num, 
          aes(y = .data[[kolom]], x = "")
        ) +
          geom_boxplot(
            fill = warna_tunggal, 
            color = warna_teks, 
            width = 0.4
          ) +
          labs(
            title = judul_grafik, 
            x = "Distribusi Keseluruhan", 
            y = "Nilai"
          ) +
          theme_minimal() + 
          tema_teks
      }
      return(p)
      
    } else {
      
      # geom visualisasi: sisanya
      
      df_clean <- df |> 
        select(all_of(kolom)) |> 
        filter(.data[[kolom]] != "" & !is.na(.data[[kolom]])) |> 
        separate_rows(all_of(kolom), sep = ",") |> 
        mutate(!!kolom := str_trim(.data[[kolom]])) |> 
        count(.data[[kolom]], name = "Frekuensi") |> 
        arrange(desc(Frekuensi)) |> 
        slice_head(n = input$top_n) |> 
        mutate(
          Persentase = Frekuensi / sum(Frekuensi) * 100,
          Label_Persen = paste0(round(Persentase, 1), "%"),
          Label_Lengkap = paste0(Frekuensi, " (", Label_Persen, ")")
        )
      
      if (opsi_palet == "pelangi") {
        warna_custom <- c("#DE2C2D", "#F37121", "#FAD201", "#8CC63F", "#1389CA", "#662D91")
      } else {
        warna_custom <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      }
      
      palet_dinamis <- colorRampPalette(warna_custom)(nrow(df_clean))
      names(palet_dinamis) <- df_clean[[kolom]]
      
      if (jenis == "Column") {
        p <- ggplot(
          df_clean,
          aes(x = reorder(.data[[kolom]], -Frekuensi), y = Frekuensi)
        ) +
          geom_col(
            fill = warna_tunggal,
            color = "white",
            width = 0.75
          ) +
          geom_text(
            aes(label = Label_Lengkap),
            vjust = -0.5,
            size = 6.5,
            family = "sans",
            color = warna_teks
          ) +
          labs(
            title = judul_grafik,
            x = "Kategori Jawaban",
            y = "Jumlah (Frekuensi)"
          ) +
          theme_minimal() +
          tema_teks +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
          ) +
          scale_y_continuous(
            expand = expansion(mult = c(0, 0.2))
          )
        
      } else if (jenis == "Bar") {
        p <- ggplot(
          df_clean,
          aes(x = reorder(.data[[kolom]], Frekuensi), y = Frekuensi)
        ) +
          geom_col(
            fill = warna_tunggal,
            color = "white",
            width = 0.75
          ) +
          geom_text(
            aes(label = Label_Lengkap),
            hjust = -0.1,
            size = 6.5,
            family = "sans",
            color = warna_teks
          ) +
          coord_flip() + 
          labs(
            title = judul_grafik,
            x = "Kategori Jawaban",
            y = "Jumlah (Frekuensi)"
          ) +
          theme_minimal() +
          tema_teks + 
          scale_y_continuous(
            expand = expansion(mult = c(0, 0.2))
          )
        
      } else if (jenis == "Pie") {
        p <- ggplot(
          df_clean,
          aes(x = "", y = Frekuensi, fill = .data[[kolom]])
        ) +
          geom_bar(
            stat = "identity",
            width = 1,
            color = "white"
          ) +
          coord_polar(
            "y", 
            start = 0
          ) +
          scale_fill_manual(values = palet_dinamis) +
          geom_text(
            aes(label = Label_Persen),
            position = position_stack(vjust = 0.5), 
            color = "#000",
            size = 6.5,
            family = "sans"
          ) + 
          labs(
            title = judul_grafik,
            fill = "Kategori Jawaban"
          ) + theme_void() + 
          tema_teks +
          theme(
            axis.text = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = margin(t = 20, r = 30, b = 20, l = 20)
          )
        
      } else if (jenis == "Treemap") {
        p <- ggplot(
          df_clean,
          aes(area = Frekuensi, fill = .data[[kolom]], label = paste0(.data[[kolom]], "\n", Label_Persen))
        ) +
          geom_treemap(
            color = "white"
          ) +
          scale_fill_manual(
            values = palet_dinamis
          ) +
          geom_treemap_text(
            family = "sans",
            color = "#ffffff",
            place = "centre",
            grow = FALSE,
            size = 24
          ) + 
          labs(
            title = judul_grafik
          ) + 
          theme_minimal() + 
          tema_teks + 
          theme(
            legend.position = "none"
          )
        
      } else {
        p_base <- ggplot(
          df_clean,
          aes(x = reorder(.data[[kolom]], -Frekuensi), y = Frekuensi)
        ) +
          labs(
            title = judul_grafik,
            x = "Kategori Jawaban",
            y = "Jumlah (Frekuensi)"
          ) +
          theme_minimal() + 
          tema_teks
        
        if (opsi_palet == "pelangi") {
          c_line1 <- "#EEA903";
          c_point1 <- "#4A605E";
          c_area2 <- "#EEC963"; 
          c_line2 <- "#EEA903"; 
          c_point2 <- "#4A605E";
          c_scat <- "#4A605E"
        } else {
          c_line1 <- "#0072B2";
          c_point1 <- "#D55E00";
          c_area2 <- "#56B4E9"; 
          c_line2 <- "#0072B2"; 
          c_point2 <- "#E69F00"; 
          c_scat <- "#009E73"
        }
        
        if (jenis == "Line") {
          p <- p_base + 
            geom_line(
              aes(group = 1), 
              color = c_line1, 
              linewidth = 1.2
            ) + 
            geom_point(
              color = c_point1, 
              size = 4
            ) + 
            geom_text(
              aes(label = Label_Persen), 
              vjust = -1.2, 
              size = 6.5, 
              fontface = "bold", 
              family = "sans", 
              color = warna_teks
            ) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1)
            ) + 
            scale_y_continuous(
              expand = expansion(mult = c(0, 0.2))
            )
          
        } else if (jenis == "Area") {
          p <- p_base + 
            geom_area(
              aes(group = 1), 
              fill = c_area2, 
              alpha = 0.7
            ) + 
            geom_line(
              aes(group = 1), 
              color = c_line2, 
              linewidth = 1
            ) + 
            geom_point(
              color = c_point2, 
              size = 3
            ) + 
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1)
            )
          
        } else if (jenis == "Scatter Plot") {
          p <- p_base + 
            geom_point(
              color = c_scat, 
              size = 6, 
              alpha = 0.9
            ) + 
            geom_text(
              aes(label = Label_Persen), 
              vjust = -1.5, 
              size = 6.5, 
              fontface = "bold", 
              family = "sans", 
              color = warna_teks
            ) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1)
            ) + 
            scale_y_continuous(
              expand = expansion(mult = c(0, 0.2))
            )
        }
      }
      return(p)
    }
  })
  
  output$plot_hasil <- renderPlot({ plot_obj() })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("Visualisasi-Data-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_obj(), width = 10, height = 7, dpi = 300, bg = "#ffffff")
    }
  )
}


# run app
shinyApp(ui = ui, server = server)