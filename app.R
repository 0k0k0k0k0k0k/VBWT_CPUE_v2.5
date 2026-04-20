# VBWT CPUE v2 FINAL

library(shiny)
library(dplyr)
library(readr)
library(DT)
library(leaflet)
library(leaflet.extras)
library(data.table)
library(ggplot2)

base_dir <- "."
cache_dir_all <- file.path(base_dir, "data_processed/caches")
cache_dir_exclude_self <- file.path(base_dir, "data_processed/caches_exclude_self")

load_app_data <- function(cache_dir) {
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(cache_files) == 0) {
    stop("No cache files found in: ", cache_dir)
  }
  
  dat <- lapply(cache_files, readRDS) %>%
    bind_rows() %>%
    mutate(
      Latitude = suppressWarnings(as.numeric(Latitude)),
      Longitude = suppressWarnings(as.numeric(Longitude)),
      TotalChecklists = suppressWarnings(as.numeric(TotalChecklists)),
      TotalSpecies = suppressWarnings(as.numeric(TotalSpecies)),
      Checklist_CPUE = suppressWarnings(as.numeric(Checklist_CPUE))
    ) %>%
    filter(
      !is.na(Latitude),
      !is.na(Longitude)
    ) %>%
    distinct(HotspotID, .keep_all = TRUE)
  
  cpue_vals <- dat$Checklist_CPUE[is.finite(dat$Checklist_CPUE)]
  
  if (length(cpue_vals) == 0) {
    dat <- dat %>%
      mutate(
        Normalized_CPUE = 0
      )
  } else {
    cpue_q05 <- as.numeric(stats::quantile(cpue_vals, probs = 0.05, na.rm = TRUE))
    cpue_q95 <- as.numeric(stats::quantile(cpue_vals, probs = 0.95, na.rm = TRUE))
    
    if (!is.finite(cpue_q05) || !is.finite(cpue_q95) || cpue_q95 <= cpue_q05) {
      dat <- dat %>%
        mutate(
          Normalized_CPUE = 0
        )
    } else {
      dat <- dat %>%
        mutate(
          Normalized_CPUE = pmin(
            pmax((Checklist_CPUE - cpue_q05) / (cpue_q95 - cpue_q05), 0),
            1
          )
        )
    }
  }
  
  checklist_range <- range(dat$TotalChecklists, na.rm = TRUE)
  
  if (!all(is.finite(checklist_range)) || checklist_range[1] == checklist_range[2]) {
    dat <- dat %>%
      mutate(
        norm_checklists = 0
      )
  } else {
    dat <- dat %>%
      mutate(
        norm_checklists = scales::rescale(TotalChecklists, to = c(0, 1), from = checklist_range)
      )
  }
  
  dat %>%
    mutate(
      gem_score = Normalized_CPUE * (1 - norm_checklists),
      category = case_when(
        Normalized_CPUE >= 0.75 & norm_checklists >= 0.5 ~ "Elite",
        Normalized_CPUE < 0.25 & norm_checklists >= 0.5 ~ "Overbirded",
        Normalized_CPUE >= 0.75 & norm_checklists < 0.5 ~ "Underbirded Gem",
        TRUE ~ "Low Value"
      )
    ) %>%
    arrange(desc(Checklist_CPUE), SiteName)
}

ui <- fluidPage(
  titlePanel("VBWT Checklist CPUE Heat Map"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "checklist_source",
        "Checklist Source:",
        choices = c("All Checklists", "Exclude My Checklists"),
        selected = "All Checklists"
      ),
      
      radioButtons(
        "cpue_mode",
        "Map Coloring:",
        choices = c("Normalized CPUE", "Raw CPUE"),
        selected = "Normalized CPUE"
      ),
      
      sliderInput(
        "min_checklists",
        "Minimum Checklists",
        min = 0,
        max = 2000,
        value = 0,
        step = 1
      ),
      
      sliderInput(
        "min_cpue",
        "Minimum Avg. Species per Checklist",
        min = 0,
        max = 50,
        value = 0,
        step = 1
      ),
      
      br(),
      
      h4("Legend"),
      htmlOutput("legend_ui"),
      
      br(),
      downloadButton("download_csv", "Download CSV")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map", height = 700)),
        tabPanel("Table", DTOutput("table")),
        tabPanel(
          "Analysis",
          h4("Effort vs Avg. Species per Checklist"),
          plotOutput("scatter", height = 350),
          br(),
          
          h4("Top Hidden Gems"),
          DTOutput("gems_table"),
          br(),
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected_cache_dir <- reactive({
    if (input$checklist_source == "Exclude My Checklists") {
      cache_dir_exclude_self
    } else {
      cache_dir_all
    }
  })
  
  raw_app_data <- reactive({
    load_app_data(selected_cache_dir())
  })
  
  app_data <- reactive({
    raw_app_data() %>%
      filter(
        TotalChecklists >= input$min_checklists,
        Checklist_CPUE >= input$min_cpue
      )
  })
  
  output$legend_ui <- renderUI({
    dat <- app_data()
    
    if (nrow(dat) == 0) {
      return(tagList(
        tags$div("No values available for current filters.")
      ))
    }
    
    if (input$cpue_mode == "Raw CPUE") {
      values <- dat$Checklist_CPUE
      title <- "Avg Species per Checklist (Raw)"
      labels <- c("Lowest", "Low", "Moderate", "High", "Highest")
      num_digits <- 3
    } else {
      values <- dat$Normalized_CPUE
      title <- "Avg Species per Checklist (Normalized)"
      labels <- c("Lowest", "Low", "Moderate", "High", "Highest")
      num_digits <- 2
    }
    
    values <- values[is.finite(values)]
    
    if (length(values) == 0) {
      return(tagList(
        tags$b(title),
        tags$br(),
        tags$div("No values available.")
      ))
    }
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = range(values, na.rm = TRUE),
      na.color = "#d9d9d9"
    )
    
    probs <- seq(0, 1, length.out = 6)
    cuts <- as.numeric(stats::quantile(values, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
    
    for (i in 2:length(cuts)) {
      if (cuts[i] <= cuts[i - 1]) {
        cuts[i] <- cuts[i - 1] + .Machine$double.eps
      }
    }
    
    mids <- (cuts[-length(cuts)] + cuts[-1]) / 2
    colors <- pal(mids)
    
    range_labels <- paste0(
      format(round(cuts[-length(cuts)], num_digits), nsmall = num_digits),
      " to ",
      format(round(cuts[-1], num_digits), nsmall = num_digits)
    )
    
    tagList(
      tags$div(
        style = "font-size: 13px; line-height: 1.35;",
        tags$b(title),
        tags$div(
          style = "margin-top: 8px; margin-bottom: 8px;",
          lapply(seq_along(labels), function(i) {
            tags$div(
              style = "display: flex; align-items: center; margin-bottom: 6px;",
              tags$div(
                style = paste0(
                  "width: 18px; height: 14px; background:", colors[i],
                  "; border: 1px solid #bbb; margin-right: 8px; flex-shrink: 0;"
                )
              ),
              tags$div(
                tags$div(
                  style = "font-weight: 600;",
                  labels[i]
                ),
                tags$div(
                  style = "font-size: 12px; color: #555;",
                  range_labels[i]
                )
              )
            )
          })
        ),
        tags$div(
          style = "font-size: 12px; color: #444; padding-top: 6px; border-top: 1px solid #ddd;",
          HTML(
            "Circle color = avg. species per checklist<br>Circle size = total checklists"
          )
        )
      )
    )
  })
  
  output$table <- renderDT({
    dat <- app_data() %>%
      transmute(
        SiteName,
        HotspotID,
        TotalChecklists,
        TotalSpecies,
        Checklist_CPUE = round(Checklist_CPUE, 4),
        Normalized_CPUE = round(Normalized_CPUE, 4),
        Hidden_Gem_Score = round(gem_score, 4),
        Category = category,
        Latitude,
        Longitude
      ) %>%
      arrange(desc(Checklist_CPUE), SiteName)
    
    datatable(
      dat,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(4, "desc"))
      ),
      rownames = FALSE
    )
  })
  
  output$map <- renderLeaflet({
    dat <- app_data()
    req(nrow(dat) > 0)
    
    if (input$cpue_mode == "Raw CPUE") {
      values <- dat$Checklist_CPUE
    } else {
      values <- dat$Normalized_CPUE
    }
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = range(values, na.rm = TRUE),
      na.color = "#d9d9d9"
    )
    
    checklist_range <- range(dat$TotalChecklists, na.rm = TRUE)
    
    if (!all(is.finite(checklist_range)) || checklist_range[1] == checklist_range[2]) {
      radius_vals <- rep(8, nrow(dat))
    } else {
      radius_vals <- scales::rescale(
        dat$TotalChecklists,
        to = c(4, 14),
        from = checklist_range
      )
    }
    
    leaflet(dat) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = radius_vals,
        stroke = TRUE,
        weight = 1,
        color = "black",
        fillColor = pal(values),
        fillOpacity = 0.75,
        popup = ~paste0(
          "<b>Site:</b> ", SiteName,
          "<br><b>Normalized CPUE:</b> ", ifelse(is.na(Normalized_CPUE), "NA", format(round(Normalized_CPUE, 4), nsmall = 4)),
          "<br><b>Hidden Gem Score:</b> ", ifelse(is.na(gem_score), "NA", format(round(gem_score, 4), nsmall = 4)),
          "<br><b>Category:</b> ", category
          "<br><b>Hotspot ID:</b> ", HotspotID,
          "<br><b>Total Checklists:</b> ", format(TotalChecklists, big.mark = ","),
          "<br><b>Total Species:</b> ", ifelse(is.na(TotalSpecies), "NA", format(TotalSpecies, big.mark = ",")),
          "<br><b>Avg. Species per Checklist:</b> ", ifelse(is.na(Checklist_CPUE), "NA", format(round(Checklist_CPUE, 4), nsmall = 4)),
    
        )
      ) %>%
      fitBounds(
        lng1 = min(dat$Longitude, na.rm = TRUE),
        lat1 = min(dat$Latitude, na.rm = TRUE),
        lng2 = max(dat$Longitude, na.rm = TRUE),
        lat2 = max(dat$Latitude, na.rm = TRUE)
      )
  })
  
  output$scatter <- renderPlot({
    dat <- app_data() %>%
      filter(!is.na(TotalChecklists), TotalChecklists > 0)
    
    req(nrow(dat) > 0)
    
    ggplot(dat, aes(x = TotalChecklists, y = Checklist_CPUE, color = category)) +
      geom_point(alpha = 0.75, size = 2.5) +
      scale_x_log10() +
      labs(
        x = "Total Checklists (log scale)",
        y = "Avg. Species per Checklist",
        color = "Category"
      ) +
      theme_minimal()
  })
  
  output$gems_table <- renderDT({
    dat <- app_data() %>%
      arrange(desc(gem_score), desc(Checklist_CPUE), SiteName) %>%
      transmute(
        SiteName,
        HotspotID,
        TotalChecklists,
        TotalSpecies,
        Checklist_CPUE = round(Checklist_CPUE, 4),
        Hidden_Gem_Score = round(gem_score, 4),
        Category = category
      ) %>%
      head(20)
    
    datatable(
      dat,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  
  output$download_csv <- downloadHandler(
    filename = function() {
      if (input$checklist_source == "Exclude My Checklists") {
        paste0("vbwt_cpue_exclude_self_", Sys.Date(), ".csv")
      } else {
        paste0("vbwt_cpue_statewide_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      fwrite(app_data(), file)
    }
  )
}

shinyApp(ui, server)