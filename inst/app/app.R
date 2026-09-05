# Palettome interactive Shiny UI.
#
# Launched only via palettome::launch_palettome_ui(), which has already
# verified that shiny/plotly/colourpicker/shinyjs are installed and has
# populated palettome:::.pt_env with the data this app reads. This file is
# never sourced as part of loading the palettome package itself.

if (!"palettome" %in% loadedNamespaces()) {
  stop("This app must be launched via palettome::launch_palettome_ui(), not run directly.")
}

library(shiny)
library(plotly)

pt_env <- get(".pt_env", envir = asNamespace("palettome"))

drag_drop_js <- "
document.addEventListener('dragstart', function(e) {
  if (e.target.classList.contains('pt-chip')) {
    e.dataTransfer.setData('text/plain', e.target.dataset.cluster);
    e.dataTransfer.effectAllowed = 'move';
  }
});
document.addEventListener('dragover', function(e) {
  if (e.target.closest('.pt-family-bin')) e.preventDefault();
});
document.addEventListener('drop', function(e) {
  var bin = e.target.closest('.pt-family-bin');
  if (bin) {
    e.preventDefault();
    var cluster = e.dataTransfer.getData('text/plain');
    var family = bin.dataset.family;
    Shiny.setInputValue('cluster_drop', {cluster: cluster, family: family}, {priority: 'event'});
  }
});
"

pt_css <- "
.pt-family-bin { border: 1px solid #ccc; border-radius: 6px; margin-bottom: 8px; padding: 6px; min-height: 44px; }
.pt-family-header { font-weight: bold; padding: 4px 8px; border-radius: 4px; margin-bottom: 4px; cursor: pointer; color: #fff; text-shadow: 0 0 2px rgba(0,0,0,.6); }
.pt-chip { display: inline-block; padding: 3px 8px; margin: 2px; border-radius: 4px; cursor: grab; color: #fff; text-shadow: 0 0 2px rgba(0,0,0,.6); font-size: 12px; }
"

n_clusters <- length(unique(pt_env$assignment$cluster))
init_n_fam <- length(unique(pt_env$assignment$family_id))

ui <- fluidPage(
  tags$head(tags$style(HTML(pt_css)), tags$script(HTML(drag_drop_js))),
  titlePanel("Palettome"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mode", "Mode", c("harmonious", "contrast"),
        selected = pt_env$session$params$mode
      ),
      numericInput("seed", "Seed", value = pt_env$session$params$seed, min = 1, step = 1),
      sliderInput("lightness_range", "Lightness range",
        min = 0, max = 100, value = pt_env$session$params$lightness_range
      ),
      sliderInput("chroma_range", "Chroma range",
        min = 0, max = 100, value = pt_env$session$params$chroma_range
      ),
      selectInput("cvd", "Colorblind-safe preview", c("none", "deutan", "protan", "tritan")),
      hr(),
      sliderInput("k", "Number of families (re-detect)",
        min = 1, max = max(1, n_clusters - 1), value = init_n_fam, step = 1
      ),
      actionButton("redetect", "Re-detect families from k"),
      tags$hr(),
      actionButton("regen", "Regenerate colors", class = "btn-primary"),
      actionButton("reset", "Reset manual overrides"),
      tags$hr(),
      downloadButton("dl_json", "Export JSON"),
      downloadButton("dl_csv", "Export CSV"),
      downloadButton("dl_png", "Export PNG (full render)"),
      tags$hr(),
      fileInput("load_session", "Load session (JSON)", accept = ".json"),
      width = 3
    ),
    mainPanel(
      plotlyOutput("scatter", height = "480px"),
      fluidRow(
        column(6, h4("Family dendrogram"), plotOutput("dendrogram", height = "280px")),
        column(6, h4("Families (drag chips to regroup, click to recolor)"), uiOutput("family_bins"))
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    assignment = pt_env$assignment,
    session = pt_env$session,
    dendro = pt_env$dendro,
    selected = NULL
  )

  regenerate <- function() {
    rv$session <- palettome::generate_palette(
      rv$assignment,
      session = rv$session, mode = input$mode, seed = input$seed,
      lightness_range = input$lightness_range, chroma_range = input$chroma_range,
      cvd = input$cvd
    )
  }

  preview_data <- reactive({
    palettome::downsample_stratified(pt_env$pdata, max_n = pt_env$preview_max_n)
  })

  output$scatter <- renderPlotly({
    pd <- preview_data()
    color_map <- stats::setNames(rv$session$clusters$color, rv$session$clusters$cluster)
    if (!identical(input$cvd, "none")) color_map <- palettome::simulate_cvd(color_map, type = input$cvd)
    df <- data.frame(
      x = pd$cells[[pd$coord_cols[1]]], y = pd$cells[[pd$coord_cols[2]]],
      cluster = pd$cells$cluster, stringsAsFactors = FALSE
    )
    plotly::plot_ly(
      df,
      x = ~x, y = ~y, color = ~cluster, colors = color_map,
      type = "scattergl", mode = "markers",
      marker = list(size = 4, opacity = 0.7)
    ) %>%
      plotly::layout(
        xaxis = list(title = pd$coord_cols[1]), yaxis = list(title = pd$coord_cols[2]),
        legend = list(itemsizing = "constant")
      )
  })

  output$dendrogram <- renderPlot({
    if (is.null(rv$dendro)) {
      plot.new()
      text(0.5, 0.5, "Families were supplied explicitly\n(no dendrogram to show)")
      return(invisible(NULL))
    }
    palettome::plot_dendrogram(rv$dendro, k = input$k)
  })

  output$family_bins <- renderUI({
    fam_ids <- unique(rv$assignment$family_id)
    fam_color <- stats::setNames(rv$session$families$color, rv$session$families$family_id)
    cluster_color <- stats::setNames(rv$session$clusters$color, rv$session$clusters$cluster)

    bins <- lapply(fam_ids, function(fid) {
      members <- rv$assignment$cluster[rv$assignment$family_id == fid]
      chips <- lapply(members, function(cl) {
        tags$div(
          class = "pt-chip", draggable = "true",
          `data-cluster` = cl,
          style = sprintf("background:%s;", cluster_color[[cl]]),
          onclick = sprintf("Shiny.setInputValue('chip_click', '%s', {priority: 'event'})", cl),
          cl
        )
      })
      tags$div(
        class = "pt-family-bin", `data-family` = fid,
        tags$div(
          class = "pt-family-header", style = sprintf("background:%s;", fam_color[[fid]]),
          onclick = sprintf("Shiny.setInputValue('family_click', '%s', {priority: 'event'})", fid),
          fid
        ),
        chips
      )
    })
    tagList(bins)
  })

  observeEvent(input$chip_click, {
    rv$selected <- list(type = "cluster", id = input$chip_click)
    current <- rv$session$clusters$color[rv$session$clusters$cluster == input$chip_click]
    showModal(modalDialog(
      title = paste("Recolor cluster", input$chip_click),
      colourpicker::colourInput("picked_color", "Color", value = current),
      footer = tagList(modalButton("Cancel"), actionButton("apply_color", "Apply"))
    ))
  })

  observeEvent(input$family_click, {
    rv$selected <- list(type = "family", id = input$family_click)
    current <- rv$session$families$color[rv$session$families$family_id == input$family_click]
    showModal(modalDialog(
      title = paste("Recolor family", input$family_click),
      colourpicker::colourInput("picked_color", "Color", value = current),
      footer = tagList(modalButton("Cancel"), actionButton("apply_color", "Apply"))
    ))
  })

  observeEvent(input$apply_color, {
    req(rv$selected)
    if (rv$selected$type == "cluster") {
      rv$session <- palettome::set_manual_color(rv$session, cluster = rv$selected$id, color = input$picked_color)
    } else {
      rv$session <- palettome::set_manual_color(rv$session, family = rv$selected$id, color = input$picked_color)
    }
    removeModal()
  })

  observeEvent(input$cluster_drop, {
    drop <- input$cluster_drop
    rv$assignment$family_id[rv$assignment$cluster == drop$cluster] <- drop$family
    regenerate()
  })

  observeEvent(input$redetect, {
    req(rv$dendro)
    rv$assignment <- palettome::cut_families(rv$dendro, k = input$k)
    regenerate()
  })

  observeEvent(input$regen, regenerate())

  observeEvent(input$reset, {
    rv$session <- palettome::reset_overrides(rv$session)
    regenerate()
  })

  observeEvent(input$load_session, {
    req(input$load_session)
    rv$session <- palettome::import_palette_json(input$load_session$datapath)
    rv$assignment <- rv$session$clusters[, c("cluster", "family_id")]
  })

  output$dl_json <- downloadHandler(
    filename = function() "palettome_session.json",
    content = function(file) palettome::export_palette_json(rv$session, file)
  )
  output$dl_csv <- downloadHandler(
    filename = function() "palettome_clusters.csv",
    content = function(file) palettome::export_palette_csv(rv$session, file)
  )
  output$dl_png <- downloadHandler(
    filename = function() "palettome_plot.png",
    content = function(file) {
      grDevices::png(file, width = 1600, height = 1200, res = 150)
      on.exit(grDevices::dev.off())
      print(palettome::plot_palette_static(pt_env$pdata, rv$session, cvd = input$cvd))
    }
  )
}

shinyApp(ui, server)
