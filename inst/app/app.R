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

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

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
    Shiny.setInputValue('cluster_drop',
      {cluster: e.dataTransfer.getData('text/plain'), family: bin.dataset.family},
      {priority: 'event'});
  }
});
"

pt_css <- "
.pt-family-bin { border: 1px solid #ccc; border-radius: 6px; margin-bottom: 8px; padding: 6px; min-height: 46px; }
.pt-family-bin.pt-empty { border-style: dashed; opacity: .8; }
.pt-family-header { font-weight: bold; padding: 4px 8px; border-radius: 4px; margin-bottom: 4px; cursor: pointer; color: #fff; text-shadow: 0 0 2px rgba(0,0,0,.6); display: flex; justify-content: space-between; }
.pt-del { cursor: pointer; opacity: .75; font-weight: normal; }
.pt-chip { display: inline-block; padding: 3px 8px; margin: 2px; border-radius: 4px; cursor: grab; color: #fff; text-shadow: 0 0 2px rgba(0,0,0,.6); font-size: 12px; }
"

n_clusters <- length(unique(pt_env$assignment$cluster))
init_n_fam <- length(unique(pt_env$assignment$family_id))
p0 <- pt_env$session$params

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(tags$style(HTML(pt_css)), tags$script(HTML(drag_drop_js))),
  titlePanel("Palettome"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mode", "Mode", c("harmonious", "contrast"), selected = p0$mode),
      conditionalPanel(
        "input.mode == 'harmonious'",
        selectInput("harmonious_style", "Harmonious style",
          c("sweep (multi-hue analogous)" = "sweep", "per family (one hue each)" = "per_family"),
          selected = p0$harmonious_style %||% "sweep"
        )
      ),
      selectInput("neighbor_hues", "Neighboring compartments",
        c("auto (by mode)" = "auto", "contrasting hues" = "contrast", "analogous hues" = "coherent"),
        selected = "auto"
      ),
      numericInput("seed", "Seed", value = p0$seed, min = 1, step = 1),
      checkboxInput("auto_range", "Auto lightness / chroma range", value = TRUE),
      conditionalPanel(
        "!input.auto_range",
        sliderInput("lightness_range", "Lightness range", min = 0, max = 100,
          value = p0$lightness_range),
        sliderInput("chroma_range", "Chroma range", min = 0, max = 100,
          value = p0$chroma_range)
      ),
      selectInput("cvd", "Colorblind-safe preview", c("none", "deutan", "protan", "tritan")),
      tags$hr(),
      sliderInput("k", "Number of families (re-detect)",
        min = 1, max = max(1, n_clusters - 1), value = init_n_fam, step = 1),
      actionButton("redetect", "Re-detect families from k"),
      actionButton("add_family", "+ Add compartment"),
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
        column(6,
          h4("Families (drag chips to regroup, click to recolor)"),
          uiOutput("family_bins")
        )
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
    family_list = unique(pt_env$assignment$family_id),
    pending_family = NULL,
    selected = NULL
  )

  neighbor_hues_arg <- function() if (identical(input$neighbor_hues, "auto")) NULL else input$neighbor_hues
  range_arg <- function(which) {
    if (isTRUE(input$auto_range)) "auto" else input[[which]]
  }

  regenerate <- function() {
    rv$session <- palettome::generate_palette(
      rv$assignment,
      session = rv$session,
      mode = input$mode,
      harmonious_style = input$harmonious_style %||% "sweep",
      neighbor_hues = neighbor_hues_arg(),
      neighbors = pt_env$neighbors,
      seed = input$seed,
      lightness_range = range_arg("lightness_range"),
      chroma_range = range_arg("chroma_range"),
      cvd = input$cvd
    )
  }

  # drop empty families (except a freshly added, still-unused one)
  prune_families <- function() {
    present <- unique(rv$assignment$family_id)
    keep <- rv$family_list[rv$family_list %in% present |
      rv$family_list == (rv$pending_family %||% "")]
    if (length(keep) == 0) keep <- present
    rv$family_list <- keep
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
      df, x = ~x, y = ~y, color = ~cluster, colors = color_map,
      type = "scattergl", mode = "markers", marker = list(size = 4, opacity = 0.7)
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
    fam_ids <- rv$family_list
    fam_color <- stats::setNames(rv$session$families$color, rv$session$families$family_id)
    cluster_color <- stats::setNames(rv$session$clusters$color, rv$session$clusters$cluster)

    hexor <- function(x, fallback = "#888888") if (length(x) != 1 || is.na(x)) fallback else x

    bins <- lapply(fam_ids, function(fid) {
      members <- rv$assignment$cluster[rv$assignment$family_id == fid]
      chips <- lapply(members, function(cl) {
        tags$div(
          class = "pt-chip", draggable = "true", `data-cluster` = cl,
          style = sprintf("background:%s;", hexor(cluster_color[cl])),
          onclick = sprintf("Shiny.setInputValue('chip_click', '%s', {priority: 'event'})", cl),
          cl
        )
      })
      hdr_bg <- hexor(fam_color[fid])
      del <- if (length(members) == 0) {
        tags$span(class = "pt-del", HTML("&times;"),
          onclick = sprintf("Shiny.setInputValue('family_del', '%s', {priority: 'event'})", fid))
      }
      tags$div(
        class = paste("pt-family-bin", if (length(members) == 0) "pt-empty"),
        `data-family` = fid,
        tags$div(
          class = "pt-family-header", style = sprintf("background:%s;", hdr_bg),
          tags$span(fid,
            onclick = sprintf("Shiny.setInputValue('family_click', '%s', {priority: 'event'})", fid)),
          del
        ),
        chips
      )
    })
    tagList(bins)
  })

  recolor_modal <- function(title, current) {
    showModal(modalDialog(
      title = title,
      colourpicker::colourInput("picked_color", "Color", value = current),
      div(style = "margin-top:6px;",
        actionButton("optimize_pick", "Optimize (snap L/C to palette)")),
      footer = tagList(modalButton("Cancel"), actionButton("apply_color", "Apply"))
    ))
  }

  observeEvent(input$chip_click, {
    rv$selected <- list(type = "cluster", id = input$chip_click)
    cur <- rv$session$clusters$color[rv$session$clusters$cluster == input$chip_click]
    recolor_modal(paste("Recolor cluster", input$chip_click), cur)
  })

  observeEvent(input$family_click, {
    rv$selected <- list(type = "family", id = input$family_click)
    cur <- rv$session$families$color[rv$session$families$family_id == input$family_click]
    recolor_modal(paste("Recolor family", input$family_click), cur)
  })

  observeEvent(input$optimize_pick, {
    req(input$picked_color)
    colourpicker::updateColourInput(session, "picked_color",
      value = palettome::optimize_color(input$picked_color, session = rv$session))
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
    if (identical(drop$family, rv$pending_family)) rv$pending_family <- NULL
    prune_families()
    regenerate()
  })

  observeEvent(input$add_family, {
    existing <- suppressWarnings(as.integer(sub("^F", "", rv$family_list)))
    nxt <- paste0("F", max(c(0, existing[!is.na(existing)])) + 1)
    rv$pending_family <- nxt
    rv$family_list <- c(rv$family_list, nxt)
    showNotification(sprintf("Added compartment %s -- drag clusters into it", nxt), duration = 3)
  })

  observeEvent(input$family_del, {
    if (any(rv$assignment$family_id == input$family_del)) return()
    rv$family_list <- setdiff(rv$family_list, input$family_del)
    if (identical(rv$pending_family, input$family_del)) rv$pending_family <- NULL
  })

  observeEvent(input$redetect, {
    req(rv$dendro)
    rv$assignment <- palettome::cut_families(rv$dendro, k = input$k)
    rv$family_list <- unique(rv$assignment$family_id)
    rv$pending_family <- NULL
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
    rv$family_list <- unique(rv$assignment$family_id)
    rv$pending_family <- NULL
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
