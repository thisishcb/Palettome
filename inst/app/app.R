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
.pt-range-preview { display: flex; height: 14px; margin: -6px 0 12px 0; border-radius: 3px; overflow: hidden; border: 1px solid rgba(0,0,0,.08); }
.pt-range-preview > div { flex: 1; }
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
      conditionalPanel(
        "input.mode == 'harmonious' && input.harmonious_style == 'sweep'",
        textInput("sweep_anchors_text", "Custom gradient anchors (optional)",
          value = "", placeholder = "#123456, #8844AA, #FFD27F"),
        uiOutput("anchors_preview")
      ),
      selectInput("neighbor_hues", "Neighboring compartments",
        c("auto (by mode)" = "auto", "contrasting hues" = "contrast", "analogous hues" = "coherent"),
        selected = "auto"
      ),
      fluidRow(
        column(7, numericInput("seed", "Seed", value = p0$seed, min = 1, step = 1)),
        column(5, div(style = "margin-top:25px;", actionButton("randomize_seed", "\U0001F3B2 Randomize")))
      ),
      checkboxInput("auto_range", "Auto lightness / chroma range", value = TRUE),
      conditionalPanel(
        "!input.auto_range",
        sliderInput("lightness_range", "Lightness range", min = 0, max = 100,
          value = p0$lightness_range),
        uiOutput("lightness_preview"),
        sliderInput("chroma_range", "Chroma range", min = 0, max = 100,
          value = p0$chroma_range),
        uiOutput("chroma_preview")
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

  # Comma-separated hex list -> validated character vector (or NULL if
  # blank / fewer than 2 valid colors, in which case the sweep falls back
  # to its automatic gradient).
  parse_anchors <- function(txt) {
    if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
    parts <- trimws(strsplit(txt, ",")[[1]])
    parts <- parts[nzchar(parts)]
    ok <- parts[vapply(parts, function(p) {
      tryCatch({
        farver::decode_colour(p)
        TRUE
      }, error = function(e) FALSE)
    }, logical(1))]
    if (length(ok) < 2) NULL else ok
  }

  regenerate <- function(seed_override = NULL) {
    anchors <- if (identical(input$harmonious_style, "sweep")) parse_anchors(input$sweep_anchors_text)
    rv$session <- palettome::generate_palette(
      rv$assignment,
      session = rv$session,
      mode = input$mode,
      harmonious_style = input$harmonious_style %||% "sweep",
      sweep_anchors = anchors,
      neighbor_hues = neighbor_hues_arg(),
      neighbors = pt_env$neighbors,
      seed = seed_override %||% input$seed,
      lightness_range = range_arg("lightness_range"),
      chroma_range = range_arg("chroma_range"),
      cvd = input$cvd
    )
  }

  output$anchors_preview <- renderUI({
    raw_text <- trimws(input$sweep_anchors_text %||% "")
    if (!nzchar(raw_text)) {
      return(tags$div(
        style = "font-size:11px; color:#888; margin:-4px 0 8px 0;",
        "Leave blank for an automatic gradient (or click \U0001F3B2 Randomize for a new one)."
      ))
    }
    anchors <- parse_anchors(raw_text)
    if (is.null(anchors)) {
      return(tags$div(
        style = "font-size:11px; color:#c0392b; margin:-4px 0 8px 0;",
        "Need 2+ valid hex colors, comma-separated -- e.g. #123456, #8844AA"
      ))
    }
    tags$div(
      class = "pt-range-preview", style = "margin:-4px 0 8px 0;",
      lapply(anchors, function(h) tags$div(style = sprintf("background:%s;", h)))
    )
  })

  observeEvent(input$randomize_seed, {
    if (!is.null(parse_anchors(input$sweep_anchors_text))) {
      showNotification(
        "Custom gradient anchors are set, so the seed won't change the gradient -- clear them to randomize.",
        type = "warning", duration = 5
      )
    }
    new_seed <- sample.int(100000, 1)
    updateNumericInput(session, "seed", value = new_seed)
    regenerate(new_seed)
  })

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

  # A representative hue for the range-preview strips below: the current
  # palette's own first family color, so "what colors am I choosing"
  # reflects this session rather than an arbitrary reference hue.
  representative_hue <- reactive({
    tryCatch(
      farver::decode_colour(rv$session$families$color[1], to = "hcl")[1, "h"],
      error = function(e) 250
    )
  })

  # A strip of swatches spanning the full 0-100 axis of `range_vals`'s
  # slider, dimmed outside the currently-selected sub-range, so the numeric
  # Lightness/Chroma sliders show what they actually mean in color.
  range_preview <- function(kind, range_vals, hue) {
    vals <- seq(0, 100, length.out = 25)
    cols <- if (kind == "L") {
      # clamp chroma to what's actually displayable at each lightness, or
      # extreme L (near black/white) fixup-clips into odd off-hue colors
      mc <- colorspace::max_chroma(h = hue, l = pmin(pmax(vals, 1), 99))
      colorspace::hex(colorspace::polarLUV(L = vals, C = pmin(42, mc * 0.95), H = hue), fixup = TRUE)
    } else {
      mc <- colorspace::max_chroma(h = hue, l = 55)
      colorspace::hex(colorspace::polarLUV(L = 55, C = pmin(vals, mc * 0.95), H = hue), fixup = TRUE)
    }
    swatches <- Map(function(v, col) {
      dim <- v < range_vals[1] || v > range_vals[2]
      tags$div(style = sprintf("background:%s; opacity:%s;", col, if (dim) "0.2" else "1"))
    }, vals, cols)
    tags$div(class = "pt-range-preview", swatches)
  }

  output$lightness_preview <- renderUI({
    req(input$lightness_range)
    range_preview("L", input$lightness_range, representative_hue())
  })
  output$chroma_preview <- renderUI({
    req(input$chroma_range)
    range_preview("C", input$chroma_range, representative_hue())
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
        xaxis = list(title = pd$coord_cols[1]),
        yaxis = list(title = pd$coord_cols[2], scaleanchor = "x", scaleratio = 1),
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
        # stopPropagation so deleting an empty bin doesn't also open the
        # recolor dialog for it (the whole header bar is now the click
        # target for that).
        tags$span(class = "pt-del", HTML("&times;"),
          onclick = sprintf(
            "event.stopPropagation(); Shiny.setInputValue('family_del', '%s', {priority: 'event'})", fid
          ))
      }
      tags$div(
        class = paste("pt-family-bin", if (length(members) == 0) "pt-empty"),
        `data-family` = fid,
        tags$div(
          class = "pt-family-header", style = sprintf("background:%s;", hdr_bg),
          onclick = sprintf("Shiny.setInputValue('family_click', '%s', {priority: 'event'})", fid),
          tags$span(fid),
          del
        ),
        chips
      )
    })
    tagList(bins)
  })

  # Tracks the last hex value either side of the hex<->HSL-slider sync
  # agreed on, so updating one side doesn't bounce back and forth with the
  # other (see the two observers below).
  last_hex <- reactiveVal(NULL)

  recolor_modal <- function(title, current) {
    hsl0 <- farver::decode_colour(current, to = "hsl")[1, ]
    last_hex(toupper(current))
    showModal(modalDialog(
      title = title,
      colourpicker::colourInput("picked_color", "Color (hex / swatch)", value = current),
      tags$div(
        style = "margin-top:8px; padding-top:6px; border-top:1px solid #eee;",
        tags$b("Raw HSL controls"), tags$span(
          " -- edit directly; this can produce colors the generator would never pick.",
          style = "color:#888; font-size:11px;"
        ),
        sliderInput("hsl_h", "Hue", min = 0, max = 360, value = round(hsl0[["h"]]), step = 1),
        sliderInput("hsl_s", "Saturation (%)", min = 0, max = 100, value = round(hsl0[["s"]]), step = 1),
        sliderInput("hsl_l", "Lightness (%)", min = 0, max = 100, value = round(hsl0[["l"]]), step = 1)
      ),
      uiOutput("color_diag"),
      div(style = "margin-top:6px;",
        actionButton("optimize_pick", "Optimize (snap L/C to palette)")),
      footer = tagList(modalButton("Cancel"), actionButton("apply_color", "Apply"))
    ))
  }

  # decode defensively: a hand-typed hex box can be momentarily invalid/
  # incomplete, and this must never take the reactive graph down with it.
  safe_decode <- function(hex, to) tryCatch(farver::decode_colour(hex, to = to)[1, ], error = function(e) NULL)

  # hex box (or Optimize) changed -> reflect it in the HSL sliders
  observeEvent(input$picked_color, {
    req(input$picked_color)
    cur <- toupper(input$picked_color)
    if (identical(cur, isolate(last_hex()))) return()
    hsl <- safe_decode(cur, "hsl")
    req(hsl)
    last_hex(cur)
    updateSliderInput(session, "hsl_h", value = round(hsl[["h"]]))
    updateSliderInput(session, "hsl_s", value = round(hsl[["s"]]))
    updateSliderInput(session, "hsl_l", value = round(hsl[["l"]]))
  }, ignoreInit = TRUE)

  # HSL sliders changed -> push the resulting hex into the color box
  observeEvent(list(input$hsl_h, input$hsl_s, input$hsl_l), {
    req(!is.null(input$hsl_h), !is.null(input$hsl_s), !is.null(input$hsl_l))
    hex <- toupper(farver::encode_colour(
      matrix(c(input$hsl_h, input$hsl_s, input$hsl_l), ncol = 3), from = "hsl"
    ))
    if (identical(hex, isolate(last_hex()))) return()
    last_hex(hex)
    colourpicker::updateColourInput(session, "picked_color", value = hex)
  }, ignoreInit = TRUE)

  # Live readout of both the raw HSL you're editing in and the perceptual
  # HCL space the generator itself reasons in, for whatever color is
  # currently in the dialog (program-picked or hand-edited).
  output$color_diag <- renderUI({
    req(input$picked_color)
    hsl <- safe_decode(input$picked_color, "hsl")
    hcl <- safe_decode(input$picked_color, "hcl")
    req(hsl, hcl)
    tags$div(
      style = "margin-top:8px; padding-top:6px; border-top:1px solid #eee; font-size:12px; color:#555;",
      tags$div(sprintf(
        "HSL: H %.0f°  S %.0f%%  L %.0f%%", hsl[["h"]], hsl[["s"]], hsl[["l"]]
      )),
      tags$div(sprintf(
        "Program's HCL (perceptual) space: H %.0f°  C %.1f  L %.1f",
        hcl[["h"]], hcl[["c"]], hcl[["l"]]
      ))
    )
  })

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
      # a family's color stands in for its whole hue: re-derive its
      # non-manual members' shades around the new color immediately.
      regenerate()
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
