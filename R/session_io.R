#' Export a palette session to JSON
#'
#' Writes the stable, language-agnostic JSON contract described in the
#' package README (`schema_version`, `clusters`, `families`, `params`) so a
#' future non-R (e.g. Python/Scanpy) frontend could read/write the same
#' file.
#'
#' @param session A `palettome_session`.
#' @param file Path to write to.
#' @return Invisibly, the JSON string that was written.
#' @export
export_palette_json <- function(session, file) {
  stopifnot(inherits(session, "palettome_session"))
  json <- jsonlite::toJSON(
    unclass(session),
    dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, na = "null"
  )
  writeLines(json, file)
  invisible(json)
}

#' Import a palette session from JSON
#'
#' @param file Path to a JSON file written by [export_palette_json()].
#' @return A `palettome_session`.
#' @export
import_palette_json <- function(file) {
  raw <- jsonlite::fromJSON(file, simplifyDataFrame = TRUE)
  clusters <- as.data.frame(raw$clusters, stringsAsFactors = FALSE)
  clusters$manual_color <- as.logical(clusters$manual_color)
  clusters$n_cells <- suppressWarnings(as.integer(clusters$n_cells))
  families <- as.data.frame(raw$families, stringsAsFactors = FALSE)
  families$manual_color <- as.logical(families$manual_color)
  structure(
    list(
      schema_version = raw$schema_version %||% "1.0",
      clusters = clusters,
      families = families,
      params = raw$params
    ),
    class = "palettome_session"
  )
}

#' Export cluster and/or family color tables as CSV
#'
#' @param session A `palettome_session`.
#' @param clusters_file Path to write the cluster -> color table to (columns
#'   `cluster`, `family_id`, `color`, `manual_color`, `n_cells`), or `NULL`
#'   to skip.
#' @param families_file Optional path to also write the family -> color
#'   table to (columns `family_id`, `color`, `manual_color`).
#' @return Invisibly, `TRUE`.
#' @export
export_palette_csv <- function(session, clusters_file, families_file = NULL) {
  stopifnot(inherits(session, "palettome_session"))
  if (!is.null(clusters_file)) {
    utils::write.csv(session$clusters, clusters_file, row.names = FALSE)
  }
  if (!is.null(families_file)) {
    utils::write.csv(session$families, families_file, row.names = FALSE)
  }
  invisible(TRUE)
}
