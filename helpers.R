`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  x
}

COLORS <- c(
  navy = "#16324F",
  ink = "#243B53",
  slate = "#4F6D7A",
  blue = "#5FA8D3",
  mist = "#CAE9FF",
  sky = "#DCEEF8",
  sage = "#7BAE7F",
  gold = "#D5A44B",
  coral = "#C97C5D",
  rose = "#B56576",
  plum = "#8E6C8A",
  sand = "#F5E6D3",
  cloud = "#F4F8FB",
  charcoal = "#495057"
)

resolve_results_location <- function() {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  candidate_dirs <- c(
    file.path(project_dir, "data"),
    file.path(tempdir(), "diplom_project_results")
  )
  candidate_labels <- c("project", "temp")

  for (i in seq_along(candidate_dirs)) {
    dir_path <- candidate_dirs[[i]]
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

    probe_file <- tempfile(pattern = "write_probe_", tmpdir = dir_path, fileext = ".tmp")
    writable <- tryCatch(
      {
        cat("ok", file = probe_file)
        file.exists(probe_file)
      },
      error = function(e) FALSE
    )

    if (isTRUE(writable)) {
      unlink(probe_file, force = TRUE)
      return(list(
        dir = dir_path,
        file = file.path(dir_path, "experiment_results.csv"),
        source = candidate_labels[[i]]
      ))
    }
  }

  stop("No writable directory is available for saving experiment results.", call. = FALSE)
}

RESULTS_LOCATION <- resolve_results_location()
RESULTS_DIR <- RESULTS_LOCATION$dir
RESULTS_FILE <- RESULTS_LOCATION$file
RESULTS_LOCK_DIR <- paste0(RESULTS_FILE, ".lock")
RESULTS_STORAGE_LABEL <- if (identical(RESULTS_LOCATION$source, "project")) {
  "Жоба бумасы"
} else {
  "Уақытша бума"
}

GESTALT_PRINCIPLES <- c("Closure", "Similarity", "Proximity", "Symmetry", "Continuity")
ANALYTIC_FAMILY <- "Analytics"
VALID_RENDERERS <- c("single_plot", "plot_grid", "multi_panel_individual")
TASK_PREFIXES <- c(
  Closure = "CLO_",
  Similarity = "SIM_",
  Proximity = "PROX_",
  Symmetry = "SYM_",
  Continuity = "CONT_"
)

RESULT_COLUMNS <- c(
  "participant_id",
  "age",
  "gender",
  "specialization",
  "experiment_started_at",
  "started_at",
  "submitted_at",
  "displayed_at_client_ms",
  "submitted_at_client_ms",
  "task_order_position",
  "task_id",
  "question_id",
  "task_family",
  "gestalt_principle",
  "visualization_type",
  "question_type",
  "question_text",
  "options_shown",
  "correct_answer",
  "selected_answer",
  "is_correct",
  "reaction_time_sec",
  "chart_count",
  "ease_rating",
  "confidence",
  "parent_task_id",
  "panel_id",
  "panel_label",
  "panel_order",
  "panel_selected_answer",
  "panel_is_correct",
  "panel_reaction_time_sec",
  "panel_submitted_at",
  "panel_displayed_at_client_ms",
  "session_id"
)

DT_LANGUAGE <- list(
  decimal = ".",
  emptyTable = "Кестеде дерек жоқ",
  info = "_START_-тен _END_-ке дейін көрсетілді, барлығы _TOTAL_ жазба",
  infoEmpty = "0-ден 0-ге дейін көрсетілді, барлығы 0 жазба",
  infoFiltered = "(_MAX_ жазбаның ішінен сүзілді)",
  thousands = ",",
  lengthMenu = "_MENU_ жазбаны көрсету",
  loadingRecords = "Жүктелуде...",
  processing = "Өңделуде...",
  search = "Іздеу:",
  zeroRecords = "Сәйкес жазбалар табылмады",
  paginate = list(
    first = "Бірінші",
    last = "Соңғы",
    "next" = "Келесі",
    previous = "Алдыңғы"
  )
)

timestamp_string <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%OS3", tz = Sys.timezone())
}

client_ms_to_posix <- function(client_ms) {
  client_ms <- suppressWarnings(as.numeric(client_ms))
  if (length(client_ms) != 1 || is.na(client_ms)) {
    return(as.POSIXct(NA))
  }
  as.POSIXct(client_ms / 1000, origin = "1970-01-01", tz = Sys.timezone())
}

trim_or_na <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) {
    return(NA_character_)
  }
  x
}

empty_results_df <- function() {
  data.frame(
    participant_id = character(),
    age = numeric(),
    gender = character(),
    specialization = character(),
    experiment_started_at = character(),
    started_at = character(),
    submitted_at = character(),
    displayed_at_client_ms = numeric(),
    submitted_at_client_ms = numeric(),
    task_order_position = integer(),
    task_id = character(),
    question_id = character(),
    task_family = character(),
    gestalt_principle = character(),
    visualization_type = character(),
    question_type = character(),
    question_text = character(),
    options_shown = character(),
    correct_answer = character(),
    selected_answer = character(),
    is_correct = logical(),
    reaction_time_sec = numeric(),
    chart_count = numeric(),
    ease_rating = numeric(),
    confidence = numeric(),
    parent_task_id = character(),
    panel_id = character(),
    panel_label = character(),
    panel_order = integer(),
    panel_selected_answer = character(),
    panel_is_correct = logical(),
    panel_reaction_time_sec = numeric(),
    panel_submitted_at = character(),
    panel_displayed_at_client_ms = numeric(),
    session_id = character(),
    stringsAsFactors = FALSE
  )
}

normalize_results_row <- function(row_df) {
  missing_cols <- setdiff(RESULT_COLUMNS, names(row_df))
  for (col_name in missing_cols) {
    row_df[[col_name]] <- NA
  }

  row_df <- row_df[, RESULT_COLUMNS, drop = FALSE]

  character_cols <- c(
    "participant_id", "gender", "specialization", "experiment_started_at",
    "started_at", "submitted_at", "task_id", "question_id", "task_family",
    "gestalt_principle", "visualization_type", "question_type",
    "question_text", "options_shown", "correct_answer", "selected_answer",
    "parent_task_id", "panel_id", "panel_label", "panel_selected_answer",
    "panel_submitted_at", "session_id"
  )
  numeric_cols <- c(
    "age", "displayed_at_client_ms", "submitted_at_client_ms",
    "task_order_position", "reaction_time_sec", "chart_count",
    "ease_rating", "confidence", "panel_order", "panel_reaction_time_sec",
    "panel_displayed_at_client_ms"
  )
  logical_cols <- c("is_correct", "panel_is_correct")

  for (col_name in intersect(character_cols, names(row_df))) {
    row_df[[col_name]] <- enc2utf8(as.character(row_df[[col_name]]))
  }

  for (col_name in intersect(numeric_cols, names(row_df))) {
    row_df[[col_name]] <- suppressWarnings(as.numeric(row_df[[col_name]]))
  }

  for (col_name in intersect(logical_cols, names(row_df))) {
    row_df[[col_name]] <- as.logical(row_df[[col_name]])
  }

  row_df
}

normalize_results_table <- function(results_df) {
  if (is.null(results_df) || nrow(results_df) == 0) {
    return(empty_results_df())
  }
  normalize_results_row(as.data.frame(results_df, stringsAsFactors = FALSE))
}

acquire_results_lock <- function(lock_dir, timeout_sec = 10, poll_sec = 0.05, stale_after_sec = 120) {
  start_time <- Sys.time()

  repeat {
    dir.create(dirname(lock_dir), recursive = TRUE, showWarnings = FALSE)

    if (dir.create(lock_dir, showWarnings = FALSE)) {
      return(invisible(TRUE))
    }

    lock_info <- suppressWarnings(file.info(lock_dir))
    if (nrow(lock_info) == 1 && !is.na(lock_info$ctime)) {
      lock_age <- as.numeric(difftime(Sys.time(), lock_info$ctime, units = "secs"))
      if (!is.na(lock_age) && lock_age > stale_after_sec) {
        unlink(lock_dir, recursive = TRUE, force = TRUE)
        next
      }
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (!is.na(elapsed) && elapsed >= timeout_sec) {
      stop("Could not acquire the results file lock within the timeout window.")
    }

    Sys.sleep(poll_sec)
  }
}

wait_for_results_unlock <- function(lock_dir, timeout_sec = 10, poll_sec = 0.05) {
  start_time <- Sys.time()
  while (dir.exists(lock_dir)) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (!is.na(elapsed) && elapsed >= timeout_sec) {
      break
    }
    Sys.sleep(poll_sec)
  }
}

read_results_file <- function(file_path, n = NULL, wait_for_lock = TRUE, fail_on_error = FALSE) {
  if (wait_for_lock) {
    wait_for_results_unlock(paste0(file_path, ".lock"))
  }

  if (!file.exists(file_path) || file.info(file_path)$size == 0) {
    return(empty_results_df())
  }

  out <- tryCatch(
    readr::read_csv(file_path, show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      if (isTRUE(fail_on_error)) {
        stop(
          sprintf("The existing results file could not be read safely: %s", e$message),
          call. = FALSE
        )
      }
      empty_results_df()
    }
  )

  out <- normalize_results_table(out)

  if (!is.null(n) && nrow(out) > n) {
    out <- utils::tail(out, n)
  }

  out
}

replace_results_file <- function(temp_file, file_path, attempts = 4, delay_sec = 0.15) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  for (attempt in seq_len(attempts)) {
    if (file.exists(file_path)) {
      suppressWarnings(unlink(file_path, force = TRUE))
    }

    renamed <- suppressWarnings(file.rename(temp_file, file_path))
    if (isTRUE(renamed) && file.exists(file_path)) {
      return(invisible(TRUE))
    }

    copied <- file.copy(temp_file, file_path, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    if (isTRUE(copied) && file.exists(file_path)) {
      return(invisible(TRUE))
    }

    Sys.sleep(delay_sec)
  }

  stop("The result file could not be updated after multiple write attempts.", call. = FALSE)
}

excel_column_name <- function(index) {
  index <- as.integer(index)
  if (is.na(index) || index < 1) {
    stop("Excel column index must be a positive integer.", call. = FALSE)
  }

  name <- character()
  while (index > 0) {
    rem <- (index - 1) %% 26
    name <- c(intToUtf8(65 + rem), name)
    index <- (index - 1) %/% 26
  }
  paste(name, collapse = "")
}

xml_escape <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  x
}

sanitize_sheet_name <- function(x, existing = character()) {
  base <- trimws(enc2utf8(as.character(x %||% "Парақ")))
  if (!nzchar(base)) {
    base <- "Парақ"
  }

  base <- gsub("[\\\\/:*?\\[\\]]", "_", base)
  base <- substr(base, 1, 31)
  candidate <- base
  counter <- 1L

  while (candidate %in% existing) {
    suffix <- paste0("_", counter)
    candidate <- paste0(substr(base, 1, max(1, 31 - nchar(suffix))), suffix)
    counter <- counter + 1L
  }

  candidate
}

build_xlsx_cell_xml <- function(value, row_index, col_index) {
  cell_ref <- paste0(excel_column_name(col_index), row_index)

  if (length(value) == 0 || is.na(value)) {
    return(sprintf('<c r="%s"/>', cell_ref))
  }

  if (inherits(value, "POSIXt")) {
    value <- format(value, "%Y-%m-%d %H:%M:%S")
  }

  if (is.numeric(value) && is.finite(value)) {
    return(sprintf('<c r="%s"><v>%s</v></c>', cell_ref, format(value, scientific = FALSE, trim = TRUE)))
  }

  text_value <- xml_escape(value)
  sprintf('<c r="%s" t="inlineStr"><is><t xml:space="preserve">%s</t></is></c>', cell_ref, text_value)
}

build_xlsx_sheet_xml <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  headers <- names(data)

  rows_xml <- character()
  header_cells <- vapply(seq_along(headers), function(i) {
    build_xlsx_cell_xml(headers[[i]], 1L, i)
  }, character(1))
  rows_xml[[1]] <- sprintf('<row r="1">%s</row>', paste(header_cells, collapse = ""))

  if (nrow(data) > 0) {
    for (row_index in seq_len(nrow(data))) {
      values <- data[row_index, , drop = TRUE]
      row_cells <- vapply(seq_along(values), function(col_index) {
        build_xlsx_cell_xml(values[[col_index]], row_index + 1L, col_index)
      }, character(1))
      rows_xml[[length(rows_xml) + 1L]] <- sprintf('<row r="%s">%s</row>', row_index + 1L, paste(row_cells, collapse = ""))
    }
  }

  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
    '<sheetData>',
    paste(rows_xml, collapse = ""),
    '</sheetData>',
    '</worksheet>'
  )
}

build_xlsx_workbook_xml <- function(sheet_names) {
  sheet_nodes <- vapply(seq_along(sheet_names), function(i) {
    sprintf(
      '<sheet name="%s" sheetId="%s" r:id="rId%s"/>',
      xml_escape(sheet_names[[i]]),
      i,
      i
    )
  }, character(1))

  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    '<sheets>',
    paste(sheet_nodes, collapse = ""),
    '</sheets>',
    '</workbook>'
  )
}

build_xlsx_workbook_rels_xml <- function(sheet_names) {
  rel_nodes <- vapply(seq_along(sheet_names), function(i) {
    sprintf(
      '<Relationship Id="rId%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet%s.xml"/>',
      i,
      i
    )
  }, character(1))

  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    paste(rel_nodes, collapse = ""),
    '</Relationships>'
  )
}

build_xlsx_content_types_xml <- function(sheet_count) {
  sheet_overrides <- vapply(seq_len(sheet_count), function(i) {
    sprintf(
      '<Override PartName="/xl/worksheets/sheet%s.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
      i
    )
  }, character(1))

  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
    '<Default Extension="xml" ContentType="application/xml"/>',
    '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
    paste(sheet_overrides, collapse = ""),
    '</Types>'
  )
}

build_xlsx_root_rels_xml <- function() {
  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
    '</Relationships>'
  )
}

compress_directory_to_zip <- function(source_dir, destination_zip) {
  source_dir <- normalizePath(source_dir, winslash = "\\", mustWork = TRUE)
  destination_zip <- normalizePath(destination_zip, winslash = "\\", mustWork = FALSE)

  ps_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  }

  command <- paste(
    "$ErrorActionPreference='Stop';",
    "Add-Type -AssemblyName System.IO.Compression.FileSystem;",
    sprintf("if (Test-Path %s) { Remove-Item %s -Force }", ps_quote(destination_zip), ps_quote(destination_zip)),
    sprintf("[System.IO.Compression.ZipFile]::CreateFromDirectory(%s, %s)", ps_quote(source_dir), ps_quote(destination_zip))
  )

  result <- suppressWarnings(
    system2("powershell", c("-NoProfile", "-Command", command), stdout = TRUE, stderr = TRUE)
  )

  if (!file.exists(destination_zip)) {
    stop(
      paste(c("The Excel archive could not be created.", result), collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

write_simple_xlsx <- function(sheets, path) {
  if (!length(sheets)) {
    stop("At least one worksheet is required for Excel export.", call. = FALSE)
  }

  root_dir <- tempfile(pattern = "xlsx_export_")
  dir.create(root_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root_dir, recursive = TRUE, force = TRUE), add = TRUE)

  dir.create(file.path(root_dir, "_rels"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root_dir, "xl", "_rels"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root_dir, "xl", "worksheets"), recursive = TRUE, showWarnings = FALSE)

  sheet_names <- character()
  sheet_values <- vector("list", length(sheets))

  for (i in seq_along(sheets)) {
    sheet_name <- names(sheets)[i] %||% paste0("Парақ ", i)
    sheet_name <- sanitize_sheet_name(sheet_name, existing = sheet_names)
    sheet_names[[i]] <- sheet_name
    sheet_values[[i]] <- as.data.frame(sheets[[i]], stringsAsFactors = FALSE, check.names = FALSE)
  }

  writeLines(build_xlsx_content_types_xml(length(sheet_names)), file.path(root_dir, "[Content_Types].xml"), useBytes = TRUE)
  writeLines(build_xlsx_root_rels_xml(), file.path(root_dir, "_rels", ".rels"), useBytes = TRUE)
  writeLines(build_xlsx_workbook_xml(sheet_names), file.path(root_dir, "xl", "workbook.xml"), useBytes = TRUE)
  writeLines(build_xlsx_workbook_rels_xml(sheet_names), file.path(root_dir, "xl", "_rels", "workbook.xml.rels"), useBytes = TRUE)

  for (i in seq_along(sheet_values)) {
    sheet_xml <- build_xlsx_sheet_xml(sheet_values[[i]])
    writeLines(sheet_xml, file.path(root_dir, "xl", "worksheets", paste0("sheet", i, ".xml")), useBytes = TRUE)
  }

  compress_directory_to_zip(root_dir, path)
  invisible(TRUE)
}

append_results_csv <- function(row_df, file_path, lock_dir, timeout_sec = 10, poll_sec = 0.05) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  row_df <- normalize_results_table(row_df)

  acquire_results_lock(lock_dir, timeout_sec = timeout_sec, poll_sec = poll_sec)
  on.exit(unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)

  existing_rows <- read_results_file(file_path, wait_for_lock = FALSE, fail_on_error = TRUE)
  combined_rows <- normalize_results_table(dplyr::bind_rows(existing_rows, row_df))
  temp_file <- tempfile(pattern = "results_", tmpdir = dirname(file_path), fileext = ".csv")
  on.exit(unlink(temp_file, force = TRUE), add = TRUE)

  readr::write_csv(combined_rows, temp_file)
  replace_results_file(temp_file, file_path)

  invisible(TRUE)
}

rescale_values <- function(x, to = c(8, 16)) {
  x <- as.numeric(x)
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    return(rep(mean(to), length(x)))
  }
  (x - rng[1]) / diff(rng) * diff(to) + to[1]
}

point_df <- function(
  x,
  y,
  color = COLORS["blue"],
  size = 4.2,
  outline = COLORS["navy"],
  label = NA_character_,
  panel = NULL,
  path_group = NULL
) {
  n <- length(x)
  data.frame(
    x = x,
    y = y,
    color = if (length(color) == 1) rep(unname(color), n) else unname(color),
    size = if (length(size) == 1) rep(unname(size), n) else unname(size),
    outline = if (length(outline) == 1) rep(unname(outline), n) else unname(outline),
    label = if (length(label) == 1) rep(label, n) else label,
    panel = if (is.null(panel)) rep("Сурет", n) else if (length(panel) == 1) rep(panel, n) else panel,
    path_group = if (is.null(path_group)) rep(NA_character_, n) else if (length(path_group) == 1) rep(path_group, n) else path_group,
    stringsAsFactors = FALSE
  )
}

curve_points <- function(x, y, color = COLORS["blue"], size = 4.2, panel = NULL, path_group = NULL) {
  point_df(x, y, color = color, size = size, panel = panel, path_group = path_group)
}

segment_points <- function(x1, y1, x2, y2, n = 6, color = COLORS["blue"], size = 4.2, panel = NULL, path_group = NULL) {
  curve_points(
    seq(x1, x2, length.out = n),
    seq(y1, y2, length.out = n),
    color = color,
    size = size,
    panel = panel,
    path_group = path_group
  )
}

arc_points <- function(center_x, center_y, radius, start_deg, end_deg, n = 16, color = COLORS["blue"], size = 4.2, panel = NULL, path_group = NULL) {
  angles <- seq(start_deg, end_deg, length.out = n) * pi / 180
  point_df(
    center_x + radius * cos(angles),
    center_y + radius * sin(angles),
    color = color,
    size = size,
    panel = panel,
    path_group = path_group
  )
}

experiment_theme <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FBFDFF", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#E3EBF2", linewidth = 0.45),
      axis.text = ggplot2::element_text(color = COLORS["ink"]),
      axis.title = ggplot2::element_text(color = COLORS["slate"], face = "bold", size = 10),
      strip.background = ggplot2::element_rect(fill = "#ECF4FA", colour = NA),
      strip.text = ggplot2::element_text(face = "bold", color = COLORS["navy"], size = 12),
      legend.position = "none",
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

plot_scatter_generic <- function(data, xlim = c(0, 10), ylim = c(0, 10), show_axes = FALSE, show_labels = FALSE, connect = FALSE, augment_fn = NULL) {
  if (!"color" %in% names(data)) data$color <- COLORS["blue"]
  if (!"outline" %in% names(data)) data$outline <- COLORS["navy"]
  if (!"size" %in% names(data)) data$size <- 4.2
  if (!"panel" %in% names(data)) data$panel <- "Сурет"
  if (!"label" %in% names(data)) data$label <- NA_character_
  if (!"path_group" %in% names(data)) data$path_group <- paste0("g", seq_len(nrow(data)))

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y))

  if (connect) {
    p <- p +
      ggplot2::geom_path(
        ggplot2::aes(group = path_group, color = color),
        linewidth = 1.1,
        alpha = 0.9,
        lineend = "round"
      )
  }

  p <- p +
    ggplot2::geom_point(
      ggplot2::aes(size = size, fill = color, color = outline),
      shape = 21,
      stroke = 0.9,
      alpha = 0.98
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::coord_fixed(ratio = 1, xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    experiment_theme()

  if (length(unique(data$panel)) > 1) {
    p <- p + ggplot2::facet_wrap(~panel)
  }

  if (show_labels && any(!is.na(data$label))) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        color = COLORS["navy"],
        fontface = "bold",
        size = 4,
        vjust = -1
      )
  }

  if (!show_axes) {
    p <- p +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
  }

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

plot_bar_generic <- function(data, compact = FALSE, show_value_labels = TRUE, y_label = "Мән", augment_fn = NULL) {
  if (!"fill" %in% names(data)) {
    data$fill <- COLORS["blue"]
  }
  if (!"x_pos" %in% names(data)) {
    data$x_pos <- seq_len(nrow(data))
  }

  data <- data[order(data$x_pos), , drop = FALSE]
  data$value <- suppressWarnings(as.numeric(data$value))

  y_min_raw <- min(c(0, data$value), na.rm = TRUE)
  y_max_raw <- max(c(0, data$value), na.rm = TRUE)
  span <- y_max_raw - y_min_raw
  if (!is.finite(span) || span == 0) {
    span <- max(abs(c(y_min_raw, y_max_raw, 1)), na.rm = TRUE)
  }

  pad <- span * if (compact) 0.08 else 0.12
  label_pad <- span * if (compact) 0.03 else 0.045
  y_lower <- if (y_min_raw < 0) y_min_raw - pad else 0
  y_upper <- if (y_max_raw > 0) y_max_raw + pad else 0
  if (y_lower == y_upper) {
    y_lower <- y_lower - 1
    y_upper <- y_upper + 1
  }

  data$label_y <- ifelse(data$value >= 0, data$value + label_pad, data$value - label_pad)
  data$label_vjust <- ifelse(data$value >= 0, 0, 1)
  data$label_text <- format(round(data$value, 0), big.mark = " ", scientific = FALSE, trim = TRUE)

  x_labels <- data$category
  if (compact && nrow(data) > 5) {
    keep_idx <- unique(c(seq(1, nrow(data), by = 2), nrow(data)))
    x_labels[!seq_along(x_labels) %in% keep_idx] <- ""
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x_pos, y = value, fill = fill)) +
    ggplot2::geom_hline(yintercept = 0, color = "#9FB3C2", linewidth = 0.45) +
    ggplot2::geom_col(width = 0.72, color = "#FFFFFF", linewidth = 0.45, alpha = 0.96) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(
      breaks = data$x_pos,
      labels = x_labels,
      expand = ggplot2::expansion(mult = c(0.04, 0.06))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_lower, y_upper),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    experiment_theme() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = y_label) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = if (compact) 8 else 10,
        angle = if (compact) 0 else 18,
        hjust = 0.5
      ),
      axis.text.y = ggplot2::element_text(size = if (compact) 8 else 10)
    )

  if (show_value_labels) {
    p <- p +
      ggplot2::geom_text(
        data = data,
        ggplot2::aes(x = x_pos, y = label_y, label = label_text, vjust = label_vjust),
        inherit.aes = FALSE,
        color = COLORS["navy"],
        fontface = "bold",
        size = if (compact) 3.1 else 3.8
      )
  }

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

plot_heatmap_generic <- function(data, compact = FALSE, show_values = TRUE, augment_fn = NULL) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!is.factor(data$row)) {
    data$row <- factor(data$row, levels = rev(unique(data$row)))
  }
  if (!is.factor(data$col)) {
    data$col <- factor(data$col, levels = unique(data$col))
  }

  rng <- range(data$value, na.rm = TRUE)
  midpoint <- mean(rng)
  data$text_color <- ifelse(data$value >= midpoint, "#FFFFFF", COLORS["navy"])

  p <- ggplot2::ggplot(data, ggplot2::aes(x = col, y = row, fill = value)) +
    ggplot2::geom_tile(color = "#FFFFFF", linewidth = 1)

  if (all(is.finite(rng)) && diff(rng) > 0) {
    p <- p + ggplot2::scale_fill_gradient2(
      low = COLORS["sky"],
      mid = COLORS["blue"],
      high = COLORS["navy"],
      midpoint = midpoint
    )
  } else {
    p <- p + ggplot2::scale_fill_gradient(low = COLORS["mist"], high = COLORS["blue"])
  }

  p <- p +
    experiment_theme() +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = if (compact) 8 else 10),
      axis.text.y = ggplot2::element_text(size = if (compact) 8 else 10),
      legend.position = "none"
    )

  if (show_values) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = round(value, 0), color = text_color),
        fontface = "bold",
        size = if (compact) 3 else 3.6
      ) +
      ggplot2::scale_color_identity()
  }

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

plot_line_generic <- function(data, compact = FALSE, x_breaks = NULL, x_labels = NULL, y_label = "Мән", augment_fn = NULL) {
  if (!"display_color" %in% names(data)) data$display_color <- COLORS["blue"]
  if (!"series" %in% names(data)) data$series <- "S"
  if (!"panel" %in% names(data)) data$panel <- "Сурет"

  if (is.null(x_breaks)) x_breaks <- sort(unique(data$x))
  if (is.null(x_labels)) x_labels <- x_breaks

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, group = series)) +
    ggplot2::geom_line(
      ggplot2::aes(color = display_color),
      linewidth = 1.35,
      lineend = "round"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = display_color),
      shape = 21,
      size = if (compact) 3 else 3.5,
      stroke = 0.9,
      color = COLORS["navy"]
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels,
      expand = ggplot2::expansion(mult = c(0.02, 0.05))
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::labs(x = NULL, y = y_label) +
    experiment_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = if (compact) 0 else 35,
        hjust = 0.5,
        size = if (compact) 8 else 9
      ),
      axis.text.y = ggplot2::element_text(size = if (compact) 8 else 10)
    )

  if (length(unique(data$panel)) > 1) {
    p <- p + ggplot2::facet_wrap(~panel)
  }

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

# ---- Teacher analysis helpers ----
strip_empty_to_na <- function(x) {
  x <- trimws(as.character(x %||% NA))
  x[!nzchar(x)] <- NA_character_
  x
}

safe_numeric_coerce <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x_chr <- strip_empty_to_na(x)
  if (all(is.na(x_chr))) {
    return(rep(NA_real_, length(x_chr)))
  }

  x_chr <- gsub("%", "", x_chr, fixed = TRUE)
  x_chr <- gsub(",", ".", x_chr, fixed = TRUE)
  x_chr <- gsub("[^0-9\\.-]", "", x_chr)
  x_chr[!nzchar(x_chr)] <- NA_character_
  suppressWarnings(as.numeric(x_chr))
}

safe_logical_coerce <- function(x) {
  if (is.logical(x)) {
    return(as.logical(x))
  }

  x_chr <- tolower(strip_empty_to_na(x))
  out <- rep(NA, length(x_chr))
  out[x_chr %in% c("true", "t", "1", "yes", "y", "иә")] <- TRUE
  out[x_chr %in% c("false", "f", "0", "no", "n", "жоқ")] <- FALSE
  out
}

nonempty_unique_values <- function(x) {
  x <- strip_empty_to_na(x)
  sort(unique(x[!is.na(x)]))
}

analysis_metric_choices <- function() {
  c(
    "Субъективті жауап" = "subjective_answer",
    "Реакция уақыты" = "reaction_time",
    "Сенім" = "confidence",
    "Жеңілдік бағасы" = "ease_rating",
    "Дәлдік" = "accuracy"
  )
}

analysis_column_specs <- function() {
  list(
    participant_id = list(
      label = "Қатысушы идентификаторы",
      aliases = c("participant_id"),
      required = FALSE
    ),
    task_identifier = list(
      label = "Тапсырма идентификаторы",
      aliases = c("question_id", "task_id"),
      required = FALSE
    ),
    block_info = list(
      label = "Блок немесе task family",
      aliases = c("block", "task_family"),
      required = FALSE
    ),
    vis_type = list(
      label = "Визуализация түрі",
      aliases = c("vis_type", "visualization_type"),
      required = TRUE
    ),
    answer = list(
      label = "Жауап",
      aliases = c("answer", "selected_answer", "panel_selected_answer"),
      required = FALSE
    ),
    is_correct = list(
      label = "Дұрыс/бұрыс белгісі",
      aliases = c("is_correct", "panel_is_correct"),
      required = FALSE
    ),
    reaction_time_sec = list(
      label = "Реакция уақыты",
      aliases = c("reaction_time_sec", "time_sec", "panel_reaction_time_sec"),
      required = FALSE
    ),
    confidence = list(
      label = "Сенім",
      aliases = c("confidence"),
      required = FALSE
    ),
    ease_rating = list(
      label = "Жеңілдік бағасы",
      aliases = c("ease_rating"),
      required = FALSE
    )
  )
}

find_matching_column <- function(col_names, aliases) {
  if (!length(col_names)) {
    return(NA_character_)
  }
  lower_names <- tolower(col_names)
  lower_aliases <- tolower(aliases)
  match_idx <- match(lower_aliases, lower_names)
  match_idx <- match_idx[!is.na(match_idx)]
  if (!length(match_idx)) {
    return(NA_character_)
  }
  col_names[[match_idx[[1]]]]
}

build_analysis_column_map <- function(df) {
  specs <- analysis_column_specs()
  col_names <- names(df)
  out <- lapply(specs, function(spec) find_matching_column(col_names, spec$aliases))
  out
}

validate_analysis_columns <- function(df) {
  if (is.null(df)) {
    return(list(
      summary = data.frame(),
      warnings = character(),
      vis_type_ready = FALSE
    ))
  }

  specs <- analysis_column_specs()
  col_map <- build_analysis_column_map(df)

  summary_rows <- lapply(names(specs), function(key) {
    spec <- specs[[key]]
    found_col <- col_map[[key]] %||% NA_character_
    found_flag <- !is.na(found_col)
    data.frame(
      `Күтілетін өріс` = spec$label,
      `Қажетті` = ifelse(isTRUE(spec$required), "Иә", "Жоқ"),
      `Табылған баған` = ifelse(found_flag, found_col, "Табылмады"),
      `Күйі` = ifelse(found_flag, "Табылды", "Табылмады"),
      stringsAsFactors = FALSE
    )
  })

  vis_type_col <- col_map$vis_type
  vis_type_values <- if (!is.na(vis_type_col)) strip_empty_to_na(df[[vis_type_col]]) else rep(NA_character_, nrow(df))
  vis_type_ready <- !all(is.na(vis_type_values))

  warnings <- character()
  if (is.na(vis_type_col)) {
    warnings <- c(warnings, "CSV файлында vis_type немесе visualization_type бағаны табылмады.")
  } else if (!vis_type_ready) {
    warnings <- c(warnings, "Визуализация түрі бағаны табылды, бірақ оның мәндері бос.")
  }

  list(
    summary = dplyr::bind_rows(summary_rows),
    warnings = warnings,
    vis_type_ready = vis_type_ready,
    column_map = col_map
  )
}

safe_read_experiment_csv <- function(file_info) {
  if (is.null(file_info) || is.null(file_info$datapath) || !file.exists(file_info$datapath)) {
    return(list(data = NULL, error = "CSV файлы таңдалмаған."))
  }

  parsed <- tryCatch(
    readr::read_csv(
      file_info$datapath,
      show_col_types = FALSE,
      guess_max = 5000,
      progress = FALSE,
      locale = readr::locale(encoding = "UTF-8")
    ),
    error = function(e) e
  )

  if (inherits(parsed, "error")) {
    fallback <- tryCatch(
      utils::read.csv(file_info$datapath, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
      error = function(e) e
    )

    if (inherits(fallback, "error")) {
      return(list(
        data = NULL,
        error = paste("CSV файлын оқу мүмкін болмады.", parsed$message)
      ))
    }

    parsed <- fallback
  }

  parsed <- as.data.frame(parsed, stringsAsFactors = FALSE)
  if (!nrow(parsed)) {
    return(list(data = parsed, error = "Жүктелген CSV файлы бос."))
  }

  list(data = parsed, error = NULL)
}

pick_mapped_column <- function(df, col_name, default = NA) {
  n <- nrow(df)
  if (is.na(col_name) || !col_name %in% names(df)) {
    return(rep(default, n))
  }
  df[[col_name]]
}

normalize_analysis_dataset <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }

  col_map <- build_analysis_column_map(df)
  n <- nrow(df)

  participant_id <- strip_empty_to_na(pick_mapped_column(df, col_map$participant_id, NA_character_))
  question_id <- strip_empty_to_na(pick_mapped_column(df, find_matching_column(names(df), c("question_id")), NA_character_))
  task_id <- strip_empty_to_na(pick_mapped_column(df, find_matching_column(names(df), c("task_id")), NA_character_))
  task_family <- strip_empty_to_na(pick_mapped_column(df, find_matching_column(names(df), c("task_family")), NA_character_))
  block <- strip_empty_to_na(pick_mapped_column(df, find_matching_column(names(df), c("block")), NA_character_))
  vis_type <- strip_empty_to_na(pick_mapped_column(df, col_map$vis_type, NA_character_))
  answer <- strip_empty_to_na(pick_mapped_column(df, col_map$answer, NA_character_))
  is_correct <- safe_logical_coerce(pick_mapped_column(df, col_map$is_correct, NA))
  reaction_time_sec <- safe_numeric_coerce(pick_mapped_column(df, col_map$reaction_time_sec, NA))
  confidence <- safe_numeric_coerce(pick_mapped_column(df, col_map$confidence, NA))
  ease_rating <- safe_numeric_coerce(pick_mapped_column(df, col_map$ease_rating, NA))

  gestalt_principle <- strip_empty_to_na(pick_mapped_column(df, find_matching_column(names(df), c("gestalt_principle")), NA_character_))

  derived_block <- block
  if (all(is.na(derived_block))) {
    derived_block <- ifelse(
      !is.na(task_family) & tolower(task_family) == "analytics",
      "Analytics",
      ifelse(!is.na(task_family) | !is.na(gestalt_principle), "Gestalt", NA_character_)
    )
  }

  derived_task_family <- task_family
  if (all(is.na(derived_task_family)) && any(!is.na(derived_block))) {
    derived_task_family <- derived_block
  }
  if (all(is.na(derived_task_family)) && any(!is.na(gestalt_principle))) {
    derived_task_family <- gestalt_principle
  }

  out <- data.frame(
    participant_id = participant_id,
    question_id = question_id,
    task_id = task_id,
    block = derived_block,
    task_family = derived_task_family,
    vis_type = vis_type,
    answer = answer,
    is_correct = is_correct,
    reaction_time_sec = reaction_time_sec,
    confidence = confidence,
    ease_rating = ease_rating,
    stringsAsFactors = FALSE
  )

  out$row_kind <- ifelse(is.na(out$is_correct), "subjective", "objective")
  out$answer_num <- safe_numeric_coerce(out$answer)
  out$accuracy_num <- ifelse(is.na(out$is_correct), NA_real_, ifelse(out$is_correct, 1, 0))
  out$source_row <- seq_len(n)
  out
}

mean_or_na <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(NA_real_)
  }
  round(mean(x), digits)
}

median_or_na <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(NA_real_)
  }
  round(stats::median(x), digits)
}

generate_analysis_descriptives <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      overview = list(participants = 0L, rows = 0L, vis_types = 0L, objective_rows = 0L, subjective_rows = 0L),
      table = data.frame()
    ))
  }

  summary_df <- data %>%
    dplyr::group_by(vis_type) %>%
    dplyr::summarise(
      `Қатысушы саны` = dplyr::n_distinct(participant_id[!is.na(participant_id)]),
      `Жол саны` = dplyr::n(),
      `Орташа реакция уақыты (сек)` = mean_or_na(reaction_time_sec),
      `Медиана реакция уақыты (сек)` = median_or_na(reaction_time_sec),
      `Орташа сенім` = mean_or_na(confidence),
      `Орташа жеңілдік` = mean_or_na(ease_rating),
      `Дәлдік (%)` = {
        x <- accuracy_num[!is.na(accuracy_num)]
        if (!length(x)) NA_real_ else round(mean(x) * 100, 2)
      },
      .groups = "drop"
    ) %>%
    dplyr::arrange(vis_type)

  list(
    overview = list(
      participants = dplyr::n_distinct(data$participant_id[!is.na(data$participant_id)]),
      rows = nrow(data),
      vis_types = dplyr::n_distinct(data$vis_type[!is.na(data$vis_type)]),
      objective_rows = sum(data$row_kind == "objective", na.rm = TRUE),
      subjective_rows = sum(data$row_kind == "subjective", na.rm = TRUE)
    ),
    table = summary_df
  )
}

filter_analysis_dataset <- function(data, vis_types = NULL, task_families = NULL, blocks = NULL, row_scope = "all") {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }

  filtered <- data

  if (!is.null(vis_types) && length(vis_types) > 0) {
    filtered <- filtered[filtered$vis_type %in% vis_types, , drop = FALSE]
  }

  if (!is.null(task_families) && length(task_families) > 0) {
    filtered <- filtered[filtered$task_family %in% task_families, , drop = FALSE]
  }

  if (!is.null(blocks) && length(blocks) > 0) {
    filtered <- filtered[filtered$block %in% blocks, , drop = FALSE]
  }

  if (identical(row_scope, "objective")) {
    filtered <- filtered[filtered$row_kind == "objective", , drop = FALSE]
  } else if (identical(row_scope, "subjective")) {
    filtered <- filtered[filtered$row_kind == "subjective", , drop = FALSE]
  }

  filtered
}

prepare_analysis_metric_data <- function(data, metric) {
  metric_label <- switch(
    metric,
    subjective_answer = "субъективті жауап",
    reaction_time = "реакция уақыты",
    confidence = "сенім",
    ease_rating = "жеңілдік бағасы",
    accuracy = "дәлдік",
    "көрсеткіш"
  )

  if (is.null(data) || nrow(data) == 0) {
    return(list(
      data = data.frame(),
      metric_label = metric_label,
      error = "Талдауға арналған дерек табылмады."
    ))
  }

  prepared <- data

  if (identical(metric, "subjective_answer")) {
    prepared <- prepared[prepared$row_kind == "subjective", , drop = FALSE]
    prepared$analysis_value <- prepared$answer_num
    if (!nrow(prepared)) {
      return(list(
        data = data.frame(),
        metric_label = metric_label,
        error = "Сүзгіден кейін субъективті жауап жолдары қалмады."
      ))
    }
    if (!any(is.finite(prepared$analysis_value))) {
      return(list(
        data = data.frame(),
        metric_label = metric_label,
        error = "Субъективті жауап бағанын сандық форматқа түрлендіру мүмкін болмады."
      ))
    }
  } else if (identical(metric, "reaction_time")) {
    prepared$analysis_value <- prepared$reaction_time_sec
  } else if (identical(metric, "confidence")) {
    prepared$analysis_value <- prepared$confidence
  } else if (identical(metric, "ease_rating")) {
    prepared$analysis_value <- prepared$ease_rating
  } else if (identical(metric, "accuracy")) {
    prepared <- prepared[prepared$row_kind == "objective", , drop = FALSE]
    prepared$analysis_value <- prepared$accuracy_num
  } else {
    prepared$analysis_value <- NA_real_
  }

  prepared$analysis_value <- safe_numeric_coerce(prepared$analysis_value)
  prepared <- prepared[!is.na(prepared$vis_type) & is.finite(prepared$analysis_value), , drop = FALSE]

  if (!nrow(prepared)) {
    return(list(
      data = data.frame(),
      metric_label = metric_label,
      error = "Таңдалған көрсеткіш үшін жеткілікті сандық дерек табылмады."
    ))
  }

  list(
    data = prepared,
    metric_label = metric_label,
    error = NULL
  )
}

format_p_value <- function(p_value) {
  if (is.null(p_value) || length(p_value) != 1 || is.na(p_value)) {
    return("анықталмады")
  }
  if (p_value < 0.001) {
    return("< 0.001")
  }
  sprintf("%.4f", p_value)
}

run_shapiro_safe <- function(data, metric_label) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      table = data.frame(),
      interpretation = "Нормалдықты тексеруге дерек жеткіліксіз."
    ))
  }

  if (identical(metric_label, "дәлдік")) {
    return(list(
      table = data.frame(
        `Визуализация түрі` = "Барлығы",
        N = nrow(data),
        `Shapiro p` = NA_character_,
        Қорытынды = "Бинарлық көрсеткіш үшін бұл тексеріс ұсынылмайды.",
        stringsAsFactors = FALSE
      ),
      interpretation = "Дәлдік бинарлық сипатта болғандықтан, Shapiro-Wilk тестін қолдану орынды емес."
    ))
  }

  split_data <- split(data$analysis_value, data$vis_type)
  rows <- lapply(names(split_data), function(group_name) {
    values <- split_data[[group_name]]
    values <- values[is.finite(values)]

    if (length(values) < 3) {
      return(data.frame(
        `Визуализация түрі` = group_name,
        N = length(values),
        `Shapiro p` = NA_character_,
        Қорытынды = "Үлгі тым аз.",
        stringsAsFactors = FALSE
      ))
    }

    if (length(values) > 5000) {
      return(data.frame(
        `Визуализация түрі` = group_name,
        N = length(values),
        `Shapiro p` = NA_character_,
        Қорытынды = "Үлгі тым үлкен, Shapiro-Wilk қолданылмады.",
        stringsAsFactors = FALSE
      ))
    }

    test_result <- tryCatch(
      stats::shapiro.test(values),
      error = function(e) e
    )

    if (inherits(test_result, "error")) {
      return(data.frame(
        `Визуализация түрі` = group_name,
        N = length(values),
        `Shapiro p` = NA_character_,
        Қорытынды = "Тексеру орындалмады.",
        stringsAsFactors = FALSE
      ))
    }

    data.frame(
      `Визуализация түрі` = group_name,
      N = length(values),
      `Shapiro p` = format_p_value(test_result$p.value),
      Қорытынды = ifelse(
        test_result$p.value < 0.05,
        "Қалыптылықтан ауытқу бар.",
        "Қалыптылықтан айқын ауытқу байқалмады."
      ),
      stringsAsFactors = FALSE
    )
  })

  table_df <- dplyr::bind_rows(rows)
  significant_flags <- grepl("ауытқу бар", table_df$Қорытынды %||% "")
  valid_rows <- !is.na(table_df$`Shapiro p`)

  interpretation <- if (!any(valid_rows)) {
    "Нормалдықты тексеру үшін жеткілікті топтар табылмады."
  } else if (any(significant_flags, na.rm = TRUE)) {
    "Кемінде бір топта қалыптылық шартынан ауытқу бар, сондықтан параметрлік емес талдауды да қарастырған жөн."
  } else {
    "Қалыптылықтан айқын ауытқу байқалмады, сондықтан параметрлік нәтижелерді қарастыруға болады."
  }

  list(table = table_df, interpretation = interpretation)
}

group_metric_summary <- function(data) {
  data %>%
    dplyr::group_by(vis_type) %>%
    dplyr::summarise(
      N = dplyr::n(),
      Орташа = mean_or_na(analysis_value),
      Медиана = median_or_na(analysis_value),
      .groups = "drop"
    ) %>%
    dplyr::arrange(vis_type)
}

run_t_test_analysis <- function(data, metric_label) {
  if (dplyr::n_distinct(data$vis_type) != 2) {
    return(list(
      status = "warning",
      raw_output = "t-test жүргізу үшін дәл екі визуализация түрі таңдалуы керек.",
      interpretation = "Екі топ таңдалмағандықтан, t-test орындалмады.",
      group_summary = group_metric_summary(data),
      result_table = data.frame(),
      posthoc_table = data.frame()
    ))
  }

  group_counts <- table(data$vis_type)
  if (any(group_counts < 2)) {
    return(list(
      status = "warning",
      raw_output = "Кемінде бір топта бақылаулар саны жеткіліксіз.",
      interpretation = "t-test жүргізу үшін әр топта кемінде екі сандық мән болуы қажет.",
      group_summary = group_metric_summary(data),
      result_table = data.frame(),
      posthoc_table = data.frame()
    ))
  }

  test_result <- tryCatch(
    stats::t.test(analysis_value ~ vis_type, data = data),
    error = function(e) e
  )

  if (inherits(test_result, "error")) {
    return(list(
      status = "error",
      raw_output = test_result$message,
      interpretation = "t-test орындалмады.",
      group_summary = group_metric_summary(data),
      result_table = data.frame(),
      posthoc_table = data.frame()
    ))
  }

  result_table <- data.frame(
    Тест = "t-test",
    Көрсеткіш = metric_label,
    `p-мәні` = format_p_value(test_result$p.value),
    `t-статистика` = round(unname(test_result$statistic), 4),
    `Еркіндік дәрежесі` = round(unname(test_result$parameter), 4),
    stringsAsFactors = FALSE
  )

  interpretation <- if (is.finite(test_result$p.value) && test_result$p.value < 0.05) {
    paste0("p < 0.05 болғандықтан, таңдалған визуализация түрлері арасында ", metric_label, " бойынша статистикалық мәнді айырмашылық бар.")
  } else {
    paste0("p > 0.05 болғандықтан, таңдалған визуализация түрлері арасында ", metric_label, " бойынша мәнді айырмашылық анықталмады.")
  }

  list(
    status = "success",
    raw_output = capture.output(print(test_result)),
    interpretation = interpretation,
    group_summary = group_metric_summary(data),
    result_table = result_table,
    posthoc_table = data.frame()
  )
}

run_anova_analysis <- function(data, metric_label) {
  group_counts <- table(data$vis_type)
  if (length(group_counts) < 3) {
    return(list(
      status = "warning",
      raw_output = "ANOVA жүргізу үшін кемінде үш визуализация түрі қажет.",
      interpretation = "ANOVA орындалмады.",
      group_summary = group_metric_summary(data),
      result_table = data.frame(),
      posthoc_table = data.frame()
    ))
  }

  if (any(group_counts < 2)) {
    return(list(
      status = "warning",
      raw_output = "Кемінде бір топта бақылау саны жеткіліксіз.",
      interpretation = "ANOVA жүргізу үшін әр топта кемінде екі сандық мән болуы қажет.",
      group_summary = group_metric_summary(data),
      result_table = data.frame(),
      posthoc_table = data.frame()
    ))
  }

  model <- tryCatch(
    stats::aov(analysis_value ~ vis_type, data = data),
    error = function(e) e
  )

  if (inherits(model, "error")) {
    return(list(
      status = "error",
      raw_output = model$message,
      interpretation = "ANOVA орындалмады.",
      group_summary = group_metric_summary(data),
      result_table = data.frame(),
      posthoc_table = data.frame()
    ))
  }

  anova_summary <- summary(model)[[1]]
  anova_table <- data.frame(
    Термин = rownames(anova_summary),
    anova_summary,
    row.names = NULL,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  p_value <- suppressWarnings(as.numeric(anova_summary[1, "Pr(>F)"]))

  posthoc_table <- data.frame()
  posthoc_note <- NULL
  raw_output <- capture.output(print(summary(model)))

  if (is.finite(p_value) && p_value < 0.05) {
    tukey_result <- tryCatch(
      stats::TukeyHSD(model),
      error = function(e) e
    )

    if (!inherits(tukey_result, "error")) {
      tukey_df <- data.frame(
        Салыстыру = rownames(tukey_result$vis_type),
        tukey_result$vis_type,
        row.names = NULL,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      posthoc_table <- tukey_df
      raw_output <- c(raw_output, "", "TukeyHSD:", capture.output(print(tukey_result)))

      significant_pairs <- tukey_df$Салыстыру[suppressWarnings(as.numeric(tukey_df$`p adj`)) < 0.05]
      if (length(significant_pairs)) {
        posthoc_note <- paste0(
          "Tukey post-hoc нәтижесі бойынша ",
          paste(significant_pairs, collapse = ", "),
          " жұптары арасында айырмашылық бар."
        )
      } else {
        posthoc_note <- "Tukey post-hoc нәтижесі бойынша нақты жұптық айырмашылық анықталмады."
      }
    }
  }

  interpretation <- if (is.finite(p_value) && p_value < 0.05) {
    paste0("ANOVA нәтижесі бойынша визуализация түрі ", metric_label, " көрсеткішіне әсер етеді.")
  } else {
    paste0("ANOVA нәтижесі бойынша визуализация түрлері арасында ", metric_label, " көрсеткіші бойынша мәнді айырмашылық анықталмады.")
  }

  if (!is.null(posthoc_note)) {
    interpretation <- c(interpretation, posthoc_note)
  }

  list(
    status = "success",
    raw_output = raw_output,
    interpretation = interpretation,
    group_summary = group_metric_summary(data),
    result_table = anova_table,
    posthoc_table = posthoc_table
  )
}

run_kruskal_analysis <- function(data, metric_label) {
  if (dplyr::n_distinct(data$vis_type) < 2) {
    return(list(
      status = "warning",
      raw_output = "Kruskal-Wallis тесті үшін кемінде екі визуализация түрі қажет.",
      interpretation = "Параметрлік емес тест орындалмады.",
      group_summary = group_metric_summary(data),
      result_table = data.frame(),
      posthoc_table = data.frame()
    ))
  }

  test_result <- tryCatch(
    stats::kruskal.test(analysis_value ~ vis_type, data = data),
    error = function(e) e
  )

  if (inherits(test_result, "error")) {
    return(list(
      status = "error",
      raw_output = test_result$message,
      interpretation = "Kruskal-Wallis тесті орындалмады.",
      group_summary = group_metric_summary(data),
      result_table = data.frame(),
      posthoc_table = data.frame()
    ))
  }

  result_table <- data.frame(
    Тест = "Kruskal-Wallis",
    Көрсеткіш = metric_label,
    `p-мәні` = format_p_value(test_result$p.value),
    `Хи-квадрат` = round(unname(test_result$statistic), 4),
    `Еркіндік дәрежесі` = round(unname(test_result$parameter), 4),
    stringsAsFactors = FALSE
  )

  interpretation <- if (is.finite(test_result$p.value) && test_result$p.value < 0.05) {
    paste0("p < 0.05 болғандықтан, таңдалған визуализация түрлері арасында ", metric_label, " бойынша статистикалық мәнді айырмашылық бар.")
  } else {
    paste0("p > 0.05 болғандықтан, таңдалған визуализациялар арасында ", metric_label, " бойынша мәнді айырмашылық анықталмады.")
  }

  list(
    status = "success",
    raw_output = capture.output(print(test_result)),
    interpretation = interpretation,
    group_summary = group_metric_summary(data),
    result_table = result_table,
    posthoc_table = data.frame()
  )
}

run_analysis_test <- function(data, metric_label, robust = FALSE) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      status = "warning",
      raw_output = "Статистикалық тест жүргізуге дерек жеткіліксіз.",
      interpretation = "Тест жүргізілмеді.",
      group_summary = data.frame(),
      result_table = data.frame(),
      posthoc_table = data.frame()
    ))
  }

  vis_type_count <- dplyr::n_distinct(data$vis_type)
  if (vis_type_count < 2) {
    return(list(
      status = "warning",
      raw_output = "Кемінде екі визуализация түрін таңдаңыз.",
      interpretation = "Салыстыру үшін визуализация түрлері жеткіліксіз.",
      group_summary = group_metric_summary(data),
      result_table = data.frame(),
      posthoc_table = data.frame()
    ))
  }

  if (isTRUE(robust)) {
    return(run_kruskal_analysis(data, metric_label))
  }

  if (vis_type_count == 2) {
    run_t_test_analysis(data, metric_label)
  } else {
    run_anova_analysis(data, metric_label)
  }
}

as_export_section <- function(df, section_name) {
  if (is.null(df) || !nrow(df)) {
    return(data.frame())
  }
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  out$section <- section_name
  out
}

build_analysis_test_export <- function(test_result, normality_result = NULL) {
  pieces <- list(
    as_export_section(test_result$group_summary %||% data.frame(), "group_summary"),
    as_export_section(test_result$result_table %||% data.frame(), "test_result"),
    as_export_section(test_result$posthoc_table %||% data.frame(), "posthoc"),
    as_export_section(normality_result$table %||% data.frame(), "normality")
  )

  pieces <- pieces[vapply(pieces, nrow, integer(1)) > 0]
  if (!length(pieces)) {
    return(data.frame(
      section = "message",
      note = "Экспортқа қолайлы статистикалық нәтиже әзірге жоқ.",
      stringsAsFactors = FALSE
    ))
  }

  dplyr::bind_rows(pieces)
}

analysis_empty_plot <- function(message_text) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message_text, color = COLORS["slate"], size = 5) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_void()
}

plot_subjective_score_boxplot <- function(data) {
  subjective_data <- data[data$row_kind == "subjective" & is.finite(data$answer_num) & !is.na(data$vis_type), , drop = FALSE]
  if (!nrow(subjective_data)) {
    return(analysis_empty_plot("Субъективті жауап дерегі табылмады."))
  }

  ggplot2::ggplot(subjective_data, ggplot2::aes(x = vis_type, y = answer_num, fill = vis_type)) +
    ggplot2::geom_boxplot(alpha = 0.9, outlier.alpha = 0.55) +
    ggplot2::scale_fill_manual(values = rep(COLORS[c("blue", "sage", "gold", "coral", "plum")], length.out = dplyr::n_distinct(subjective_data$vis_type))) +
    ggplot2::labs(x = NULL, y = "Субъективті баға") +
    experiment_theme() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 18, hjust = 1)
    )
}

plot_confidence_mean_bar <- function(data) {
  summary_df <- data %>%
    dplyr::filter(!is.na(vis_type), is.finite(confidence)) %>%
    dplyr::group_by(vis_type) %>%
    dplyr::summarise(mean_confidence = mean(confidence), .groups = "drop")

  if (!nrow(summary_df)) {
    return(analysis_empty_plot("Сенім көрсеткіші табылмады."))
  }

  plot_bar_generic(
    data.frame(
      category = summary_df$vis_type,
      value = summary_df$mean_confidence,
      fill = rep(unname(COLORS["blue"]), nrow(summary_df)),
      stringsAsFactors = FALSE
    ),
    show_value_labels = TRUE,
    y_label = "Орташа сенім"
  )
}

plot_accuracy_bar <- function(data) {
  summary_df <- data %>%
    dplyr::filter(!is.na(vis_type), !is.na(accuracy_num)) %>%
    dplyr::group_by(vis_type) %>%
    dplyr::summarise(accuracy_pct = mean(accuracy_num) * 100, .groups = "drop")

  if (!nrow(summary_df)) {
    return(analysis_empty_plot("Дәлдікке қатысты объективті дерек табылмады."))
  }

  plot_bar_generic(
    data.frame(
      category = summary_df$vis_type,
      value = summary_df$accuracy_pct,
      fill = rep(unname(COLORS["sage"]), nrow(summary_df)),
      stringsAsFactors = FALSE
    ),
    show_value_labels = TRUE,
    y_label = "Дәлдік (%)"
  )
}

plot_reaction_time_boxplot <- function(data) {
  rt_data <- data[data$reaction_time_sec > 0 & is.finite(data$reaction_time_sec) & !is.na(data$vis_type), , drop = FALSE]
  if (!nrow(rt_data)) {
    return(analysis_empty_plot("Реакция уақыты дерегі табылмады."))
  }

  ggplot2::ggplot(rt_data, ggplot2::aes(x = vis_type, y = reaction_time_sec, fill = vis_type)) +
    ggplot2::geom_boxplot(alpha = 0.9, outlier.alpha = 0.55) +
    ggplot2::scale_fill_manual(values = rep(COLORS[c("coral", "gold", "blue", "sage", "plum")], length.out = dplyr::n_distinct(rt_data$vis_type))) +
    ggplot2::labs(x = NULL, y = "Реакция уақыты (сек)") +
    experiment_theme() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 18, hjust = 1)
    )
}
