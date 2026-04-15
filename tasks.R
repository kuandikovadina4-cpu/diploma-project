make_task <- function(
    task_id,
    task_family,
    principle = NA_character_,
    visualization_type,
    question_type,
    question_text,
    answer_choices = NULL,
    correct_answer = NA_character_,
    renderer = "single_plot",
    chart_count = 1L,
    plot_fun = NULL,
    plot_funs = NULL,
    panel_labels = NULL,
    panels = NULL
) {
  list(
    task_id = task_id,
    question_id = paste0("Q_", task_id),
    task_family = task_family,
    principle = principle,
    visualization_type = visualization_type,
    question_type = question_type,
    question_text = question_text,
    answer_choices = answer_choices,
    correct_answer = correct_answer,
    renderer = renderer,
    chart_count = chart_count,
    plot_fun = plot_fun,
    plot_funs = plot_funs,
    panel_labels = panel_labels,
    panels = panels
  )
}

validate_task_bank <- function(task_bank) {
  issues <- character()
  regular_analytic_types <- c("Bar chart", "Treemap", "Heatmap", "Bubble chart")
  
  if (length(task_bank) != 32) {
    issues <- c(issues, sprintf("Expected 32 total tasks, found %s.", length(task_bank)))
  }
  
  task_ids <- vapply(task_bank, function(task) task$task_id, character(1))
  question_ids <- vapply(task_bank, function(task) task$question_id, character(1))
  
  if (anyDuplicated(task_ids)) {
    issues <- c(issues, "Task IDs must be unique.")
  }
  if (anyDuplicated(question_ids)) {
    issues <- c(issues, "Question IDs must be unique.")
  }
  
  special_tasks <- Filter(function(task) identical(task$renderer, "multi_panel_individual"), task_bank)
  regular_tasks <- Filter(function(task) !identical(task$renderer, "multi_panel_individual"), task_bank)
  
  if (length(regular_tasks) != 30) {
    issues <- c(issues, sprintf("Expected 30 regular tasks, found %s.", length(regular_tasks)))
  }
  if (length(special_tasks) != 2) {
    issues <- c(issues, sprintf("Expected 2 special multi-panel tasks, found %s.", length(special_tasks)))
  }
  
  regular_families <- vapply(regular_tasks, function(task) task$task_family, character(1))
  regular_principles <- vapply(regular_tasks, function(task) task$principle %||% NA_character_, character(1))
  regular_visuals <- vapply(regular_tasks, function(task) task$visualization_type, character(1))
  
  regular_gestalt_n <- sum(regular_principles %in% GESTALT_PRINCIPLES)
  regular_analytic_n <- sum(regular_families == ANALYTIC_FAMILY)
  
  if (regular_gestalt_n != 10) {
    issues <- c(issues, sprintf("Expected 10 regular gestalt tasks, found %s.", regular_gestalt_n))
  }
  if (regular_analytic_n != 20) {
    issues <- c(issues, sprintf("Expected 20 regular analytic tasks, found %s.", regular_analytic_n))
  }
  
  gestalt_counts <- table(factor(regular_principles[regular_principles %in% GESTALT_PRINCIPLES], levels = GESTALT_PRINCIPLES))
  if (any(gestalt_counts != 2)) {
    issues <- c(
      issues,
      paste(
        "Each gestalt principle must contain exactly 2 regular tasks.",
        paste(sprintf("%s=%s", names(gestalt_counts), as.integer(gestalt_counts)), collapse = ", ")
      )
    )
  }
  
  analytic_visuals <- regular_visuals[regular_families == ANALYTIC_FAMILY]
  if (!all(analytic_visuals %in% regular_analytic_types)) {
    invalid_visuals <- unique(analytic_visuals[!analytic_visuals %in% regular_analytic_types])
    issues <- c(
      issues,
      paste("Regular analytic tasks must use only Bar chart, Treemap, Heatmap, or Bubble chart:", paste(invalid_visuals, collapse = ", "))
    )
  }
  
  analytic_counts <- table(factor(analytic_visuals, levels = regular_analytic_types))
  if (any(analytic_counts != 5)) {
    issues <- c(
      issues,
      paste(
        "Each regular analytic family must contain exactly 5 tasks.",
        paste(sprintf("%s=%s", names(analytic_counts), as.integer(analytic_counts)), collapse = ", ")
      )
    )
  }
  
  special_ids <- sort(vapply(special_tasks, function(task) task$task_id, character(1)))
  if (!identical(special_ids, c("SPA_01", "SPG_01"))) {
    issues <- c(issues, paste("Special task IDs must be SPA_01 and SPG_01. Found:", paste(special_ids, collapse = ", ")))
  }
  
  for (task in task_bank) {
    if (!task$renderer %in% VALID_RENDERERS) {
      issues <- c(issues, paste("Unsupported renderer:", task$task_id))
    }
    if (!nzchar(task$question_text)) {
      issues <- c(issues, paste("Missing question text:", task$task_id))
    }
    
    if (identical(task$renderer, "single_plot")) {
      if (length(task$answer_choices) < 2) {
        issues <- c(issues, paste("Task needs at least two answer choices:", task$task_id))
      }
      if (!task$correct_answer %in% task$answer_choices) {
        issues <- c(issues, paste("Correct answer missing from choices:", task$task_id))
      }
      if (!is.function(task$plot_fun)) {
        issues <- c(issues, paste("single_plot task missing plot_fun:", task$task_id))
      }
    }
    
    if (identical(task$renderer, "multi_panel_individual")) {
      if (is.null(task$panels) || length(task$panels) != 4) {
        issues <- c(issues, paste("multi_panel_individual task must have exactly 4 panels:", task$task_id))
      } else {
        for (panel in task$panels) {
          required_fields <- c("panel_id", "panel_label", "answer_choices", "correct_answer", "plot_fun", "panel_order")
          if (!all(required_fields %in% names(panel))) {
            issues <- c(issues, paste("Panel definition is incomplete:", task$task_id))
            next
          }
          if (length(panel$answer_choices) < 2) {
            issues <- c(issues, paste("Panel needs at least two answer choices:", task$task_id, panel$panel_id))
          }
          if (!panel$correct_answer %in% panel$answer_choices) {
            issues <- c(issues, paste("Panel correct answer missing from choices:", task$task_id, panel$panel_id))
          }
          if (!is.function(panel$plot_fun)) {
            issues <- c(issues, paste("Panel plot_fun missing:", task$task_id, panel$panel_id))
          }
        }
      }
    }
  }
  
  if (length(issues) > 0) {
    stop(paste(c("Task bank validation failed:", issues), collapse = "\n"), call. = FALSE)
  }
  
  invisible(TRUE)
}

ANALYTIC_STIM_DATA <- data.frame(
  Category = c("A", "B", "C", "D", "E"),
  Value = c(40, 70, 55, 90, 30),
  stringsAsFactors = FALSE
)

strip_titles <- function(p) {
  p +
    ggplot2::labs(title = NULL, subtitle = NULL) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank()
    )
}

plot_reference_closure <- function(show_titles = TRUE) {
  rects <- data.frame(
    xmin = c(0.6, 3.0, 5.4),
    xmax = c(2.4, 4.8, 7.2),
    ymin = c(1.0, 1.0, 1.0),
    ymax = c(3.6, 3.6, 3.6),
    fill = c("#9ecae1", "#a1d99b", "#fdae6b"),
    stringsAsFactors = FALSE
  )
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = rects,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = rects$fill,
      color = NA,
      alpha = 0.95
    ) +
    ggplot2::geom_segment(ggplot2::aes(x = 0.6, xend = 2.4, y = 1.0, yend = 1.0), linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 0.6, xend = 0.6, y = 1.0, yend = 3.6), linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 2.4, xend = 2.4, y = 1.0, yend = 3.6), linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 0.8, xend = 2.2, y = 3.6, yend = 3.6), linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 3.0, xend = 4.8, y = 1.0, yend = 1.0), linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 3.0, xend = 3.0, y = 1.0, yend = 3.6), linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 4.8, xend = 4.8, y = 1.0, yend = 3.6), linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 3.3, xend = 4.2, y = 3.6, yend = 3.6), linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 5.4, xend = 7.2, y = 1.0, yend = 1.0), linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 5.4, xend = 5.4, y = 1.0, yend = 3.6), linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 7.2, xend = 7.2, y = 1.0, yend = 3.6), linewidth = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 5.4, xend = 6.8, y = 3.6, yend = 3.6), linewidth = 1) +
    ggplot2::annotate("text", x = c(1.5, 3.9, 6.3), y = 2.3, label = c("A", "B", "C"), color = "white", size = 8) +
    ggplot2::coord_equal() +
    ggplot2::xlim(0, 8) +
    ggplot2::ylim(0.5, 4.2) +
    ggplot2::theme_void(base_size = 14)
  
  strip_titles(p)
}

plot_reference_similarity <- function(show_titles = TRUE) {
  df <- data.frame(
    x = c(1, 4, 7, 2, 5, 8, 3, 6, 9),
    y = c(3, 3, 3, 2, 2, 2, 1, 1, 1),
    grp = c(
      "Синяя", "Синяя", "Синяя",
      "Оранжевая", "Оранжевая", "Оранжевая",
      "Зелёная", "Зелёная", "Зелёная"
    ),
    stringsAsFactors = FALSE
  )
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, color = grp)) +
    ggplot2::geom_point(size = 7) +
    ggplot2::scale_color_manual(values = c("Синяя" = "#3182bd", "Оранжевая" = "#fd8d3c", "Зелёная" = "#31a354")) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = NULL, y = NULL)
  
  strip_titles(p)
}

plot_reference_proximity <- function(show_titles = TRUE) {
  set.seed(123)
  
  cluster1 <- data.frame(
    x = rnorm(12, mean = 1.0, sd = 0.12),
    y = rnorm(12, mean = 1.0, sd = 0.12)
  )
  cluster2 <- data.frame(
    x = rnorm(12, mean = 3.2, sd = 0.12),
    y = rnorm(12, mean = 3.0, sd = 0.12)
  )
  cluster3 <- data.frame(
    x = rnorm(12, mean = 5.3, sd = 0.12),
    y = rnorm(12, mean = 1.0, sd = 0.12)
  )
  
  df <- dplyr::bind_rows(cluster1, cluster2, cluster3)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point(size = 4, color = "steelblue") +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(x = NULL, y = NULL)
  
  strip_titles(p)
}

plot_reference_symmetry <- function(show_titles = TRUE) {
  df <- data.frame(
    x = c(-3, -2, -1, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 1, 2, 3),
    y = c(3, 2, 1, 1.5, 2.5, 3.2, 3.2, 2.5, 1.5, 1, 2, 3),
    stringsAsFactors = FALSE
  )
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point(size = 6, color = "steelblue") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 1) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(x = NULL, y = NULL)
  
  strip_titles(p)
}

plot_reference_continuity <- function(show_titles = TRUE) {
  line_a <- data.frame(
    x = c(1, 1.8, 2.6, 3.4, 4.2, 5.0, 5.8, 6.6),
    y = c(1, 1.5, 2.0, 2.6, 3.1, 3.6, 4.1, 4.6),
    stringsAsFactors = FALSE
  )
  
  line_b <- data.frame(
    x = c(1.2, 2.2, 3.1, 4.0, 5.0, 5.9),
    y = c(4.2, 3.7, 3.1, 2.6, 2.1, 1.6),
    stringsAsFactors = FALSE
  )
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = line_a, ggplot2::aes(x, y), color = "orange", linewidth = 1.5) +
    ggplot2::geom_point(data = line_a, ggplot2::aes(x, y), size = 5, color = "steelblue") +
    ggplot2::geom_point(data = line_b, ggplot2::aes(x, y), size = 4, color = "gray50") +
    ggplot2::annotate("text", x = 1.15, y = 0.8, label = "A", color = "orange", fontface = "bold", size = 5) +
    ggplot2::annotate("text", x = 1.15, y = 4.45, label = "B", color = "gray35", fontface = "bold", size = 5) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(x = NULL, y = NULL)
  
  strip_titles(p)
}

plot_reference_bar <- function(df = ANALYTIC_STIM_DATA, show_titles = TRUE) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Category, y = Value)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(label = Value), vjust = -0.4, size = 5) +
    ggplot2::ylim(0, max(df$Value) * 1.15) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(x = "Санат", y = "Мән")
  
  strip_titles(p)
}

plot_reference_treemap <- function(df = ANALYTIC_STIM_DATA, show_titles = TRUE) {
  rect_df <- data.frame(
    Category = c("D", "B", "C", "A", "E"),
    Value = c(90, 70, 55, 40, 30),
    xmin = c(0.0, 31.6, 69.9, 31.6, 70.7),
    xmax = c(31.6, 69.9, 100.0, 70.7, 100.0),
    ymin = c(0.0, 35.9, 35.9, 0.0, 0.0),
    ymax = c(100.0, 100.0, 100.0, 35.9, 35.9),
    stringsAsFactors = FALSE
  )
  rect_df$label <- paste(rect_df$Category, rect_df$Value, sep = "\n")
  rect_df$fill <- c("#08306b", "#2171b5", "#4292c6", "#6baed6", "#9ecae1")
  rect_df$text_color <- c("#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#1f2937")
  
  p <- ggplot2::ggplot(rect_df) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      color = "white",
      linewidth = 1
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = (xmin + xmax) / 2,
        y = (ymin + ymax) / 2,
        label = label,
        color = text_color
      ),
      lineheight = 0.9,
      fontface = "bold",
      size = 4
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::theme_void(base_size = 14)
  
  strip_titles(p)
}

plot_reference_heatmap <- function(df = ANALYTIC_STIM_DATA, show_titles = TRUE) {
  df2 <- df
  df2$Row <- "Values"
  
  p <- ggplot2::ggplot(df2, ggplot2::aes(x = Category, y = Row, fill = Value)) +
    ggplot2::geom_tile(color = "white", linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = Value), size = 5) +
    ggplot2::scale_fill_gradient(low = "yellow", high = "red") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "Санат")
  
  strip_titles(p)
}

plot_reference_bubble <- function(df = ANALYTIC_STIM_DATA, show_titles = TRUE) {
  bubble_breaks <- sort(unique(df$Value))
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Category, y = 1, size = Value)) +
    ggplot2::geom_point(alpha = 0.7, color = "tomato") +
    ggplot2::geom_text(ggplot2::aes(label = Value), nudge_y = -0.42, vjust = 1, size = 4.5) +
    ggplot2::scale_size(
      range = c(8, 24),
      breaks = bubble_breaks,
      labels = bubble_breaks,
      name = NULL
    ) +
    ggplot2::scale_y_continuous(limits = c(0.35, 1.35), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 10, 20, 10),
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "Санат")
  
  strip_titles(p)
}

build_task_bank <- function() {
  tasks <- list()
  
  add_task <- function(task) {
    tasks[[length(tasks) + 1]] <<- task
  }
  
  single_task <- function(
    task_id,
    task_family,
    principle = NA_character_,
    visualization_type,
    question_type,
    question_text,
    answer_choices,
    correct_answer,
    plot_fun
  ) {
    make_task(
      task_id = task_id,
      task_family = task_family,
      principle = principle,
      visualization_type = visualization_type,
      question_type = question_type,
      question_text = question_text,
      answer_choices = answer_choices,
      correct_answer = correct_answer,
      renderer = "single_plot",
      chart_count = 1L,
      plot_fun = plot_fun
    )
  }
  
  multi_panel_task <- function(
    task_id,
    task_family,
    principle,
    visualization_type,
    question_type,
    question_text,
    panels
  ) {
    make_task(
      task_id = task_id,
      task_family = task_family,
      principle = principle,
      visualization_type = visualization_type,
      question_type = question_type,
      question_text = question_text,
      answer_choices = panels[[1]]$answer_choices,
      correct_answer = NA_character_,
      renderer = "multi_panel_individual",
      chart_count = length(panels),
      panels = panels
    )
  }
  
  # ---- Regular Gestalt: 10 (exactly based on reference code, without likert-only screens) ----
  add_task(single_task(
    "CLO_01", "Closure", "Closure", "Gestalt plot", "Region count",
    "Қанша логикалық аймақ анықталады?",
    c("1", "2", "3", "4"),
    "3",
    function() plot_reference_closure(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "CLO_02", "Closure", "Closure", "Gestalt plot", "Most complete area",
    "Қай аймақ ең толық болып қабылданады?",
    c("A", "B", "C"),
    "B",
    function() plot_reference_closure(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "SIM_01", "Similarity", "Similarity", "Gestalt plot", "Group count",
    "Қанша топ байқалады?",
    c("1", "2", "3", "4"),
    "3",
    function() plot_reference_similarity(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "SIM_02", "Similarity", "Similarity", "Gestalt plot", "Grouping criterion",
    "Нысандар қай белгі бойынша топтасқан?",
    c("Түсі бойынша", "Өлшемі бойынша", "Қашықтығы бойынша", "Пішіні бойынша"),
    "Түсі бойынша",
    function() plot_reference_similarity(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "PROX_01", "Proximity", "Proximity", "Gestalt plot", "Cluster count",
    "Қанша топ анықталады?",
    c("1", "2", "3", "4"),
    "3",
    function() plot_reference_proximity(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "PROX_02", "Proximity", "Proximity", "Gestalt plot", "Central group",
    "Қай топ орталыққа ең жақын орналасқан?",
    c("Сол жақ", "Орталық", "Оң жақ"),
    "Орталық",
    function() plot_reference_proximity(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "SYM_01", "Symmetry", "Symmetry", "Gestalt plot", "More ordered structure",
    "Қай құрылым неғұрлым реттелген болып көрінеді?",
    c("Симметриялы", "Симметриясыз", "Бірдей"),
    "Симметриялы",
    function() plot_reference_symmetry(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "SYM_02", "Symmetry", "Symmetry", "Gestalt plot", "Mirrored parts",
    "Қай бөлік екіншісін симметриялы түрде қайталайды?",
    c("Сол және оң", "Жоғарғы және төменгі", "Тек ортасы"),
    "Сол және оң",
    function() plot_reference_symmetry(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "CONT_01", "Continuity", "Continuity", "Gestalt plot", "Unified line",
    "Қай нүктелер тізбегі үздіксіз сызық ретінде қабылданады?",
    c("A сызығы", "B сызығы", "Екеуі де", "Ешқайсысы"),
    "A сызығы",
    function() plot_reference_continuity(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "CONT_02", "Continuity", "Continuity", "Gestalt plot", "Main line count",
    "Қанша негізгі сызық анықталады?",
    c("1", "2", "3"),
    "1",
    function() plot_reference_continuity(show_titles = TRUE)
  ))
  
  # ---- Special Gestalt Multi-panel: extra 1 ----
  add_task(multi_panel_task(
    "SPG_01", "Gestalt Special", "Mixed", "4-panel gestalt reference", "Mixed gestalt comparison",
    "Төмендегі төрт суреттің әрқайсысы үшін қай жауап дұрыс?",
    panels = list(
      list(
        panel_id = "spg_closure",
        panel_label = "Қанша логикалық аймақ анықталады?",
        panel_order = 1L,
        answer_choices = c("1", "2", "3", "4"),
        correct_answer = "3",
        plot_fun = function() plot_reference_closure(show_titles = FALSE)
      ),
      list(
        panel_id = "spg_similarity",
        panel_label = "Нысандар қай белгі бойынша топтасқан?",
        panel_order = 2L,
        answer_choices = c("Түсі бойынша", "Өлшемі бойынша", "Қашықтығы бойынша", "Пішіні бойынша"),
        correct_answer = "Түсі бойынша",
        plot_fun = function() plot_reference_similarity(show_titles = FALSE)
      ),
      list(
        panel_id = "spg_proximity",
        panel_label = "Қай топ орталыққа ең жақын орналасқан?",
        panel_order = 3L,
        answer_choices = c("Сол жақ", "Орталық", "Оң жақ"),
        correct_answer = "Орталық",
        plot_fun = function() plot_reference_proximity(show_titles = FALSE)
      ),
      list(
        panel_id = "spg_continuity",
        panel_label = "Қай нүктелер тізбегі үздіксіз сызық ретінде қабылданады?",
        panel_order = 4L,
        answer_choices = c("A сызығы", "B сызығы", "Екеуі де", "Ешқайсысы"),
        correct_answer = "A сызығы",
        plot_fun = function() plot_reference_continuity(show_titles = FALSE)
      )
    )
  ))
  
  # ---- Regular Analytics: 20 (4 families x 5 questions exactly like reference) ----
  add_task(single_task(
    "BAR_01", ANALYTIC_FAMILY, NA_character_, "Bar chart", "Maximum",
    "Қай санаттың мәні ең жоғары?",
    c("A", "B", "C", "D", "E"),
    "D",
    function() plot_reference_bar(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "BAR_02", ANALYTIC_FAMILY, NA_character_, "Bar chart", "Minimum",
    "Қай санаттың мәні ең төмен?",
    c("A", "B", "C", "D", "E"),
    "E",
    function() plot_reference_bar(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "BAR_03", ANALYTIC_FAMILY, NA_character_, "Bar chart", "Compare",
    "Қай санаттың мәні жоғары: B әлде C?",
    c("B", "C", "Бірдей"),
    "B",
    function() plot_reference_bar(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "BAR_04", ANALYTIC_FAMILY, NA_character_, "Bar chart", "Second highest",
    "Қай санаттың мәні максималды мәннен кейін екінші орында тұр?",
    c("A", "B", "C", "D", "E"),
    "B",
    function() plot_reference_bar(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "BAR_05", ANALYTIC_FAMILY, NA_character_, "Bar chart", "Difference",
    "D санатының мәні B санатынан қанша бірлікке жоғары?",
    c("10", "15", "20", "25", "30"),
    "20",
    function() plot_reference_bar(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "TREE_01", ANALYTIC_FAMILY, NA_character_, "Treemap", "Maximum",
    "Қай санаттың мәні ең жоғары?",
    c("A", "B", "C", "D", "E"),
    "D",
    function() plot_reference_treemap(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "TREE_02", ANALYTIC_FAMILY, NA_character_, "Treemap", "Minimum",
    "Қай санаттың мәні ең төмен?",
    c("A", "B", "C", "D", "E"),
    "E",
    function() plot_reference_treemap(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "TREE_03", ANALYTIC_FAMILY, NA_character_, "Treemap", "Compare",
    "Қай санаттың мәні жоғары: B әлде C?",
    c("B", "C", "Бірдей"),
    "B",
    function() plot_reference_treemap(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "TREE_04", ANALYTIC_FAMILY, NA_character_, "Treemap", "Second highest",
    "Қай санаттың мәні максималды мәннен кейін екінші орында тұр?",
    c("A", "B", "C", "D", "E"),
    "B",
    function() plot_reference_treemap(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "TREE_05", ANALYTIC_FAMILY, NA_character_, "Treemap", "Difference",
    "D санатының мәні B санатынан қанша бірлікке жоғары?",
    c("10", "15", "20", "25", "30"),
    "20",
    function() plot_reference_treemap(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "HEAT_01", ANALYTIC_FAMILY, NA_character_, "Heatmap", "Maximum",
    "Қай санаттың мәні ең жоғары?",
    c("A", "B", "C", "D", "E"),
    "D",
    function() plot_reference_heatmap(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "HEAT_02", ANALYTIC_FAMILY, NA_character_, "Heatmap", "Minimum",
    "Қай санаттың мәні ең төмен?",
    c("A", "B", "C", "D", "E"),
    "E",
    function() plot_reference_heatmap(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "HEAT_03", ANALYTIC_FAMILY, NA_character_, "Heatmap", "Compare",
    "Қай санаттың мәні жоғары: B әлде C?",
    c("B", "C", "Бірдей"),
    "B",
    function() plot_reference_heatmap(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "HEAT_04", ANALYTIC_FAMILY, NA_character_, "Heatmap", "Second highest",
    "Қай санаттың мәні максималды мәннен кейін екінші орында тұр?",
    c("A", "B", "C", "D", "E"),
    "B",
    function() plot_reference_heatmap(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "HEAT_05", ANALYTIC_FAMILY, NA_character_, "Heatmap", "Difference",
    "D санатының мәні B санатынан қанша бірлікке жоғары?",
    c("10", "15", "20", "25", "30"),
    "20",
    function() plot_reference_heatmap(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "BUB_01", ANALYTIC_FAMILY, NA_character_, "Bubble chart", "Maximum",
    "Қай санаттың мәні ең жоғары?",
    c("A", "B", "C", "D", "E"),
    "D",
    function() plot_reference_bubble(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "BUB_02", ANALYTIC_FAMILY, NA_character_, "Bubble chart", "Minimum",
    "Қай санаттың мәні ең төмен?",
    c("A", "B", "C", "D", "E"),
    "E",
    function() plot_reference_bubble(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "BUB_03", ANALYTIC_FAMILY, NA_character_, "Bubble chart", "Compare",
    "Қай санаттың мәні жоғары: B әлде C?",
    c("B", "C", "Бірдей"),
    "B",
    function() plot_reference_bubble(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "BUB_04", ANALYTIC_FAMILY, NA_character_, "Bubble chart", "Second highest",
    "Қай санаттың мәні максималды мәннен кейін екінші орында тұр?",
    c("A", "B", "C", "D", "E"),
    "B",
    function() plot_reference_bubble(show_titles = TRUE)
  ))
  
  add_task(single_task(
    "BUB_05", ANALYTIC_FAMILY, NA_character_, "Bubble chart", "Difference",
    "D санатының мәні B санатынан қанша бірлікке жоғары?",
    c("10", "15", "20", "25", "30"),
    "20",
    function() plot_reference_bubble(show_titles = TRUE)
  ))
  
  # ---- Special Analytics Multi-panel: extra 1 ----
  add_task(multi_panel_task(
    "SPA_01", ANALYTIC_FAMILY, NA_character_, "4-panel analytics reference", "Mixed analytics comparison",
    "Сұрақтарға жауап бер",
    panels = list(
      list(
        panel_id = "spa_bar",
        panel_label = "Қай санаттың мәні ең жоғары?",
        panel_order = 1L,
        answer_choices = c("A", "B", "C", "D", "E"),
        correct_answer = "D",
        plot_fun = function() plot_reference_bar(show_titles = FALSE)
      ),
      list(
        panel_id = "spa_treemap",
        panel_label = "Қай санаттың мәні ең жоғары?",
        panel_order = 2L,
        answer_choices = c("A", "B", "C", "D", "E"),
        correct_answer = "D",
        plot_fun = function() plot_reference_treemap(show_titles = FALSE)
      ),
      list(
        panel_id = "spa_heatmap",
        panel_label = "Қай санаттың мәні ең жоғары?",
        panel_order = 3L,
        answer_choices = c("A", "B", "C", "D", "E"),
        correct_answer = "D",
        plot_fun = function() plot_reference_heatmap(show_titles = FALSE)
      ),
      list(
        panel_id = "spa_bubble",
        panel_label = "Қай санаттың мәні ең жоғары?",
        panel_order = 4L,
        answer_choices = c("A", "B", "C", "D", "E"),
        correct_answer = "D",
        plot_fun = function() plot_reference_bubble(show_titles = FALSE)
      )
    )
  ))
  
  tasks
}
