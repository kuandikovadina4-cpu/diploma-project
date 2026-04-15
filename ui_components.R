soft_card <- function(..., class = "") {
  shiny::div(class = trimws(paste("soft-card", class)), ...)
}

mini_badge <- function(text) {
  shiny::div(class = "mini-badge", text)
}

welcome_screen_ui <- function() {
  shiny::div(
    class = "page-wrap",
    shiny::div(
      class = "hero-grid",
      soft_card(
        class = "hero-card",
        shiny::tags$h1("Визуалды қабылдауды эксперименттік бағалау жүйесі"),
        shiny::tags$p(
          class = "lead-text",
          "Бұл қосымша визуалды құрылымдарды қабылдауды зерттеуге арналған. Қатысушы әр бетте берілген суретке қарап, қысқа сұраққа жауап береді."
        ),
        shiny::div(
          class = "hero-highlights",
          mini_badge("32 тапсырма"),
          mini_badge("Кездейсоқ рет"),
          mini_badge("Жауап уақыты тіркеледі"),
          mini_badge("Нәтижелер CSV форматына сақталады")
        ),
        shiny::tags$ul(
          class = "brief-list",
          shiny::tags$li("Қатысушы туралы мәліметті енгізіп, экспериментті бастаңыз."),
          shiny::tags$li("Әр тапсырмада бір негізгі сұраққа жауап беріңіз."),
          shiny::tags$li("Соңында нәтижелерді жүктеп алуға болады.")
        ),
        shiny::tags$p(
          class = "results-note",
          paste("Нәтижелер мына файлға сақталады:", normalizePath(RESULTS_FILE, winslash = "/", mustWork = FALSE))
        )
      ),
      soft_card(
        class = "form-card",
        shiny::tags$h2("Қатысушы туралы ақпарат"),
        shiny::textInput("participant_id", "Қатысушы ID-і", placeholder = "мысалы, P-001"),
        shiny::numericInput("age", "Жасы (міндетті емес)", value = NA, min = 16, max = 99, step = 1),
        shiny::selectInput(
          "gender",
          "Жынысы (міндетті емес)",
          choices = c(
            "Таңдаңыз..." = "",
            "Әйел" = "Әйел",
            "Ер" = "Ер",
            "Басқа" = "Басқа",
            "Көрсетпеуді жөн көремін" = "Көрсетпеуді жөн көремін"
          )
        ),
        shiny::textInput("specialization", "Мамандығы (міндетті емес)", placeholder = "мысалы, Психология"),
        shiny::actionButton("start_experiment", "Экспериментті бастау", class = "action-main")
      )
    )
  )
}

experiment_screen_ui <- function(task, position, total, participant_id) {
  progress_pct <- round(100 * position / total)

  shiny::div(
    class = "page-wrap",
    shiny::div(
      class = "top-strip",
      shiny::div(
        class = "progress-shell",
        shiny::div(
          class = "progress-track",
          shiny::div(class = "progress-fill", style = paste0("width:", progress_pct, "%;"))
        ),
        shiny::div(class = "progress-text", paste("Тапсырма", position, "/", total))
      ),
      shiny::div(class = "progress-text", paste("Қатысушы:", participant_id))
    ),
    shiny::div(
      class = "experiment-grid",
      soft_card(
        class = "plot-card",
        shiny::uiOutput("task_visual_ui")
      ),
      soft_card(
        class = "question-card",
        shiny::tags$h2(task$question_text),
        shiny::radioButtons(
          "answer_choice",
          label = "Жауабыңыз",
          choices = task$answer_choices,
          selected = character(0),
          inline = TRUE
        ),
        shiny::sliderInput(
          "ease_rating",
          "Бұл тапсырманы орындау қаншалықты жеңіл болды?",
          min = 1,
          max = 5,
          value = 3,
          step = 1,
          ticks = FALSE
        ),
        shiny::div(class = "confidence-scale", "1 = өте қиын, 5 = өте жеңіл"),
        shiny::sliderInput(
          "confidence",
          "Жауабыңызға қаншалықты сенімдісіз?",
          min = 1,
          max = 5,
          value = 3,
          step = 1,
          ticks = FALSE
        ),
        shiny::div(class = "confidence-scale", "1 = сенім аз, 5 = сенім жоғары"),
        shiny::tags$button(
          id = "submit_answer",
          type = "button",
          class = "action-main action-submit",
          disabled = "disabled",
          "Жауапты жіберу"
        )
      )
    )
  )
}

multi_panel_task_ui <- function(task, position, total, participant_id) {
  progress_pct <- round(100 * position / total)

  shiny::div(
    class = "page-wrap",
    shiny::div(
      class = "top-strip",
      shiny::div(
        class = "progress-shell",
        shiny::div(
          class = "progress-track",
          shiny::div(class = "progress-fill", style = paste0("width:", progress_pct, "%;"))
        ),
        shiny::div(class = "progress-text", paste("Тапсырма", position, "/", total))
      ),
      shiny::div(class = "progress-text", paste("Қатысушы:", participant_id))
    ),
    soft_card(
      class = "multi-task-card",
      shiny::tags$h2(task$question_text),
      shiny::div(
        class = "multi-slider-grid",
        shiny::div(
          class = "multi-slider-card",
          shiny::sliderInput(
            "ease_rating",
            "Бұл тапсырманы орындау қаншалықты жеңіл болды?",
            min = 1,
            max = 5,
            value = 3,
            step = 1,
            ticks = FALSE
          ),
          shiny::div(class = "confidence-scale", "1 = өте қиын, 5 = өте жеңіл")
        ),
        shiny::div(
          class = "multi-slider-card",
          shiny::sliderInput(
            "confidence",
            "Жауабыңызға қаншалықты сенімдісіз?",
            min = 1,
            max = 5,
            value = 3,
            step = 1,
            ticks = FALSE
          ),
          shiny::div(class = "confidence-scale", "1 = сенім аз, 5 = сенім жоғары")
        )
      ),
      shiny::uiOutput("task_visual_ui")
    )
  )
}

completion_screen_ui <- function() {
  shiny::div(
    class = "page-wrap",
    soft_card(
      class = "completion-card",
      shiny::tags$h1("Эксперимент аяқталды"),
      shiny::tags$p(
        class = "lead-text",
        "Жауаптарыңыз сәтті сақталды. Қажет болса, төменнен нәтижелерді жүктей аласыз."
      ),
      shiny::uiOutput("completion_summary_ui"),
      shiny::div(class = "download-row", shiny::uiOutput("download_buttons_ui")),
      shiny::div(class = "results-note", shiny::textOutput("results_path_text", inline = TRUE)),
      shiny::actionButton("reset_experiment", "Жаңа қатысушыны бастау", class = "action-secondary")
    ),
    shiny::div(
      class = "completion-grid",
      soft_card(
        class = "table-card",
        shiny::tags$h2("Ағымдағы жауаптар"),
        DT::DTOutput("participant_results_table")
      ),
      soft_card(
        class = "table-card",
        shiny::tags$h2("Соңғы сақталған жазбалар"),
        DT::DTOutput("recent_results_table")
      )
    )
  )
}

teacher_analysis_ui <- function() {
  shiny::div(
    class = "page-wrap teacher-page",
    shiny::div(
      class = "teacher-grid",
      soft_card(
        class = "teacher-upload-card",
        shiny::tags$h1("Мұғалімге арналған талдау"),
        shiny::tags$p(
          class = "lead-text",
          "Бұл бөлімде эксперимент нәтижелері бар CSV файлын жүктеп, визуализация түрлері бойынша статистикалық талдау жүргізуге болады."
        ),
        shiny::fileInput(
          "teacher_csv",
          "CSV файлын жүктеңіз",
          accept = c(".csv")
        ),
        shiny::uiOutput("teacher_upload_message_ui"),
        shiny::div(
          class = "teacher-actions",
          shiny::actionButton("teacher_run_analysis", "Талдауды бастау", class = "action-main"),
          shiny::downloadButton("teacher_download_summary", "Қорытындыны CSV-ге жүктеу", class = "action-secondary"),
          shiny::downloadButton("teacher_download_tests", "Тест нәтижесін CSV-ге жүктеу", class = "action-secondary")
        )
      ),
      soft_card(
        class = "teacher-filter-card",
        shiny::tags$h2("Талдау параметрлері"),
        shiny::checkboxGroupInput(
          "teacher_vis_types",
          "Визуализация түрлері",
          choices = character(0)
        ),
        shiny::selectInput(
          "teacher_metric",
          "Негізгі көрсеткіш",
          choices = analysis_metric_choices(),
          selected = "subjective_answer"
        ),
        shiny::selectizeInput(
          "teacher_task_family",
          "Task family бойынша сүзу",
          choices = character(0),
          multiple = TRUE
        ),
        shiny::selectizeInput(
          "teacher_block",
          "Блок бойынша сүзу",
          choices = character(0),
          multiple = TRUE
        ),
        shiny::radioButtons(
          "teacher_row_scope",
          "Жол түрі",
          choices = c(
            "Барлығы" = "all",
            "Тек объективті" = "objective",
            "Тек субъективті" = "subjective"
          ),
          selected = "all",
          inline = TRUE
        ),
        shiny::checkboxInput(
          "teacher_robust",
          "Параметрлік емес талдауды қолдану",
          value = FALSE
        ),
        shiny::checkboxInput(
          "teacher_check_normality",
          "Нормалдықты тексеру",
          value = FALSE
        )
      )
    ),
    shiny::div(
      class = "teacher-output-grid",
      soft_card(
        class = "table-card",
        shiny::tags$h2("Бағандарды тексеру"),
        DT::DTOutput("teacher_validation_table")
      ),
      soft_card(
        class = "table-card",
        shiny::tags$h2("Жүктелген файлдың үзіндісі"),
        DT::DTOutput("teacher_preview_table")
      )
    ),
    shiny::div(
      class = "teacher-output-grid",
      soft_card(
        class = "table-card",
        shiny::tags$h2("Жалпы қорытынды"),
        shiny::uiOutput("teacher_overview_ui"),
        DT::DTOutput("teacher_summary_table")
      ),
      soft_card(
        class = "table-card",
        shiny::tags$h2("Статистикалық нәтиже"),
        shiny::uiOutput("teacher_interpretation_ui"),
        shiny::verbatimTextOutput("teacher_test_output"),
        DT::DTOutput("teacher_group_summary_table"),
        DT::DTOutput("teacher_normality_table"),
        DT::DTOutput("teacher_posthoc_table")
      )
    ),
    shiny::div(
      class = "teacher-plot-grid",
      soft_card(
        class = "plot-card teacher-plot-card",
        shiny::tags$h2("Субъективті бағаның boxplot-ы"),
        shiny::plotOutput("teacher_subjective_boxplot", height = "340px")
      ),
      soft_card(
        class = "plot-card teacher-plot-card",
        shiny::tags$h2("Орташа сенім"),
        shiny::plotOutput("teacher_confidence_barplot", height = "340px")
      ),
      soft_card(
        class = "plot-card teacher-plot-card",
        shiny::tags$h2("Дәлдік"),
        shiny::plotOutput("teacher_accuracy_barplot", height = "340px")
      ),
      soft_card(
        class = "plot-card teacher-plot-card",
        shiny::tags$h2("Реакция уақытының boxplot-ы"),
        shiny::plotOutput("teacher_reaction_time_boxplot", height = "340px")
      )
    )
  )
}
