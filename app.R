# ---- Package setup ----
required_packages <- c(
  "shiny",
  "ggplot2",
  "dplyr",
  "readr",
  "DT"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste(
      "Please install the required packages before running the app:",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

source(file.path("R", "helpers.R"), encoding = "UTF-8")
source(file.path("R", "tasks.R"), encoding = "UTF-8")
source(file.path("R", "ui_components.R"), encoding = "UTF-8")

TASK_BANK <- build_task_bank()
validate_task_bank(TASK_BANK)

# ---- UI ----
ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::includeCSS(file.path("www", "styles.css")),
    shiny::tags$script(
      shiny::HTML(
        "
        window.gestaltTaskClock = {
          taskId: null,
          shownPerf: null,
          shownAtMs: null,
          observer: null,
          fallbackTimer: null
        };

        window.multiPanelTaskClock = {
          taskId: null,
          shownPerf: null,
          shownAtMs: null,
          activePanelId: null,
          panelShownPerf: {},
          panelShownAtMs: {},
          observer: null,
          fallbackTimer: null
        };

        window.disableTaskSubmit = function() {
          var button = document.getElementById('submit_answer');
          if (button) {
            button.disabled = true;
            button.classList.add('is-disabled');
          }
        };

        window.unlockTaskSubmit = function() {
          var button = document.getElementById('submit_answer');
          if (button) {
            button.disabled = false;
            button.classList.remove('is-disabled');
          }
        };

        window.disableAllPanelButtons = function() {
          document.querySelectorAll('.panel-confirm-button').forEach(function(button) {
            button.disabled = true;
            button.classList.add('is-disabled');
          });
        };

        window.setPanelSubmitting = function(panelId, isSubmitting) {
          var panel = document.querySelector('.multi-panel-panel[data-panel-id=\"' + panelId + '\"]');
          if (!panel) {
            return;
          }

          panel.dataset.submitting = isSubmitting ? 'true' : 'false';

          var button = panel.querySelector('.panel-confirm-button');
          if (button && button.dataset.locked !== 'true') {
            button.disabled = !!isSubmitting;
            button.classList.toggle('is-disabled', !!isSubmitting);
          }

          panel.querySelectorAll('input[type=\"radio\"]').forEach(function(input) {
            if (panel.classList.contains('is-answered')) {
              input.disabled = true;
              return;
            }
            input.disabled = !!isSubmitting;
          });
        };

        window.ensureMultiPanelTimerStarted = function(panelId) {
          if (!panelId) {
            return;
          }

          if (window.multiPanelTaskClock.panelShownAtMs[panelId]) {
            return;
          }

          var nowPerf = window.performance.now();
          var nowMs = Date.now();

          window.multiPanelTaskClock.panelShownPerf[panelId] = nowPerf;
          window.multiPanelTaskClock.panelShownAtMs[panelId] = nowMs;

          if (window.Shiny && window.Shiny.setInputValue) {
            Shiny.setInputValue('multi_panel_panel_display_event', {
              task_id: window.multiPanelTaskClock.taskId,
              panel_id: panelId,
              displayed_at_ms: nowMs,
              nonce: Math.random()
            }, { priority: 'event' });
          }
        };

        window.syncMultiPanelInputs = function() {
          document.querySelectorAll('.multi-panel-panel').forEach(function(panel) {
            var isAnswered = panel.classList.contains('is-answered');
            var isSubmitting = panel.dataset.submitting === 'true';

            panel.querySelectorAll('input[type=\"radio\"]').forEach(function(input) {
              input.disabled = isAnswered || isSubmitting;
            });

            var button = panel.querySelector('.panel-confirm-button');
            if (!button || button.dataset.locked === 'true') {
              return;
            }

            if (isAnswered || isSubmitting) {
              button.disabled = true;
              button.classList.add('is-disabled');
              return;
            }

            var hasSelection = !!panel.querySelector('input[type=\"radio\"]:checked');
            button.disabled = !hasSelection;
            button.classList.toggle('is-disabled', !hasSelection);
          });
        };

        window.unlockPendingPanelButtons = function() {
          window.setNextActivePanel();
          window.syncMultiPanelInputs();
        };

        window.setNextActivePanel = function() {
          var panels = document.querySelectorAll('.multi-panel-panel');
          var currentActive = Array.prototype.find.call(panels, function(panel) {
            return panel.classList.contains('is-active') && !panel.classList.contains('is-answered');
          });

          var nextPanel = currentActive || Array.prototype.find.call(panels, function(panel) {
            return !panel.classList.contains('is-answered');
          });

          panels.forEach(function(panel) {
            if (panel.classList.contains('is-answered')) {
              return;
            }

            var badge = panel.querySelector('.multi-panel-status');
            if (panel === nextPanel) {
              panel.classList.remove('is-pending');
              panel.classList.add('is-active');
              if (badge) {
                badge.textContent = 'Белсенді';
              }
              window.multiPanelTaskClock.activePanelId = panel.dataset.panelId;
            } else {
              panel.classList.remove('is-active');
              panel.classList.add('is-pending');
              if (badge) {
                badge.textContent = 'Күтілуде';
              }
            }
          });

          if (!nextPanel) {
            window.multiPanelTaskClock.activePanelId = null;
          }

          window.syncMultiPanelInputs();
        };

        window.resetMultiPanelUi = function() {
          window.multiPanelTaskClock.activePanelId = null;
          window.multiPanelTaskClock.panelShownPerf = {};
          window.multiPanelTaskClock.panelShownAtMs = {};

          document.querySelectorAll('.multi-panel-panel').forEach(function(panel) {
            panel.classList.remove('is-active', 'is-answered');
            panel.classList.add('is-pending');
            panel.dataset.locked = 'false';
            panel.dataset.submitting = 'false';

            var badge = panel.querySelector('.multi-panel-status');
            if (badge) {
              badge.textContent = 'Күтілуде';
            }

            panel.querySelectorAll('input[type=\"radio\"]').forEach(function(input) {
              input.disabled = true;
              input.checked = false;
            });

            var button = panel.querySelector('.panel-confirm-button');
            if (button) {
              button.disabled = true;
              button.classList.add('is-disabled');
              button.dataset.locked = 'false';
              button.textContent = 'Жауапты растау';
            }
          });
        };

        window.markPanelAnsweredUi = function(panelId) {
          var panel = document.querySelector('.multi-panel-panel[data-panel-id=\"' + panelId + '\"]');
          if (!panel) {
            return;
          }

          panel.classList.remove('is-pending', 'is-active');
          panel.classList.add('is-answered');
          panel.dataset.locked = 'true';
          panel.dataset.submitting = 'false';

          var badge = panel.querySelector('.multi-panel-status');
          if (badge) {
            badge.textContent = 'Аяқталды';
          }

          panel.querySelectorAll('input[type=\"radio\"]').forEach(function(input) {
            input.disabled = true;
          });

          var button = panel.querySelector('.panel-confirm-button');
          if (button) {
            button.disabled = true;
            button.classList.add('is-disabled');
            button.dataset.locked = 'true';
            button.textContent = 'Расталды';
          }

          window.setNextActivePanel();
        };

        window.clearTaskDisplayObserver = function() {
          if (window.gestaltTaskClock.observer) {
            window.gestaltTaskClock.observer.disconnect();
            window.gestaltTaskClock.observer = null;
          }
          if (window.gestaltTaskClock.fallbackTimer) {
            window.clearTimeout(window.gestaltTaskClock.fallbackTimer);
            window.gestaltTaskClock.fallbackTimer = null;
          }
        };

        window.clearMultiPanelObserver = function() {
          if (window.multiPanelTaskClock.observer) {
            window.multiPanelTaskClock.observer.disconnect();
            window.multiPanelTaskClock.observer = null;
          }
          if (window.multiPanelTaskClock.fallbackTimer) {
            window.clearTimeout(window.multiPanelTaskClock.fallbackTimer);
            window.multiPanelTaskClock.fallbackTimer = null;
          }
        };

        window.markTaskDisplayed = function(taskId) {
          if (window.gestaltTaskClock.taskId !== taskId || window.gestaltTaskClock.shownPerf !== null) {
            return;
          }

          window.clearTaskDisplayObserver();
          window.gestaltTaskClock.shownPerf = window.performance.now();
          window.gestaltTaskClock.shownAtMs = Date.now();
          window.unlockTaskSubmit();

          if (window.Shiny && window.Shiny.setInputValue) {
            Shiny.setInputValue('task_display_event', {
              task_id: taskId,
              displayed_at_ms: window.gestaltTaskClock.shownAtMs,
              nonce: Math.random()
            }, { priority: 'event' });
          }
        };

        window.markMultiPanelDisplayed = function(taskId) {
          if (window.multiPanelTaskClock.taskId !== taskId || window.multiPanelTaskClock.shownPerf !== null) {
            return;
          }

          window.clearMultiPanelObserver();
          window.multiPanelTaskClock.shownPerf = window.performance.now();
          window.multiPanelTaskClock.shownAtMs = Date.now();
          window.unlockPendingPanelButtons();
        };

        window.initTaskDisplayObserver = function(taskId) {
          window.clearMultiPanelObserver();
          window.clearTaskDisplayObserver();
          window.gestaltTaskClock = {
            taskId: taskId,
            shownPerf: null,
            shownAtMs: null,
            observer: null,
            fallbackTimer: null
          };

          window.disableTaskSubmit();
          var container = document.getElementById('task-visual-container');

          if (!container) {
            window.gestaltTaskClock.fallbackTimer = window.setTimeout(function() {
              window.markTaskDisplayed(taskId);
            }, 250);
            return;
          }

          var arePlotsReady = function() {
            var plotImages = container.querySelectorAll('.shiny-plot-output img');
            if (!plotImages.length) {
              return false;
            }
            return Array.prototype.every.call(plotImages, function(img) {
              return img.complete && img.naturalWidth > 0;
            });
          };

          var checkPlots = function() {
            if (window.gestaltTaskClock.taskId !== taskId) {
              return;
            }

            if (arePlotsReady()) {
              window.requestAnimationFrame(function() {
                window.markTaskDisplayed(taskId);
              });
              return;
            }

            var plotImages = container.querySelectorAll('.shiny-plot-output img');
            Array.prototype.forEach.call(plotImages, function(img) {
              if (img.complete && img.naturalWidth > 0) {
                return;
              }

              var onDone = function() {
                window.requestAnimationFrame(checkPlots);
              };
              img.addEventListener('load', onDone, { once: true });
              img.addEventListener('error', onDone, { once: true });
            });
          };

          window.gestaltTaskClock.observer = new MutationObserver(function() {
            window.requestAnimationFrame(checkPlots);
          });
          window.gestaltTaskClock.observer.observe(container, {
            childList: true,
            subtree: true,
            attributes: true
          });

          window.gestaltTaskClock.fallbackTimer = window.setTimeout(function() {
            window.markTaskDisplayed(taskId);
          }, 5000);

          window.requestAnimationFrame(checkPlots);
        };

        window.initMultiPanelObserver = function(taskId) {
          window.clearTaskDisplayObserver();
          window.clearMultiPanelObserver();
          window.multiPanelTaskClock = {
            taskId: taskId,
            shownPerf: null,
            shownAtMs: null,
            activePanelId: null,
            panelShownPerf: {},
            panelShownAtMs: {},
            observer: null,
            fallbackTimer: null
          };

          window.resetMultiPanelUi();
          window.disableAllPanelButtons();

          var container = document.getElementById('task-visual-container');
          if (!container) {
            window.multiPanelTaskClock.fallbackTimer = window.setTimeout(function() {
              window.markMultiPanelDisplayed(taskId);
            }, 250);
            return;
          }

          var arePlotsReady = function() {
            var plotImages = container.querySelectorAll('.shiny-plot-output img');
            if (!plotImages.length) {
              return false;
            }
            return Array.prototype.every.call(plotImages, function(img) {
              return img.complete && img.naturalWidth > 0;
            });
          };

          var checkPlots = function() {
            if (window.multiPanelTaskClock.taskId !== taskId) {
              return;
            }

            if (arePlotsReady()) {
              window.requestAnimationFrame(function() {
                window.markMultiPanelDisplayed(taskId);
              });
              return;
            }

            var plotImages = container.querySelectorAll('.shiny-plot-output img');
            Array.prototype.forEach.call(plotImages, function(img) {
              if (img.complete && img.naturalWidth > 0) {
                return;
              }

              var onDone = function() {
                window.requestAnimationFrame(checkPlots);
              };
              img.addEventListener('load', onDone, { once: true });
              img.addEventListener('error', onDone, { once: true });
            });
          };

          window.multiPanelTaskClock.observer = new MutationObserver(function() {
            window.requestAnimationFrame(checkPlots);
          });
          window.multiPanelTaskClock.observer.observe(container, {
            childList: true,
            subtree: true,
            attributes: true
          });

          window.multiPanelTaskClock.fallbackTimer = window.setTimeout(function() {
            window.markMultiPanelDisplayed(taskId);
          }, 5000);

          window.requestAnimationFrame(checkPlots);
        };

        document.addEventListener('click', function(event) {
          var button = event.target.closest('#submit_answer');
          if (!button || button.disabled) {
            return;
          }

          var selectedOption = document.querySelector('input[name=\"answer_choice\"]:checked');
          if (!selectedOption) {
            if (window.Shiny && window.Shiny.setInputValue) {
              Shiny.setInputValue('submit_validation_event', {
                reason: 'missing_answer',
                nonce: Math.random()
              }, { priority: 'event' });
            }
            return;
          }

          var tracker = window.gestaltTaskClock || {};
          var nowPerf = window.performance.now();
          var nowMs = Date.now();
          var reactionTimeMs = tracker.shownPerf === null ? null : Math.max(0, nowPerf - tracker.shownPerf);

          window.disableTaskSubmit();

          if (window.Shiny && window.Shiny.setInputValue) {
            Shiny.setInputValue('task_submit_event', {
              task_id: tracker.taskId,
              selected_answer: selectedOption.value,
              submitted_at_ms: nowMs,
              reaction_time_ms: reactionTimeMs,
              nonce: Math.random()
            }, { priority: 'event' });
          } else {
            window.unlockTaskSubmit();
          }
        });

        document.addEventListener('change', function(event) {
          if (event.target.type !== 'radio') {
            return;
          }

          var panel = event.target.closest('.multi-panel-panel');
          if (!panel) {
            return;
          }

          var panelId = panel.dataset.panelId;
          window.ensureMultiPanelTimerStarted(panelId);

          document.querySelectorAll('.multi-panel-panel').forEach(function(otherPanel) {
            if (otherPanel.classList.contains('is-answered')) {
              return;
            }

            otherPanel.classList.remove('is-active');
            otherPanel.classList.add('is-pending');

            var otherBadge = otherPanel.querySelector('.multi-panel-status');
            if (otherBadge) {
              otherBadge.textContent = 'Күтілуде';
            }
          });

          panel.classList.remove('is-pending');
          panel.classList.add('is-active');

          var badge = panel.querySelector('.multi-panel-status');
          if (badge && !panel.classList.contains('is-answered')) {
            badge.textContent = 'Белсенді';
          }

          window.multiPanelTaskClock.activePanelId = panelId;
          window.syncMultiPanelInputs();
        });

        document.addEventListener('click', function(event) {
          var panelButton = event.target.closest('.panel-confirm-button');
          if (!panelButton || panelButton.disabled) {
            return;
          }

          var panelId = panelButton.dataset.panelId;
          var panel = document.querySelector('.multi-panel-panel[data-panel-id=\"' + panelId + '\"]');
          if (!panel) {
            return;
          }

          var selectedOption = panel.querySelector('input[type=\"radio\"]:checked');
          if (!selectedOption) {
            if (window.Shiny && window.Shiny.setInputValue) {
              Shiny.setInputValue('panel_validation_event', {
                panel_id: panelId,
                nonce: Math.random()
              }, { priority: 'event' });
            }
            return;
          }

          window.ensureMultiPanelTimerStarted(panelId);

          var tracker = window.multiPanelTaskClock || {};
          var nowPerf = window.performance.now();
          var nowMs = Date.now();
          var panelShownPerf = tracker.panelShownPerf ? tracker.panelShownPerf[panelId] : null;
          var panelShownAtMs = tracker.panelShownAtMs ? tracker.panelShownAtMs[panelId] : null;
          var reactionTimeMs = panelShownPerf == null ? null : Math.max(0, nowPerf - panelShownPerf);

          window.setPanelSubmitting(panelId, true);

          if (window.Shiny && window.Shiny.setInputValue) {
            Shiny.setInputValue('multi_panel_submit_event', {
              task_id: tracker.taskId,
              panel_id: panelId,
              selected_answer: selectedOption.value,
              displayed_at_ms: panelShownAtMs,
              submitted_at_ms: nowMs,
              reaction_time_ms: reactionTimeMs,
              nonce: Math.random()
            }, { priority: 'event' });
          } else {
            window.unlockPendingPanelButtons();
          }
        });

        if (window.Shiny && window.Shiny.addCustomMessageHandler) {
          Shiny.addCustomMessageHandler('initTaskObserver', function(message) {
            if (message && message.task_id) {
              window.initTaskDisplayObserver(message.task_id);
            }
          });
          Shiny.addCustomMessageHandler('initMultiPanelObserver', function(message) {
            if (message && message.task_id) {
              window.initMultiPanelObserver(message.task_id);
            }
          });
          Shiny.addCustomMessageHandler('unlockSubmit', function() {
            window.unlockTaskSubmit();
          });
          Shiny.addCustomMessageHandler('disableSubmit', function() {
            window.disableTaskSubmit();
          });
          Shiny.addCustomMessageHandler('disableAllPanels', function() {
            window.disableAllPanelButtons();
          });
          Shiny.addCustomMessageHandler('unlockPanel', function(message) {
            if (!message || !message.panel_id) {
              return;
            }
            window.setPanelSubmitting(message.panel_id, false);
            window.syncMultiPanelInputs();
          });
          Shiny.addCustomMessageHandler('lockPanelWhileSubmitting', function(message) {
            if (!message || !message.panel_id) {
              return;
            }
            window.setPanelSubmitting(message.panel_id, true);
          });
          Shiny.addCustomMessageHandler('unlockPendingPanels', function() {
            window.unlockPendingPanelButtons();
          });
          Shiny.addCustomMessageHandler('markPanelAnswered', function(message) {
            if (message && message.panel_id) {
              window.markPanelAnsweredUi(message.panel_id);
            }
          });
          Shiny.addCustomMessageHandler('resetMultiPanelUi', function() {
            window.resetMultiPanelUi();
          });
        }
        "
      )
    )
  ),
  shiny::div(
    class = "app-tabs",
    shiny::tabsetPanel(
      id = "app_mode",
      shiny::tabPanel("Қатысушы бөлімі", shiny::uiOutput("app_body")),
      shiny::tabPanel("Мұғалімге арналған талдау", teacher_analysis_ui())
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  state <- shiny::reactiveValues(
    screen = "welcome",
    task_sequence = NULL,
    current_index = 0,
    task_started_at = NULL,
    task_started_at_client_ms = NA_real_,
    experiment_started_at = NULL,
    participant_run_id = NULL,
    session_results = empty_results_df(),
    is_submitting = FALSE,
    panel_progress = NULL
  )

  current_task <- shiny::reactive({
    shiny::req(!is.null(state$task_sequence))
    shiny::req(state$current_index >= 1)
    shiny::req(state$current_index <= length(state$task_sequence))
    state$task_sequence[[state$current_index]]
  })

  reset_task_display_state <- function() {
    state$task_started_at <- NULL
    state$task_started_at_client_ms <- NA_real_
  }

  participant_id_current <- shiny::reactive({
    trimws(input$participant_id %||% "")
  })

  message_datatable <- function(message_text) {
    DT::datatable(
      data.frame(Хабарлама = message_text, stringsAsFactors = FALSE),
      options = list(dom = "t", language = DT_LANGUAGE),
      rownames = FALSE
    )
  }

  empty_teacher_test <- function(message_text) {
    list(
      status = "warning",
      raw_output = message_text,
      interpretation = message_text,
      group_summary = data.frame(),
      result_table = data.frame(),
      posthoc_table = data.frame()
    )
  }

  teacher_analysis_result <- shiny::reactiveVal(NULL)

  teacher_upload <- shiny::reactive({
    if (is.null(input$teacher_csv)) {
      return(list(data = NULL, error = NULL))
    }
    safe_read_experiment_csv(input$teacher_csv)
  })

  teacher_raw_data <- shiny::reactive({
    teacher_upload()$data
  })

  teacher_upload_error <- shiny::reactive({
    teacher_upload()$error
  })

  teacher_validation <- shiny::reactive({
    validate_analysis_columns(teacher_raw_data())
  })

  teacher_analysis_data <- shiny::reactive({
    df <- teacher_raw_data()
    if (is.null(df) || !nrow(df)) {
      return(data.frame())
    }
    normalize_analysis_dataset(df)
  })

  initialize_panel_progress <- function(task) {
    if (!identical(task$renderer, "multi_panel_individual")) {
      state$panel_progress <- NULL
      return(invisible(NULL))
    }

    progress <- lapply(task$panels, function(panel) {
      list(
        panel_id = panel$panel_id,
        panel_label = panel$panel_label,
        panel_order = panel$panel_order,
        displayed_at_client_ms = NA_real_,
        submitted_at_client_ms = NA_real_,
        selected_answer = NA_character_,
        is_correct = NA,
        reaction_time_sec = NA_real_,
        submitted_at = as.POSIXct(NA),
        completed = FALSE
      )
    })
    names(progress) <- vapply(task$panels, function(panel) panel$panel_id, character(1))
    state$panel_progress <- progress
    invisible(NULL)
  }

  arm_task_observer <- function(task) {
    force(task)
    session$sendCustomMessage("disableSubmit", list())
    session$sendCustomMessage("disableAllPanels", list())
    session$onFlushed(function() {
      shiny::updateSliderInput(session, "ease_rating", value = 3)
      shiny::updateSliderInput(session, "confidence", value = 3)

      if (identical(task$renderer, "multi_panel_individual")) {
        session$sendCustomMessage("resetMultiPanelUi", list())
        session$sendCustomMessage("initMultiPanelObserver", list(task_id = task$task_id))
      } else {
        shiny::updateRadioButtons(session, "answer_choice", selected = character(0))
        session$sendCustomMessage("initTaskObserver", list(task_id = task$task_id))
      }
    }, once = TRUE)
  }

  shiny::observeEvent(input$teacher_csv, {
    teacher_analysis_result(NULL)
  }, ignoreInit = TRUE)

  shiny::observeEvent(teacher_analysis_data(), {
    data <- teacher_analysis_data()

    if (is.null(data) || !nrow(data)) {
      shiny::updateCheckboxGroupInput(session, "teacher_vis_types", choices = character(0), selected = character(0))
      shiny::updateSelectizeInput(session, "teacher_task_family", choices = character(0), selected = character(0), server = TRUE)
      shiny::updateSelectizeInput(session, "teacher_block", choices = character(0), selected = character(0), server = TRUE)
      return()
    }

    vis_choices <- nonempty_unique_values(data$vis_type)
    task_family_choices <- nonempty_unique_values(data$task_family)
    block_choices <- nonempty_unique_values(data$block)

    shiny::updateCheckboxGroupInput(session, "teacher_vis_types", choices = vis_choices, selected = vis_choices)
    shiny::updateSelectizeInput(session, "teacher_task_family", choices = task_family_choices, selected = character(0), server = TRUE)
    shiny::updateSelectizeInput(session, "teacher_block", choices = block_choices, selected = character(0), server = TRUE)
  }, ignoreInit = FALSE)

  output$app_body <- shiny::renderUI({
    if (identical(state$screen, "welcome")) {
      return(welcome_screen_ui())
    }

    if (identical(state$screen, "experiment")) {
      participant_label <- participant_id_current()
      if (!nzchar(participant_label)) {
        participant_label <- "Қатысушы"
      }

      task <- current_task()
      if (identical(task$renderer, "multi_panel_individual")) {
        return(multi_panel_task_ui(task, state$current_index, length(state$task_sequence), participant_label))
      }

      return(experiment_screen_ui(task, state$current_index, length(state$task_sequence), participant_label))
    }

    completion_screen_ui()
  })

  output$teacher_upload_message_ui <- shiny::renderUI({
    if (is.null(input$teacher_csv)) {
      return(
        shiny::div(
          class = "teacher-message is-info",
          "Нәтижелерді талдау үшін алдымен CSV файлын жүктеңіз."
        )
      )
    }

    upload_error <- teacher_upload_error()
    if (!is.null(upload_error)) {
      return(shiny::div(class = "teacher-message is-warning", upload_error))
    }

    raw_df <- teacher_raw_data()
    validation <- teacher_validation()

    tags <- list(
      shiny::div(
        class = "teacher-message is-success",
        paste("Файл сәтті жүктелді:", nrow(raw_df), "жол,", ncol(raw_df), "баған.")
      )
    )

    if (length(validation$warnings)) {
      tags <- c(
        tags,
        list(
          shiny::div(
            class = "teacher-message is-warning",
            shiny::tags$ul(
              class = "teacher-note-list",
              lapply(validation$warnings, shiny::tags$li)
            )
          )
        )
      )
    }

    shiny::tagList(tags)
  })

  output$teacher_validation_table <- DT::renderDT({
    if (is.null(input$teacher_csv)) {
      return(message_datatable("Әзірге файл жүктелген жоқ."))
    }

    upload_error <- teacher_upload_error()
    if (!is.null(upload_error)) {
      return(message_datatable(upload_error))
    }

    validation <- teacher_validation()
    DT::datatable(
      validation$summary,
      rownames = FALSE,
      options = list(dom = "t", autoWidth = TRUE, language = DT_LANGUAGE)
    )
  })

  output$teacher_preview_table <- DT::renderDT({
    if (is.null(input$teacher_csv)) {
      return(message_datatable("Файл жүктелгеннен кейін алдын ала қарау осында көрсетіледі."))
    }

    upload_error <- teacher_upload_error()
    if (!is.null(upload_error)) {
      return(message_datatable(upload_error))
    }

    preview_df <- utils::head(as.data.frame(teacher_raw_data(), stringsAsFactors = FALSE), 15)
    DT::datatable(
      preview_df,
      rownames = FALSE,
      options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, language = DT_LANGUAGE)
    )
  })

  shiny::observeEvent(input$teacher_run_analysis, {
    if (is.null(input$teacher_csv)) {
      teacher_analysis_result(list(
        error = "Алдымен CSV файлын жүктеңіз.",
        filtered = data.frame(),
        descriptives = generate_analysis_descriptives(data.frame()),
        metric_info = list(data = data.frame(), metric_label = NULL, error = NULL),
        normality = list(table = data.frame(), interpretation = NULL),
        test_result = empty_teacher_test("Талдау әлі орындалған жоқ.")
      ))
      return()
    }

    upload_error <- teacher_upload_error()
    if (!is.null(upload_error)) {
      teacher_analysis_result(list(
        error = upload_error,
        filtered = data.frame(),
        descriptives = generate_analysis_descriptives(data.frame()),
        metric_info = list(data = data.frame(), metric_label = NULL, error = NULL),
        normality = list(table = data.frame(), interpretation = NULL),
        test_result = empty_teacher_test(upload_error)
      ))
      return()
    }

    validation <- teacher_validation()
    normalized <- teacher_analysis_data()

    if (!isTRUE(validation$vis_type_ready)) {
      error_text <- "Визуализация түрі көрсетілмегендіктен, салыстырмалы талдау жүргізу мүмкін емес."
      teacher_analysis_result(list(
        error = error_text,
        filtered = normalized,
        descriptives = generate_analysis_descriptives(data.frame()),
        metric_info = list(data = data.frame(), metric_label = NULL, error = NULL),
        normality = list(table = data.frame(), interpretation = NULL),
        test_result = empty_teacher_test(error_text)
      ))
      return()
    }

    available_vis <- nonempty_unique_values(normalized$vis_type)
    selected_vis <- input$teacher_vis_types %||% character(0)
    if (length(available_vis) > 0 && length(selected_vis) == 0) {
      teacher_analysis_result(list(
        error = "Кемінде бір визуализация түрін таңдаңыз.",
        filtered = data.frame(),
        descriptives = generate_analysis_descriptives(data.frame()),
        metric_info = list(data = data.frame(), metric_label = NULL, error = NULL),
        normality = list(table = data.frame(), interpretation = NULL),
        test_result = empty_teacher_test("Кемінде бір визуализация түрі таңдалуы тиіс.")
      ))
      return()
    }

    filtered <- filter_analysis_dataset(
      normalized,
      vis_types = if (length(selected_vis)) selected_vis else NULL,
      task_families = if (length(input$teacher_task_family %||% character(0))) input$teacher_task_family else NULL,
      blocks = if (length(input$teacher_block %||% character(0))) input$teacher_block else NULL,
      row_scope = input$teacher_row_scope %||% "all"
    )

    if (!nrow(filtered)) {
      teacher_analysis_result(list(
        error = "Сүзгіден кейін дерек қалмады.",
        filtered = filtered,
        descriptives = generate_analysis_descriptives(filtered),
        metric_info = list(data = data.frame(), metric_label = NULL, error = NULL),
        normality = list(table = data.frame(), interpretation = NULL),
        test_result = empty_teacher_test("Статистикалық тест жүргізуге дерек жеткіліксіз.")
      ))
      return()
    }

    descriptives <- generate_analysis_descriptives(filtered)
    metric_info <- prepare_analysis_metric_data(filtered, input$teacher_metric %||% "subjective_answer")

    normality <- list(table = data.frame(), interpretation = NULL)
    if (isTRUE(input$teacher_check_normality) && is.null(metric_info$error)) {
      normality <- run_shapiro_safe(metric_info$data, metric_info$metric_label)
    }

    test_result <- if (is.null(metric_info$error)) {
      run_analysis_test(
        metric_info$data,
        metric_info$metric_label,
        robust = isTRUE(input$teacher_robust)
      )
    } else {
      empty_teacher_test(metric_info$error)
    }

    teacher_analysis_result(list(
      error = NULL,
      filtered = filtered,
      descriptives = descriptives,
      metric_info = metric_info,
      normality = normality,
      test_result = test_result
    ))
  }, ignoreInit = TRUE)

  output$teacher_overview_ui <- shiny::renderUI({
    result <- teacher_analysis_result()
    if (is.null(result)) {
      return(shiny::div(class = "teacher-message is-info", "Файлды жүктеп, «Талдауды бастау» батырмасын басыңыз."))
    }

    if (!is.null(result$error)) {
      return(shiny::div(class = "teacher-message is-warning", result$error))
    }

    overview <- result$descriptives$overview
    shiny::div(
      class = "metric-grid",
      shiny::div(
        class = "metric-card",
        shiny::div(class = "metric-label", "Қатысушылар"),
        shiny::div(class = "metric-value", overview$participants)
      ),
      shiny::div(
        class = "metric-card",
        shiny::div(class = "metric-label", "Жол саны"),
        shiny::div(class = "metric-value", overview$rows)
      ),
      shiny::div(
        class = "metric-card",
        shiny::div(class = "metric-label", "Визуализация түрлері"),
        shiny::div(class = "metric-value", overview$vis_types)
      ),
      shiny::div(
        class = "metric-card",
        shiny::div(class = "metric-label", "Объективті / субъективті"),
        shiny::div(class = "metric-value", paste0(overview$objective_rows, " / ", overview$subjective_rows))
      )
    )
  })

  output$teacher_summary_table <- DT::renderDT({
    result <- teacher_analysis_result()
    if (is.null(result)) {
      return(message_datatable("Қорытынды кесте талдау басталғаннан кейін көрсетіледі."))
    }

    if (!is.null(result$error)) {
      return(message_datatable(result$error))
    }

    summary_df <- result$descriptives$table
    if (!nrow(summary_df)) {
      return(message_datatable("Қорытынды кесте үшін дерек табылмады."))
    }

    DT::datatable(
      summary_df,
      rownames = FALSE,
      options = list(pageLength = 12, autoWidth = TRUE, scrollX = TRUE, language = DT_LANGUAGE)
    )
  })

  output$teacher_interpretation_ui <- shiny::renderUI({
    result <- teacher_analysis_result()
    if (is.null(result)) {
      return(NULL)
    }

    messages <- list()

    if (!is.null(result$error)) {
      messages <- c(messages, list(shiny::tags$p(result$error)))
    }

    metric_error <- result$metric_info$error %||% NULL
    if (!is.null(metric_error)) {
      messages <- c(messages, list(shiny::tags$p(metric_error)))
    }

    test_interpretation <- result$test_result$interpretation %||% NULL
    if (!is.null(test_interpretation)) {
      if (length(test_interpretation) == 1) {
        messages <- c(messages, list(shiny::tags$p(test_interpretation)))
      } else {
        messages <- c(messages, lapply(test_interpretation, shiny::tags$p))
      }
    }

    normality_interpretation <- result$normality$interpretation %||% NULL
    if (!is.null(normality_interpretation) && nzchar(normality_interpretation)) {
      messages <- c(messages, list(shiny::tags$p(normality_interpretation)))
    }

    if (!length(messages)) {
      return(NULL)
    }

    shiny::div(class = "teacher-interpretation", messages)
  })

  output$teacher_test_output <- shiny::renderPrint({
    result <- teacher_analysis_result()
    if (is.null(result)) {
      cat("Статистикалық тест нәтижесі әлі есептелген жоқ.")
      return(invisible(NULL))
    }

    raw_output <- result$test_result$raw_output %||% "Статистикалық тест орындалмады."
    if (length(raw_output) > 1) {
      cat(paste(raw_output, collapse = "\n"))
    } else {
      cat(raw_output)
    }
  })

  output$teacher_group_summary_table <- DT::renderDT({
    result <- teacher_analysis_result()
    if (is.null(result)) {
      return(message_datatable("Топтық қорытынды әлі дайын емес."))
    }

    df <- result$test_result$group_summary
    if (is.null(df) || !nrow(df)) {
      return(message_datatable("Таңдалған көрсеткіш бойынша топтық қорытынды жасауға дерек жеткіліксіз."))
    }

    DT::datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", autoWidth = TRUE, language = DT_LANGUAGE)
    )
  })

  output$teacher_normality_table <- DT::renderDT({
    result <- teacher_analysis_result()
    if (is.null(result) || !isTRUE(input$teacher_check_normality)) {
      return(message_datatable("Нормалдықты тексеру таңдалған кезде осы жерде көрсетіледі."))
    }

    df <- result$normality$table
    if (is.null(df) || !nrow(df)) {
      return(message_datatable("Нормалдықты тексеруге жеткілікті дерек табылмады."))
    }

    DT::datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", autoWidth = TRUE, language = DT_LANGUAGE)
    )
  })

  output$teacher_posthoc_table <- DT::renderDT({
    result <- teacher_analysis_result()
    if (is.null(result)) {
      return(message_datatable("Post-hoc нәтижесі талдау аяқталғаннан кейін көрсетіледі."))
    }

    df <- result$test_result$posthoc_table
    if (is.null(df) || !nrow(df)) {
      return(message_datatable("Бұл талдау үшін post-hoc кестесі жоқ."))
    }

    DT::datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE, language = DT_LANGUAGE)
    )
  })

  output$teacher_subjective_boxplot <- shiny::renderPlot({
    result <- teacher_analysis_result()
    if (is.null(result) || is.null(result$filtered) || !nrow(result$filtered)) {
      return(analysis_empty_plot("Талдауды бастағаннан кейін график осында шығады."))
    }
    plot_subjective_score_boxplot(result$filtered)
  }, res = 110)

  output$teacher_confidence_barplot <- shiny::renderPlot({
    result <- teacher_analysis_result()
    if (is.null(result) || is.null(result$filtered) || !nrow(result$filtered)) {
      return(analysis_empty_plot("Талдауды бастағаннан кейін график осында шығады."))
    }
    plot_confidence_mean_bar(result$filtered)
  }, res = 110)

  output$teacher_accuracy_barplot <- shiny::renderPlot({
    result <- teacher_analysis_result()
    if (is.null(result) || is.null(result$filtered) || !nrow(result$filtered)) {
      return(analysis_empty_plot("Талдауды бастағаннан кейін график осында шығады."))
    }
    plot_accuracy_bar(result$filtered)
  }, res = 110)

  output$teacher_reaction_time_boxplot <- shiny::renderPlot({
    result <- teacher_analysis_result()
    if (is.null(result) || is.null(result$filtered) || !nrow(result$filtered)) {
      return(analysis_empty_plot("Талдауды бастағаннан кейін график осында шығады."))
    }
    plot_reaction_time_boxplot(result$filtered)
  }, res = 110)

  output$teacher_download_summary <- shiny::downloadHandler(
    filename = function() {
      paste0("analysis_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      result <- teacher_analysis_result()
      summary_df <- if (is.null(result) || is.null(result$descriptives$table) || !nrow(result$descriptives$table)) {
        data.frame(note = "Қорытынды статистика әзірге есептелген жоқ.", stringsAsFactors = FALSE)
      } else {
        result$descriptives$table
      }
      readr::write_csv(summary_df, file, na = "")
    }
  )

  output$teacher_download_tests <- shiny::downloadHandler(
    filename = function() {
      paste0("analysis_test_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      result <- teacher_analysis_result()
      export_df <- if (is.null(result)) {
        data.frame(section = "message", note = "Статистикалық тест нәтижесі әзірге жоқ.", stringsAsFactors = FALSE)
      } else {
        build_analysis_test_export(result$test_result, result$normality)
      }
      readr::write_csv(export_df, file, na = "")
    }
  )

  output$task_visual_ui <- shiny::renderUI({
    shiny::req(identical(state$screen, "experiment"))
    task <- current_task()

    if (identical(task$renderer, "multi_panel_individual")) {
      return(
        shiny::div(
          id = "task-visual-container",
          class = "multi-panel-grid",
          lapply(seq_along(task$panels), function(i) {
            panel <- task$panels[[i]]
            shiny::div(
              class = "multi-panel-panel is-pending",
              `data-panel-id` = panel$panel_id,
              shiny::div(
                class = "multi-panel-header",
                shiny::div(class = "multi-panel-title", panel$panel_label),
                shiny::div(class = "multi-panel-status", "Күтілуде")
              ),
              shiny::div(
                class = "multi-panel-plot-shell",
                shiny::plotOutput(paste0("multi_panel_plot_", i), height = "220px")
              ),
              shiny::radioButtons(
                inputId = paste0("panel_answer_", panel$panel_id),
                label = NULL,
                choices = panel$answer_choices,
                selected = character(0),
                inline = TRUE
              ),
              shiny::tags$button(
                id = paste0("panel_confirm_", panel$panel_id),
                type = "button",
                class = "action-secondary panel-confirm-button is-disabled",
                `data-panel-id` = panel$panel_id,
                disabled = "disabled",
                "Жауапты растау"
              )
            )
          })
        )
      )
    }

    if (identical(task$renderer, "plot_grid")) {
      n_plots <- length(task$plot_funs)
      grid_cols <- if (n_plots <= 2) n_plots else 2
      plot_height <- if (n_plots <= 2) "320px" else "270px"

      return(
        shiny::div(
          id = "task-visual-container",
          shiny::div(
            class = "stimulus-grid",
            style = paste0("grid-template-columns: repeat(", grid_cols, ", minmax(0, 1fr));"),
            lapply(seq_len(n_plots), function(i) {
              shiny::div(
                class = "stimulus-panel",
                shiny::div(class = "panel-label", task$panel_labels[[i]]),
                shiny::plotOutput(paste0("grid_plot_", i), height = plot_height)
              )
            })
          )
        )
      )
    }

    shiny::div(
      id = "task-visual-container",
      shiny::plotOutput("task_plot", height = "520px")
    )
  })

  output$task_plot <- shiny::renderPlot({
    task <- current_task()
    shiny::req(identical(task$renderer, "single_plot"))
    task$plot_fun()
  }, res = 110)

  render_grid_plot <- function(index) {
    shiny::renderPlot({
      task <- current_task()
      shiny::req(identical(task$renderer, "plot_grid"))
      shiny::req(length(task$plot_funs) >= index)
      task$plot_funs[[index]]()
    }, res = 110)
  }

  render_multi_panel_plot <- function(index) {
    shiny::renderPlot({
      task <- current_task()
      shiny::req(identical(task$renderer, "multi_panel_individual"))
      shiny::req(length(task$panels) >= index)
      task$panels[[index]]$plot_fun()
    }, res = 110)
  }

  output$grid_plot_1 <- render_grid_plot(1)
  output$grid_plot_2 <- render_grid_plot(2)
  output$grid_plot_3 <- render_grid_plot(3)
  output$grid_plot_4 <- render_grid_plot(4)

  output$multi_panel_plot_1 <- render_multi_panel_plot(1)
  output$multi_panel_plot_2 <- render_multi_panel_plot(2)
  output$multi_panel_plot_3 <- render_multi_panel_plot(3)
  output$multi_panel_plot_4 <- render_multi_panel_plot(4)

  output$completion_summary_ui <- shiny::renderUI({
    df <- state$session_results
    if (nrow(df) == 0) {
      return(NULL)
    }

    is_correct_vec <- as.logical(df$is_correct)
    reaction_time_vec <- suppressWarnings(as.numeric(df$reaction_time_sec))
    ease_vec <- suppressWarnings(as.numeric(df$ease_rating))
    confidence_vec <- suppressWarnings(as.numeric(df$confidence))

    accuracy <- if (all(is.na(is_correct_vec))) NA_real_ else round(mean(is_correct_vec, na.rm = TRUE) * 100, 1)
    median_rt <- if (all(is.na(reaction_time_vec))) NA_real_ else round(stats::median(reaction_time_vec, na.rm = TRUE), 2)
    avg_ease <- if (all(is.na(ease_vec))) NA_real_ else round(mean(ease_vec, na.rm = TRUE), 2)
    avg_conf <- if (all(is.na(confidence_vec))) NA_real_ else round(mean(confidence_vec, na.rm = TRUE), 2)

    shiny::div(
      class = "metric-grid",
      shiny::div(
        class = "metric-card",
        shiny::div(class = "metric-label", "Дәлдік"),
        shiny::div(class = "metric-value", paste0(accuracy, "%"))
      ),
      shiny::div(
        class = "metric-card",
        shiny::div(class = "metric-label", "Медиана уақыты"),
        shiny::div(class = "metric-value", paste0(median_rt, " сек"))
      ),
      shiny::div(
        class = "metric-card",
        shiny::div(class = "metric-label", "Орташа жеңілдік"),
        shiny::div(class = "metric-value", avg_ease)
      ),
      shiny::div(
        class = "metric-card",
        shiny::div(class = "metric-label", "Орташа сенім"),
        shiny::div(class = "metric-value", avg_conf)
      )
    )
  })

  output$download_buttons_ui <- shiny::renderUI({
    buttons <- list(
      shiny::downloadButton("download_my_results", "Менің нәтижелерімді CSV-ге жүктеу", class = "action-main")
    )

    if (file.exists(RESULTS_FILE)) {
      buttons <- c(
        buttons,
        list(shiny::downloadButton("download_all_results", "Барлық нәтижелерді CSV-ге жүктеу", class = "action-secondary"))
      )
    }

    shiny::tagList(buttons)
  })

  output$results_path_text <- shiny::renderText({
    paste(
      "Нәтижелер файлы:",
      normalizePath(RESULTS_FILE, winslash = "/", mustWork = FALSE),
      paste0("(", RESULTS_STORAGE_LABEL, ")")
    )
  })

  output$participant_results_table <- DT::renderDT({
    df <- state$session_results
    if (nrow(df) == 0) {
      return(
        DT::datatable(
          data.frame(Хабарлама = "Әзірге сақталған жауап жоқ."),
          options = list(dom = "t", language = DT_LANGUAGE),
          rownames = FALSE
        )
      )
    }

    view_df <- dplyr::transmute(
      df,
      Тапсырма = task_order_position,
      Панель = ifelse(is.na(panel_label) | panel_label == "", "-", panel_label),
      `Таңдалған жауап` = selected_answer,
      `Дұрыс жауап` = correct_answer,
      Дұрыс = ifelse(is_correct, "Иә", "Жоқ"),
      `Реакция уақыты, сек` = reaction_time_sec,
      `Жеңілдік бағасы` = ease_rating,
      Сенім = confidence
    )

    DT::datatable(
      view_df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE, language = DT_LANGUAGE)
    )
  })

  output$recent_results_table <- DT::renderDT({
    df <- read_results_file(RESULTS_FILE, n = 12)
    if (nrow(df) == 0) {
      return(
        DT::datatable(
          data.frame(Хабарлама = "Әзірге сақталған жазба жоқ."),
          options = list(dom = "t", language = DT_LANGUAGE),
          rownames = FALSE
        )
      )
    }

    view_df <- dplyr::transmute(
      df,
      `Қатысушы ID-і` = participant_id,
      `Тапсырма ID-і` = task_id,
      Панель = ifelse(is.na(panel_label) | panel_label == "", "-", panel_label),
      `Таңдалған жауап` = selected_answer,
      Дұрыс = ifelse(is_correct, "Иә", "Жоқ"),
      `Реакция уақыты, сек` = reaction_time_sec,
      `Жіберілген уақыты` = submitted_at
    )

    DT::datatable(
      view_df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE, language = DT_LANGUAGE)
    )
  })

  output$download_my_results <- shiny::downloadHandler(
    filename = function() {
      paste0("participant_results_", trimws(input$participant_id %||% "participant"), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(normalize_results_table(state$session_results), file, na = "")
    }
  )

  output$download_all_results <- shiny::downloadHandler(
    filename = function() {
      paste0("experiment_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(read_results_file(RESULTS_FILE), file, na = "")
    }
  )

  shiny::observeEvent(input$start_experiment, {
    participant_id <- trimws(input$participant_id %||% "")
    if (!nzchar(participant_id)) {
      shiny::showNotification("Экспериментті бастау үшін қатысушы ID-ін енгізіңіз.", type = "error")
      return()
    }

    state$screen <- "experiment"
    state$task_sequence <- unname(TASK_BANK[sample.int(length(TASK_BANK))])
    state$current_index <- 1
    state$experiment_started_at <- Sys.time()
    state$participant_run_id <- paste(session$token, format(Sys.time(), "%Y%m%d%H%M%OS3"), sep = "_")
    state$session_results <- empty_results_df()
    reset_task_display_state()

    first_task <- state$task_sequence[[1]]
    initialize_panel_progress(first_task)
    arm_task_observer(first_task)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$task_display_event, {
    shiny::req(identical(state$screen, "experiment"))
    payload <- input$task_display_event
    task <- current_task()

    if (!identical(task$renderer, "single_plot") && !identical(task$renderer, "plot_grid")) {
      return()
    }

    if (is.null(payload$task_id) || !identical(payload$task_id, task$task_id)) {
      return()
    }

    if (!is.null(state$task_started_at)) {
      return()
    }

    state$task_started_at_client_ms <- suppressWarnings(as.numeric(payload$displayed_at_ms))
    displayed_at <- client_ms_to_posix(state$task_started_at_client_ms)
    if (is.na(displayed_at)) {
      displayed_at <- Sys.time()
    }
    state$task_started_at <- displayed_at
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$multi_panel_panel_display_event, {
    shiny::req(identical(state$screen, "experiment"))
    payload <- input$multi_panel_panel_display_event
    task <- current_task()

    if (!identical(task$renderer, "multi_panel_individual")) {
      return()
    }
    if (is.null(payload$task_id) || !identical(payload$task_id, task$task_id)) {
      return()
    }
    if (is.null(state$panel_progress)) {
      return()
    }

    panel_id <- payload$panel_id %||% ""
    if (!nzchar(panel_id) || !panel_id %in% names(state$panel_progress)) {
      return()
    }

    displayed_ms <- suppressWarnings(as.numeric(payload$displayed_at_ms))
    progress <- state$panel_progress
    if (is.na(progress[[panel_id]]$displayed_at_client_ms)) {
      progress[[panel_id]]$displayed_at_client_ms <- displayed_ms
    }
    state$panel_progress <- progress
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$submit_validation_event, {
    shiny::req(identical(state$screen, "experiment"))
    shiny::showNotification("Жібермес бұрын жауап нұсқасын таңдаңыз.", type = "error")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$panel_validation_event, {
    shiny::req(identical(state$screen, "experiment"))
    shiny::showNotification("Алдымен осы панель үшін жауап таңдаңыз.", type = "error")
    session$sendCustomMessage("unlockPendingPanels", list())
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$task_submit_event, {
    shiny::req(identical(state$screen, "experiment"))

    if (isTRUE(state$is_submitting)) {
      return()
    }

    task <- current_task()
    payload <- input$task_submit_event

    if (!identical(task$renderer, "single_plot") && !identical(task$renderer, "plot_grid")) {
      session$sendCustomMessage("unlockSubmit", list())
      return()
    }

    if (is.null(payload$task_id) || !identical(payload$task_id, task$task_id)) {
      session$sendCustomMessage("unlockSubmit", list())
      return()
    }

    selected_answer <- trimws(payload$selected_answer %||% input$answer_choice %||% "")
    if (!nzchar(selected_answer)) {
      shiny::showNotification("Жібермес бұрын жауап нұсқасын таңдаңыз.", type = "error")
      session$sendCustomMessage("unlockSubmit", list())
      return()
    }

    state$is_submitting <- TRUE
    on.exit({
      state$is_submitting <- FALSE
    }, add = TRUE)

    submitted_at_client_ms <- suppressWarnings(as.numeric(payload$submitted_at_ms))
    submitted_at <- client_ms_to_posix(submitted_at_client_ms)
    if (is.na(submitted_at)) {
      submitted_at <- Sys.time()
    }

    started_at <- state$task_started_at %||% submitted_at
    reaction_time <- suppressWarnings(as.numeric(payload$reaction_time_ms) / 1000)
    if (is.na(reaction_time) || reaction_time < 0) {
      reaction_time <- round(as.numeric(difftime(submitted_at, started_at, units = "secs")), 3)
    } else {
      reaction_time <- round(reaction_time, 3)
    }

    age_value <- suppressWarnings(as.numeric(input$age))
    if (is.na(age_value) || age_value <= 0) {
      age_value <- NA_real_
    }

    result_row <- normalize_results_row(data.frame(
      participant_id = participant_id_current(),
      age = age_value,
      gender = trim_or_na(input$gender),
      specialization = trim_or_na(input$specialization),
      experiment_started_at = timestamp_string(state$experiment_started_at),
      started_at = timestamp_string(started_at),
      submitted_at = timestamp_string(submitted_at),
      displayed_at_client_ms = state$task_started_at_client_ms,
      submitted_at_client_ms = submitted_at_client_ms,
      task_order_position = state$current_index,
      task_id = task$task_id,
      question_id = task$question_id,
      task_family = task$task_family,
      gestalt_principle = task$principle,
      visualization_type = task$visualization_type,
      question_type = task$question_type,
      question_text = task$question_text,
      options_shown = paste(task$answer_choices, collapse = " | "),
      correct_answer = task$correct_answer,
      selected_answer = selected_answer,
      is_correct = identical(selected_answer, task$correct_answer),
      reaction_time_sec = reaction_time,
      chart_count = task$chart_count,
      ease_rating = input$ease_rating,
      confidence = input$confidence,
      session_id = state$participant_run_id,
      stringsAsFactors = FALSE
    ))

    save_ok <- tryCatch(
      {
        append_results_csv(result_row, RESULTS_FILE, RESULTS_LOCK_DIR)
        TRUE
      },
      error = function(e) {
        shiny::showNotification(
          paste("Жауап сақталмады. Нәтиже файлына жазу мүмкін болмады.", e$message),
          type = "error",
          duration = 7
        )
        session$sendCustomMessage("unlockSubmit", list())
        FALSE
      }
    )

    if (!save_ok) {
      return()
    }

    state$session_results <- normalize_results_table(dplyr::bind_rows(state$session_results, result_row))

    if (state$current_index < length(state$task_sequence)) {
      next_index <- state$current_index + 1
      next_task <- state$task_sequence[[next_index]]
      reset_task_display_state()
      state$current_index <- next_index
      initialize_panel_progress(next_task)
      arm_task_observer(next_task)
    } else {
      state$screen <- "complete"
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$multi_panel_submit_event, {
    shiny::req(identical(state$screen, "experiment"))

    payload <- input$multi_panel_submit_event
    panel_id <- payload$panel_id %||% ""

    if (isTRUE(state$is_submitting)) {
      if (nzchar(panel_id)) {
        session$sendCustomMessage("unlockPanel", list(panel_id = panel_id))
      }
      return()
    }

    task <- current_task()

    if (!identical(task$renderer, "multi_panel_individual")) {
      if (nzchar(panel_id)) {
        session$sendCustomMessage("unlockPanel", list(panel_id = panel_id))
      }
      return()
    }

    if (is.null(payload$task_id) || !identical(payload$task_id, task$task_id)) {
      if (nzchar(panel_id)) {
        session$sendCustomMessage("unlockPanel", list(panel_id = panel_id))
      }
      return()
    }

    if (!nzchar(panel_id) || is.null(state$panel_progress) || !panel_id %in% names(state$panel_progress)) {
      if (nzchar(panel_id)) {
        session$sendCustomMessage("unlockPanel", list(panel_id = panel_id))
      }
      return()
    }

    selected_answer <- trimws(payload$selected_answer %||% "")
    if (!nzchar(selected_answer)) {
      shiny::showNotification("Алдымен осы панель үшін жауап таңдаңыз.", type = "error")
      session$sendCustomMessage("unlockPanel", list(panel_id = panel_id))
      return()
    }

    old_progress <- state$panel_progress
    progress <- state$panel_progress

    if (isTRUE(progress[[panel_id]]$completed)) {
      session$sendCustomMessage("unlockPanel", list(panel_id = panel_id))
      return()
    }

    panel_index <- which(vapply(task$panels, function(panel) identical(panel$panel_id, panel_id), logical(1)))
    panel_meta <- task$panels[[panel_index]]

    state$is_submitting <- TRUE
    on.exit({
      state$is_submitting <- FALSE
    }, add = TRUE)

    submitted_at_client_ms <- suppressWarnings(as.numeric(payload$submitted_at_ms))
    submitted_at <- client_ms_to_posix(submitted_at_client_ms)
    if (is.na(submitted_at)) {
      submitted_at <- Sys.time()
    }

    displayed_at_client_ms <- suppressWarnings(as.numeric(progress[[panel_id]]$displayed_at_client_ms))
    if (is.na(displayed_at_client_ms)) {
      displayed_at_client_ms <- suppressWarnings(as.numeric(payload$displayed_at_ms))
      progress[[panel_id]]$displayed_at_client_ms <- displayed_at_client_ms
      state$panel_progress <- progress
    }
    displayed_at <- client_ms_to_posix(displayed_at_client_ms)
    if (is.na(displayed_at)) {
      displayed_at <- submitted_at
    }

    reaction_time <- suppressWarnings(as.numeric(payload$reaction_time_ms) / 1000)
    if (is.na(reaction_time) || reaction_time < 0) {
      reaction_time <- round(as.numeric(difftime(submitted_at, displayed_at, units = "secs")), 3)
    } else {
      reaction_time <- round(reaction_time, 3)
    }

    progress[[panel_id]]$selected_answer <- selected_answer
    progress[[panel_id]]$submitted_at_client_ms <- submitted_at_client_ms
    progress[[panel_id]]$submitted_at <- submitted_at
    progress[[panel_id]]$reaction_time_sec <- reaction_time
    progress[[panel_id]]$is_correct <- identical(selected_answer, panel_meta$correct_answer)
    progress[[panel_id]]$completed <- TRUE
    state$panel_progress <- progress

    all_completed <- all(vapply(progress, function(panel) isTRUE(panel$completed), logical(1)))

    if (!all_completed) {
      session$sendCustomMessage("markPanelAnswered", list(panel_id = panel_id))
      return()
    }

    age_value <- suppressWarnings(as.numeric(input$age))
    if (is.na(age_value) || age_value <= 0) {
      age_value <- NA_real_
    }

    result_rows <- normalize_results_table(dplyr::bind_rows(lapply(task$panels, function(panel) {
      panel_state <- progress[[panel$panel_id]]
      panel_started_at <- client_ms_to_posix(panel_state$displayed_at_client_ms)
      if (is.na(panel_started_at)) {
        panel_started_at <- panel_state$submitted_at
      }

      normalize_results_row(data.frame(
        participant_id = participant_id_current(),
        age = age_value,
        gender = trim_or_na(input$gender),
        specialization = trim_or_na(input$specialization),
        experiment_started_at = timestamp_string(state$experiment_started_at),
        started_at = timestamp_string(panel_started_at),
        submitted_at = timestamp_string(panel_state$submitted_at),
        displayed_at_client_ms = panel_state$displayed_at_client_ms,
        submitted_at_client_ms = panel_state$submitted_at_client_ms,
        task_order_position = state$current_index,
        task_id = task$task_id,
        question_id = paste0(task$question_id, "_", panel$panel_id),
        task_family = task$task_family,
        gestalt_principle = task$principle,
        visualization_type = task$visualization_type,
        question_type = task$question_type,
        question_text = task$question_text,
        options_shown = paste(panel$answer_choices, collapse = " | "),
        correct_answer = panel$correct_answer,
        selected_answer = panel_state$selected_answer,
        is_correct = panel_state$is_correct,
        reaction_time_sec = panel_state$reaction_time_sec,
        chart_count = task$chart_count,
        ease_rating = input$ease_rating,
        confidence = input$confidence,
        parent_task_id = task$task_id,
        panel_id = panel$panel_id,
        panel_label = panel$panel_label,
        panel_order = panel$panel_order,
        panel_selected_answer = panel_state$selected_answer,
        panel_is_correct = panel_state$is_correct,
        panel_reaction_time_sec = panel_state$reaction_time_sec,
        panel_submitted_at = timestamp_string(panel_state$submitted_at),
        panel_displayed_at_client_ms = panel_state$displayed_at_client_ms,
        session_id = state$participant_run_id,
        stringsAsFactors = FALSE
      ))
    })))

    save_ok <- tryCatch(
      {
        append_results_csv(result_rows, RESULTS_FILE, RESULTS_LOCK_DIR)
        TRUE
      },
      error = function(e) {
        state$panel_progress <- old_progress
        shiny::showNotification(
          paste("Жауаптар сақталмады. Файлға жазу мүмкін болмады.", e$message),
          type = "error",
          duration = 7
        )
        session$sendCustomMessage("unlockPanel", list(panel_id = panel_id))
        FALSE
      }
    )

    if (!save_ok) {
      return()
    }

    state$session_results <- normalize_results_table(dplyr::bind_rows(state$session_results, result_rows))

    if (state$current_index < length(state$task_sequence)) {
      next_index <- state$current_index + 1
      next_task <- state$task_sequence[[next_index]]
      reset_task_display_state()
      state$current_index <- next_index
      initialize_panel_progress(next_task)
      arm_task_observer(next_task)
    } else {
      state$screen <- "complete"
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$reset_experiment, {
    state$screen <- "welcome"
    state$task_sequence <- NULL
    state$current_index <- 0
    reset_task_display_state()
    state$experiment_started_at <- NULL
    state$participant_run_id <- NULL
    state$session_results <- empty_results_df()
    state$panel_progress <- NULL

    session$onFlushed(function() {
      shiny::updateTextInput(session, "participant_id", value = "")
      shiny::updateNumericInput(session, "age", value = NA)
      shiny::updateSelectInput(session, "gender", selected = "")
      shiny::updateTextInput(session, "specialization", value = "")
    }, once = TRUE)
  }, ignoreInit = TRUE)
}

shiny::shinyApp(ui = ui, server = server)
