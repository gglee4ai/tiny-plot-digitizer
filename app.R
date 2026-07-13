library(shiny)

app_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
app_dir <- getOption("digitization.point.editor.app_dir")
if (is.null(app_dir) && !is.null(app_file)) app_dir <- dirname(normalizePath(app_file))
if (is.null(app_dir) && file.exists(file.path(getwd(), "tools/digitization-point-editor/app.R"))) {
  app_dir <- normalizePath(file.path(getwd(), "tools/digitization-point-editor"))
}
if (is.null(app_dir)) app_dir <- normalizePath(getwd())
repo_dir <- normalizePath(file.path(app_dir, "../.."))
data_raw_dir <- file.path(repo_dir, "data-raw")

number_pattern <- "[-+]?[0-9]*\\.?[0-9]+"

resolve_column <- function(name, columns) {
  name <- trimws(name)
  if (name %in% columns) return(name)
  if (name == "fluence") {
    match <- grep("^fluence_.*1e22", columns, value = TRUE)
    if (length(match)) return(match[1])
  }
  NULL
}

parse_calibration <- function(lines, columns) {
  line <- grep("^# Axis calibration.*pixel_x|^# Axis calibration.*x =", lines, value = TRUE)
  if (!length(line)) return(NULL)

  formula_text <- sub("^[^:]+: *", "", line[1])
  formula_text <- sub("\\.$", "", formula_text)
  parts <- strsplit(formula_text, ";", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(NULL)

  x_text <- sub("^(pixel_x|x) *= *", "", trimws(parts[1]))
  y_text <- sub("^(pixel_y|y) *= *", "", trimws(parts[2]))

  x_log_pattern <- sprintf("^(%s) *\\+ *(%s) *\\* *log10\\(([^)]+)\\)$", number_pattern, number_pattern)
  x_linear_pattern <- sprintf("^(%s) *\\+ *(%s) *\\* *([A-Za-z0-9_]+)$", number_pattern, number_pattern)

  match <- regmatches(x_text, regexec(x_log_pattern, x_text))[[1]]
  if (length(match)) {
    x <- list(intercept = as.numeric(match[2]), scale = as.numeric(match[3]),
              column = resolve_column(match[4], columns), transform = "log10")
  } else {
    match <- regmatches(x_text, regexec(x_linear_pattern, x_text))[[1]]
    if (!length(match)) return(NULL)
    x <- list(intercept = as.numeric(match[2]), scale = as.numeric(match[3]),
              column = resolve_column(match[4], columns), transform = "linear")
  }
  if (is.null(x$column)) return(NULL)

  y_tilt_pattern <- sprintf(
    "^(%s) *\\+ *(%s) *\\* *([A-Za-z0-9_]+) *- *(%s) *\\* *([A-Za-z0-9_]+)$",
    number_pattern, number_pattern, number_pattern
  )
  y_linear_pattern <- sprintf("^(%s) *- *(%s) *\\* *([A-Za-z0-9_]+)$", number_pattern, number_pattern)

  match <- regmatches(y_text, regexec(y_tilt_pattern, y_text))[[1]]
  if (length(match)) {
    y <- list(intercept = as.numeric(match[2]), tilt = as.numeric(match[3]),
              tilt_column = resolve_column(match[4], columns), scale = as.numeric(match[5]),
              column = resolve_column(match[6], columns))
  } else {
    match <- regmatches(y_text, regexec(y_linear_pattern, y_text))[[1]]
    if (!length(match)) return(NULL)
    y <- list(intercept = as.numeric(match[2]), tilt = 0, tilt_column = NULL,
              scale = as.numeric(match[3]), column = resolve_column(match[4], columns))
  }
  if (is.null(y$column) || (y$tilt != 0 && is.null(y$tilt_column))) return(NULL)

  list(x = x, y = y, text = formula_text)
}

read_source_name <- function(lines) {
  line <- grep("^# Source figure:", lines, value = TRUE)
  if (!length(line)) return(NULL)
  trimws(sub("^# Source figure: *", "", line[1]))
}

discover_folders <- function() {
  paths <- list.dirs(data_raw_dir, recursive = FALSE, full.names = TRUE)
  setNames(paths, basename(paths))
}

discover_datasets <- function(folder_path) {
  files <- list.files(folder_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  datasets <- list()

  for (path in files) {
    lines <- readLines(path, warn = FALSE)
    source_name <- read_source_name(lines)
    if (is.null(source_name)) next

    data <- try(read.csv(path, comment.char = "#", check.names = FALSE), silent = TRUE)
    if (inherits(data, "try-error") || !nrow(data)) next

    calibration <- parse_calibration(lines, names(data))
    source_path <- file.path(dirname(path), source_name)
    if (is.null(calibration) || !file.exists(source_path)) next

    key <- substring(normalizePath(path), nchar(repo_dir) + 2)
    datasets[[key]] <- list(
      path = normalizePath(path),
      source_path = normalizePath(source_path),
      calibration = calibration,
      label = substring(normalizePath(path), nchar(normalizePath(folder_path)) + 2)
    )
  }
  datasets
}

forward_x <- function(values, calibration) {
  transformed <- if (calibration$transform == "log10") log10(values) else values
  calibration$intercept + calibration$scale * transformed
}

inverse_x <- function(pixel_x, calibration) {
  value <- (pixel_x - calibration$intercept) / calibration$scale
  if (calibration$transform == "log10") 10^value else max(0, value)
}

forward_y <- function(data, calibration) {
  tilt_value <- if (calibration$tilt == 0) 0 else calibration$tilt * data[[calibration$tilt_column]]
  calibration$intercept + tilt_value - calibration$scale * data[[calibration$column]]
}

inverse_y <- function(pixel_y, data, row, calibration) {
  tilt_value <- if (calibration$tilt == 0) 0 else calibration$tilt * data[[calibration$tilt_column]][row]
  max(0, (calibration$intercept + tilt_value - pixel_y) / calibration$scale)
}

sync_units <- function(data, row, changed_column) {
  if (changed_column == "YS_ksi" && "YS_MPa" %in% names(data)) data$YS_MPa[row] <- data$YS_ksi[row] * 6.894757
  if (changed_column == "YS_MPa" && "YS_ksi" %in% names(data)) data$YS_ksi[row] <- data$YS_MPa[row] / 6.894757
  if (changed_column == "UTS_ksi" && "UTS_MPa" %in% names(data)) data$UTS_MPa[row] <- data$UTS_ksi[row] * 6.894757
  if (changed_column == "UTS_MPa" && "UTS_ksi" %in% names(data)) data$UTS_ksi[row] <- data$UTS_MPa[row] / 6.894757
  if (changed_column == "strain_rate_min" && "strain_rate_s" %in% names(data)) data$strain_rate_s[row] <- data$strain_rate_min[row] / 60
  if (changed_column == "test_temp_C" && "test_temp_F" %in% names(data)) data$test_temp_F[row] <- data$test_temp_C[row] * 9 / 5 + 32
  data
}

format_csv_value <- function(value, column) {
  if (column %in% c("pixel_x", "pixel_y")) return(sprintf("%d", round(value)))
  if (grepl("fluence_.*1e22", column)) return(sprintf("%.3f", value))
  if (column == "strain_rate_min") return(sprintf("%.6f", value))
  if (column == "strain_rate_s") return(trimws(formatC(value, format = "fg", digits = 8)))
  if (column %in% c("test_temp_C", "test_temp_F")) return(sprintf("%.1f", value))
  if (grepl("_(MPa|ksi|pct)$", column)) return(sprintf("%.1f", value))
  as.character(value)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f7f7f5; color: #202124; }
      .container-fluid { padding: 12px 18px; }
      h3 { margin: 0 0 12px; font-size: 20px; font-weight: 600; }
      h4 { margin: 12px 0 8px; font-size: 14px; font-weight: 600; }
      .control-panel { border-right: 1px solid #d8d8d4; padding-right: 18px; min-height: calc(100vh - 48px); }
      .form-group { margin-bottom: 10px; }
      .arrow-grid { display: grid; grid-template-columns: repeat(3, 44px); grid-template-rows: repeat(3, 38px); gap: 4px; justify-content: center; margin: 8px 0 12px; }
      .arrow-grid .btn { width: 44px; height: 38px; padding: 4px; font-size: 18px; border-radius: 4px; }
      .arrow-up { grid-column: 2; grid-row: 1; }
      .arrow-left { grid-column: 1; grid-row: 2; }
      .arrow-reset { grid-column: 2; grid-row: 2; }
      .arrow-right { grid-column: 3; grid-row: 2; }
      .arrow-down { grid-column: 2; grid-row: 3; }
      .status-line { min-height: 22px; margin-top: 8px; font-size: 12px; }
      .point-nav { display: grid; grid-template-columns: 1fr 1fr; gap: 6px; margin-bottom: 10px; }
      .point-values { font-family: Menlo, Consolas, monospace; font-size: 12px; line-height: 1.55; white-space: pre-wrap; }
      .plot-title { margin: 0 0 4px; font-size: 13px; font-weight: 600; }
      .btn-primary { background: #2f5d50; border-color: #2f5d50; }
      .btn-primary:hover { background: #24483e; border-color: #24483e; }
    ")),
    tags$script(HTML("
      document.addEventListener('keydown', function(event) {
        var tag = document.activeElement && document.activeElement.tagName;
        if (tag === 'INPUT' || tag === 'SELECT' || tag === 'TEXTAREA') return;
        if (event.key === '[' || (event.shiftKey && event.key === 'ArrowLeft')) {
          event.preventDefault();
          Shiny.setInputValue('key_point_nav', 'previous', {priority: 'event'});
          return;
        }
        if (event.key === ']' || (event.shiftKey && event.key === 'ArrowRight')) {
          event.preventDefault();
          Shiny.setInputValue('key_point_nav', 'next', {priority: 'event'});
          return;
        }
        var directions = {ArrowLeft: 'left', ArrowRight: 'right', ArrowUp: 'up', ArrowDown: 'down'};
        if (directions[event.key]) {
          event.preventDefault();
          Shiny.setInputValue('key_move', directions[event.key], {priority: 'event'});
        }
      });
    "))
  ),
  h3("Digitizing Point Editor"),
  fluidRow(
    column(
      width = 3,
      div(
        class = "control-panel",
        selectInput("folder", "Folder (data-raw)", choices = setNames("", "Select folder"),
                    selected = "", selectize = FALSE),
        selectInput("dataset", "Dataset", choices = setNames("", "Select CSV"),
                    selected = "", selectize = FALSE),
        selectInput("point", "Point", choices = NULL),
        div(
          class = "point-nav",
          actionButton("previous_point", "이전 [", title = "이전 포인트 ([ 또는 Shift+←)"),
          actionButton("next_point", "다음 ]", title = "다음 포인트 (] 또는 Shift+→)")
        ),
        div(
          class = "arrow-grid",
          actionButton("up", "↑", class = "arrow-up", title = "위로 이동"),
          actionButton("left", "←", class = "arrow-left", title = "왼쪽으로 이동"),
          actionButton("undo", "↺", class = "arrow-reset", title = "선택점 되돌리기"),
          actionButton("right", "→", class = "arrow-right", title = "오른쪽으로 이동"),
          actionButton("down", "↓", class = "arrow-down", title = "아래로 이동")
        ),
        selectInput("zoom", "Zoom radius (pixel)", choices = c(20, 40, 80), selected = 40),
        div(class = "point-values", textOutput("point_values")),
        actionButton("save", "CSV 저장", class = "btn-primary", width = "100%"),
        actionButton("reload", "저장 전 변경 취소", width = "100%"),
        div(class = "status-line", textOutput("status"))
      )
    ),
    column(
      width = 6,
      div(class = "plot-title", "Source image"),
      plotOutput("overview", height = "calc(100vh - 82px)", click = "overview_click")
    ),
    column(
      width = 3,
      div(class = "plot-title", "Selected point"),
      plotOutput("zoom_plot", height = "430px")
    )
  )
)

server <- function(input, output, session) {
  folders <- reactiveVal(discover_folders())
  catalog <- reactiveVal(list())
  rv <- reactiveValues(
    data = NULL, original = NULL, image = NULL, raster = NULL,
    dataset = NULL, lines = NULL, header_index = NULL,
    columns_on_disk = NULL, changed = integer(), status = ""
  )

  clear_dataset <- function() {
    rv$data <- NULL
    rv$original <- NULL
    rv$image <- NULL
    rv$raster <- NULL
    rv$dataset <- NULL
    rv$changed <- integer()
    rv$status <- ""
    updateSelectInput(session, "point", choices = character(), selected = character())
  }

  update_folders <- function(selected = NULL) {
    choices <- c(setNames("", "Select folder"), setNames(names(folders()), names(folders())))
    if (is.null(selected)) selected <- ""
    updateSelectInput(session, "folder", choices = choices, selected = selected)
  }

  update_catalog <- function(folder_path, selected = NULL) {
    clear_dataset()
    datasets <- discover_datasets(folder_path)
    catalog(datasets)
    if (length(datasets)) {
      labels <- vapply(datasets, `[[`, character(1), "label")
      choices <- setNames(names(datasets), labels)
      if (is.null(selected)) selected <- unname(choices[1])
    } else {
      choices <- setNames("", "No compatible CSV")
      selected <- ""
    }
    updateSelectInput(session, "dataset", choices = choices, selected = selected)
  }

  observeEvent(TRUE, update_folders(), once = TRUE)

  observeEvent(input$folder, {
    req(input$folder %in% names(folders()))
    update_catalog(folders()[[input$folder]])
  })

  point_labels <- function(data, calibration) {
    x <- data[[calibration$x$column]]
    y <- data[[calibration$y$column]]
    marker <- if ("marker" %in% names(data)) data$marker else "point"
    temperature <- if ("test_temp_C" %in% names(data)) paste0(data$test_temp_C, " C") else ""
    sprintf("%02d | %s | %s | x=%s | y=%s", seq_len(nrow(data)), marker, temperature,
            format(x, digits = 4), format(y, digits = 5))
  }

  load_dataset <- function(key) {
    req(key, key %in% names(catalog()))
    dataset <- catalog()[[key]]
    lines <- readLines(dataset$path, warn = FALSE)
    data <- read.csv(dataset$path, comment.char = "#", check.names = FALSE)
    columns_on_disk <- names(data)
    calibration <- dataset$calibration

    calculated_pixel_x <- forward_x(data[[calibration$x$column]], calibration$x)
    calculated_pixel_y <- forward_y(data, calibration$y)
    data$pixel_x <- if ("pixel_x" %in% columns_on_disk) data$pixel_x else calculated_pixel_x
    data$pixel_y <- if ("pixel_y" %in% columns_on_disk) data$pixel_y else calculated_pixel_y

    source_image <- png::readPNG(dataset$source_path)
    source_gray <- if (length(dim(source_image)) == 2) source_image else source_image[, , 1]

    rv$data <- data
    rv$original <- data
    rv$image <- source_gray
    rv$raster <- as.raster(ifelse(source_gray > 0.5, "white", "black"))
    rv$dataset <- dataset
    rv$lines <- lines
    rv$header_index <- which(nzchar(lines) & !grepl("^#", lines))[1]
    rv$columns_on_disk <- columns_on_disk
    rv$changed <- integer()
    rv$status <- ""

    labels <- point_labels(data, calibration)
    updateSelectInput(session, "point", choices = setNames(seq_len(nrow(data)), labels), selected = 1)
  }

  observeEvent(input$dataset, load_dataset(input$dataset))

  selected_row <- reactive({
    req(rv$data, input$point)
    row <- as.integer(input$point)
    validate(need(row >= 1 && row <= nrow(rv$data), "Select a point"))
    row
  })

  navigate_point <- function(direction) {
    row <- selected_row()
    next_row <- if (direction == "previous") max(1, row - 1) else min(nrow(rv$data), row + 1)
    updateSelectInput(session, "point", selected = next_row)
  }

  observeEvent(input$previous_point, navigate_point("previous"))
  observeEvent(input$next_point, navigate_point("next"))
  observeEvent(input$key_point_nav, navigate_point(input$key_point_nav))

  recalculate_row <- function(data, row) {
    calibration <- rv$dataset$calibration
    x_column <- calibration$x$column
    y_column <- calibration$y$column
    data[[x_column]][row] <- inverse_x(data$pixel_x[row], calibration$x)
    data <- sync_units(data, row, x_column)
    data[[y_column]][row] <- inverse_y(data$pixel_y[row], data, row, calibration$y)
    sync_units(data, row, y_column)
  }

  move_selected <- function(direction) {
    row <- selected_row()
    data <- rv$data
    width <- ncol(rv$image)
    height <- nrow(rv$image)

    if (direction == "left") data$pixel_x[row] <- max(0, data$pixel_x[row] - 1)
    if (direction == "right") data$pixel_x[row] <- min(width, data$pixel_x[row] + 1)
    if (direction == "up") data$pixel_y[row] <- max(0, data$pixel_y[row] - 1)
    if (direction == "down") data$pixel_y[row] <- min(height, data$pixel_y[row] + 1)

    rv$data <- recalculate_row(data, row)
    rv$changed <- union(rv$changed, row)
    rv$status <- "저장되지 않은 변경"
  }

  observeEvent(input$left, move_selected("left"))
  observeEvent(input$right, move_selected("right"))
  observeEvent(input$up, move_selected("up"))
  observeEvent(input$down, move_selected("down"))
  observeEvent(input$key_move, move_selected(input$key_move))

  observeEvent(input$undo, {
    row <- selected_row()
    rv$data[row, ] <- rv$original[row, ]
    rv$changed <- setdiff(rv$changed, row)
    rv$status <- if (length(rv$changed)) "저장되지 않은 변경" else ""
  })

  observeEvent(input$reload, {
    req(input$dataset)
    load_dataset(input$dataset)
    rv$status <- "변경 취소됨"
  })

  observeEvent(input$overview_click, {
    req(rv$data)
    distance <- (rv$data$pixel_x - input$overview_click$x)^2 +
      (rv$data$pixel_y - input$overview_click$y)^2
    row <- which.min(distance)
    updateSelectInput(session, "point", selected = row)
  })

  draw_image <- function(xlim = NULL, ylim = NULL) {
    width <- ncol(rv$image)
    height <- nrow(rv$image)
    if (is.null(xlim)) xlim <- c(0, width)
    if (is.null(ylim)) ylim <- c(height, 0)

    par(mar = c(0, 0, 0, 0), bg = "white")
    plot.new()
    plot.window(xlim, ylim, xaxs = "i", yaxs = "i", asp = 1)
    rasterImage(rv$raster, 0, height, width, 0)
  }

  output$overview <- renderPlot({
    req(rv$data, rv$image)
    row <- selected_row()
    draw_image()
    points(rv$data$pixel_x, rv$data$pixel_y, pch = 3, col = "#777777", cex = 0.7, lwd = 1)
    points(rv$data$pixel_x[row], rv$data$pixel_y[row], pch = 1, col = "#c23b22", cex = 1.5, lwd = 2)
  }, res = 110)

  output$zoom_plot <- renderPlot({
    req(rv$data, rv$image)
    row <- selected_row()
    radius <- as.numeric(input$zoom)
    x <- rv$data$pixel_x[row]
    y <- rv$data$pixel_y[row]
    draw_image(c(x - radius, x + radius), c(y + radius, y - radius))
    points(rv$data$pixel_x, rv$data$pixel_y, pch = 3, col = "#777777", cex = 1.1, lwd = 1)
    points(x, y, pch = 1, col = "#c23b22", cex = 2.2, lwd = 2)
  }, res = 130)

  output$point_values <- renderText({
    req(rv$data)
    row <- selected_row()
    calibration <- rv$dataset$calibration
    sprintf(
      "pixel_x: %.0f\npixel_y: %.0f\n%s: %s\n%s: %s",
      rv$data$pixel_x[row], rv$data$pixel_y[row],
      calibration$x$column, format(rv$data[[calibration$x$column]][row], digits = 7),
      calibration$y$column, format(rv$data[[calibration$y$column]][row], digits = 7)
    )
  })

  observeEvent(input$save, {
    req(rv$data, length(rv$changed) > 0)
    calibration <- rv$dataset$calibration
    paired_columns <- c("YS_ksi", "YS_MPa", "UTS_ksi", "UTS_MPa",
                        "strain_rate_min", "strain_rate_s", "test_temp_C", "test_temp_F")
    columns <- unique(c("pixel_x", "pixel_y", calibration$x$column, calibration$y$column, paired_columns))
    columns <- intersect(columns, rv$columns_on_disk)
    header <- strsplit(rv$lines[rv$header_index], ",", fixed = TRUE)[[1]]

    for (row in rv$changed) {
      line_index <- rv$header_index + row
      fields <- strsplit(rv$lines[line_index], ",", fixed = TRUE)[[1]]
      for (column in columns) {
        index <- match(column, header)
        if (!is.na(index)) fields[index] <- format_csv_value(rv$data[[column]][row], column)
      }
      rv$lines[line_index] <- paste(fields, collapse = ",")
    }

    writeLines(rv$lines, rv$dataset$path, useBytes = TRUE)
    rv$original <- rv$data
    rv$changed <- integer()
    rv$status <- paste("저장됨:", basename(rv$dataset$path))
  })

  output$status <- renderText(rv$status)
}

shinyApp(ui, server)
