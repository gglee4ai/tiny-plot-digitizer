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
source(file.path(data_raw_dir, "01_build-helpers.R"))

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

  list(type = "formula", x = x, y = y, text = formula_text)
}

parse_projective_calibration <- function(metadata, columns) {
  box <- metadata$calibration_box
  corner_names <- c("origin", "x_axis_end", "xy_axis_end", "y_axis_end")
  if (is.null(box) || !all(corner_names %in% names(box))) return(NULL)
  if (!all(c("pixel_x", "pixel_y") %in% columns)) return(NULL)

  minimum_names <- grep("_(min|minimum)$", names(box), value = TRUE)
  axes <- lapply(minimum_names, function(minimum_name) {
    axis_name <- sub("_(min|minimum)$", "", minimum_name)
    maximum_names <- c(
      paste0(axis_name, "_max"),
      paste0(axis_name, "_maximum")
    )
    maximum_name <- maximum_names[maximum_names %in% names(box)][1]
    if (is.na(maximum_name)) return(NULL)

    zero_threshold_name <- paste0(axis_name, "_zero_threshold")
    list(
      column = axis_name,
      minimum = as.numeric(box[[minimum_name]]),
      maximum = as.numeric(box[[maximum_name]]),
      zero_threshold = if (zero_threshold_name %in% names(box)) {
        as.numeric(box[[zero_threshold_name]])
      } else {
        NULL
      }
    )
  })
  axes <- Filter(Negate(is.null), axes)
  if (length(axes) < 2) return(NULL)

  list(
    type = "projective",
    x = axes[[1]],
    y = axes[[2]],
    box = box,
    text = sprintf(
      "projective: %s [%s, %s]; %s [%s, %s]",
      axes[[1]]$column, axes[[1]]$minimum, axes[[1]]$maximum,
      axes[[2]]$column, axes[[2]]$minimum, axes[[2]]$maximum
    )
  )
}

read_source_name <- function(lines, metadata) {
  if (!is.null(metadata$source_figure)) return(as.character(metadata$source_figure))
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
    metadata <- try(read_csv_metadata(path), silent = TRUE)
    if (inherits(metadata, "try-error")) metadata <- list()
    source_name <- read_source_name(lines, metadata)
    if (is.null(source_name)) next

    data <- try(read.csv(path, comment.char = "#", check.names = FALSE), silent = TRUE)
    if (inherits(data, "try-error") || !nrow(data)) next

    calibration <- parse_projective_calibration(metadata, names(data))
    if (is.null(calibration)) calibration <- parse_calibration(lines, names(data))
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

projective_axis_values <- function(data, calibration) {
  position <- project_pixels_to_unit(
    data$pixel_x,
    data$pixel_y,
    calibration$box
  )
  x <- with(
    calibration$x,
    minimum + position$x_fraction * (maximum - minimum)
  )
  if (!is.null(calibration$x$zero_threshold)) {
    x[abs(x) < calibration$x$zero_threshold] <- 0
  }
  x <- pmax(x, calibration$x$minimum)
  y <- with(
    calibration$y,
    minimum + position$y_fraction * (maximum - minimum)
  )
  data.frame(x = x, y = y)
}

axis_values <- function(data, calibration) {
  if (calibration$type == "projective") {
    return(projective_axis_values(data, calibration))
  }
  data.frame(
    x = data[[calibration$x$column]],
    y = data[[calibration$y$column]]
  )
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
  if (!length(value) || is.na(value)) return("")
  if (column %in% c("pixel_x", "pixel_y")) return(sprintf("%d", round(value)))
  if (grepl("fluence_.*1e22", column)) return(sprintf("%.3f", value))
  if (column == "strain_rate_min") return(sprintf("%.6f", value))
  if (column == "strain_rate_s") return(trimws(formatC(value, format = "fg", digits = 8)))
  if (column %in% c("test_temp_C", "test_temp_F")) return(sprintf("%.1f", value))
  if (grepl("_(MPa|ksi|pct)$", column)) return(sprintf("%.1f", value))
  as.character(value)
}

split_csv_fields <- function(line) {
  characters <- strsplit(line, "", fixed = TRUE)[[1]]
  fields <- character()
  field <- character()
  quoted <- FALSE
  index <- 1L

  while (index <= length(characters)) {
    character <- characters[index]
    if (character == '"') {
      field <- c(field, character)
      if (quoted && index < length(characters) && characters[index + 1L] == '"') {
        field <- c(field, '"')
        index <- index + 1L
      } else {
        quoted <- !quoted
      }
    } else if (character == "," && !quoted) {
      fields <- c(fields, paste(field, collapse = ""))
      field <- character()
    } else {
      field <- c(field, character)
    }
    index <- index + 1L
  }

  c(fields, paste(field, collapse = ""))
}

escape_csv_field <- function(value) {
  if (!length(value) || is.na(value)) return("")
  value <- as.character(value)
  if (grepl('[,"\\r\\n]', value) || grepl('^\\s|\\s$', value)) {
    return(paste0('"', gsub('"', '""', value, fixed = TRUE), '"'))
  }
  value
}

update_csv_row <- function(line, data, row, columns, columns_on_disk) {
  fields <- split_csv_fields(line)
  if (length(fields) != length(columns_on_disk)) {
    stop("CSV 행의 필드 수가 헤더와 일치하지 않습니다.")
  }

  for (column in intersect(columns, columns_on_disk)) {
    index <- match(column, columns_on_disk)
    fields[index] <- escape_csv_field(format_csv_value(data[[column]][row], column))
  }
  paste(fields, collapse = ",")
}

copy_csv_fields <- function(target, source, columns, columns_on_disk) {
  target_fields <- split_csv_fields(target)
  source_fields <- split_csv_fields(source)
  if (length(target_fields) != length(columns_on_disk) ||
      length(source_fields) != length(columns_on_disk)) {
    stop("CSV 행의 필드 수가 헤더와 일치하지 않습니다.")
  }

  indexes <- match(intersect(columns, columns_on_disk), columns_on_disk)
  target_fields[indexes] <- source_fields[indexes]
  paste(target_fields, collapse = ",")
}

marker_glyph <- function(marker) {
  glyphs <- c(
    circle = "○", triangle_down = "▽", triangle_left = "◁", square = "□",
    diamond = "◇", triangle_up = "△", triangle_right = "▷",
    circle_filled = "●", square_filled = "■"
  )
  ifelse(marker %in% names(glyphs), unname(glyphs[marker]), "·")
}

build_series_catalog <- function(data, calibration) {
  if (!nrow(data)) return(NULL)

  marker <- if ("marker" %in% names(data)) as.character(data$marker) else rep("point", nrow(data))
  condition_columns <- grep(
    "temp|temperature|irradiation|condition|material|heat|fluence_reported",
    names(data), value = TRUE, ignore.case = TRUE
  )
  axis_columns <- if (calibration$type == "formula") {
    c(calibration$x$column, calibration$y$column)
  } else {
    character()
  }
  condition_columns <- setdiff(condition_columns, c("marker", "pixel_x", "pixel_y", axis_columns))
  profile_columns <- intersect(c("marker", condition_columns), names(data))

  marker_levels <- unique(marker)
  representative_rows <- vapply(marker_levels, function(marker_value) {
    rows <- which(marker == marker_value)
    if (!length(profile_columns)) return(rows[1])
    keys <- apply(data[rows, profile_columns, drop = FALSE], 1, function(values) {
      paste(ifelse(is.na(values), "<NA>", as.character(values)), collapse = "\r")
    })
    counts <- table(keys)
    rows[match(names(counts)[which.max(counts)], keys)]
  }, integer(1))

  profiles <- data[representative_rows, profile_columns, drop = FALSE]
  varying_columns <- profile_columns[vapply(profiles, function(values) {
    length(unique(ifelse(is.na(values), "<NA>", as.character(values)))) > 1
  }, logical(1))]
  label_columns <- setdiff(varying_columns, "marker")

  labels <- vapply(seq_along(marker_levels), function(index) {
    marker_value <- marker_levels[index]
    label <- paste(marker_glyph(marker_value), marker_value)
    if (length(label_columns)) {
      details <- vapply(label_columns, function(column) {
        value <- profiles[[column]][index]
        if (is.na(value)) value <- "NA"
        paste0(column, "=", value)
      }, character(1))
      label <- paste(label, paste(details, collapse = " | "), sep = " | ")
    }
    label
  }, character(1))

  list(
    profiles = profiles,
    profile_columns = profile_columns,
    representative_rows = representative_rows,
    marker_levels = marker_levels,
    choices = setNames(seq_along(marker_levels), labels)
  )
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
      .series-actions { display: grid; grid-template-columns: 1fr 38px 38px; gap: 5px; align-items: end; }
      .series-actions .form-group { margin-bottom: 10px; }
      .series-actions .btn { width: 38px; height: 34px; padding: 5px; margin-bottom: 10px; border-radius: 4px; }
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
        selectInput("dataset", "Figure", choices = setNames("", "Select figure"),
                    selected = "", selectize = FALSE),
        selectInput("point", "Point", choices = NULL),
        div(
          class = "series-actions",
          selectInput("series", "Series", choices = NULL, selectize = FALSE),
          actionButton("add_point", label = NULL, icon = icon("plus"),
                       title = "선택한 Series의 포인트 추가"),
          actionButton("delete_point", label = NULL, icon = icon("trash"),
                       title = "선택한 포인트 삭제")
        ),
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
    data = NULL, image = NULL, raster = NULL, dataset = NULL,
    prefix_lines = NULL, row_lines = NULL, suffix_lines = NULL,
    columns_on_disk = NULL, dirty = FALSE, undo_stack = list(),
    series_catalog = NULL, add_mode = FALSE, add_series = NULL,
    selected = NULL, status = "", pending_switch_status = NULL
  )

  save_changes <- function(auto = FALSE) {
    if (is.null(rv$data) || is.null(rv$dataset) || !rv$dirty) return(NULL)

    stopifnot(nrow(rv$data) == length(rv$row_lines))
    writeLines(
      c(rv$prefix_lines, rv$row_lines, rv$suffix_lines),
      rv$dataset$path,
      useBytes = TRUE
    )
    rv$dirty <- FALSE
    rv$undo_stack <- list()

    prefix <- if (auto) "자동 저장됨:" else "저장됨:"
    message <- paste(prefix, basename(rv$dataset$path))
    if (auto) {
      rv$pending_switch_status <- message
    } else {
      rv$status <- message
    }
    message
  }

  clear_dataset <- function() {
    rv$data <- NULL
    rv$image <- NULL
    rv$raster <- NULL
    rv$dataset <- NULL
    rv$prefix_lines <- NULL
    rv$row_lines <- NULL
    rv$suffix_lines <- NULL
    rv$columns_on_disk <- NULL
    rv$dirty <- FALSE
    rv$undo_stack <- list()
    rv$series_catalog <- NULL
    rv$add_mode <- FALSE
    rv$add_series <- NULL
    rv$selected <- NULL
    rv$status <- ""
    updateSelectInput(session, "point", choices = character(), selected = character())
    updateSelectInput(session, "series", choices = character(), selected = character())
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
      choices <- setNames("", "No compatible figure")
      selected <- ""
      if (!is.null(rv$pending_switch_status)) {
        rv$status <- rv$pending_switch_status
        rv$pending_switch_status <- NULL
      }
    }
    updateSelectInput(session, "dataset", choices = choices, selected = selected)
  }

  observeEvent(TRUE, update_folders(), once = TRUE)

  observeEvent(input$folder, {
    req(input$folder %in% names(folders()))
    save_changes(auto = TRUE)
    update_catalog(folders()[[input$folder]])
  })

  point_labels <- function(data, calibration) {
    values <- axis_values(data, calibration)
    marker <- if ("marker" %in% names(data)) data$marker else "point"
    temperature <- if ("test_temp_C" %in% names(data)) {
      paste0(data$test_temp_C, " C")
    } else if ("test_temp_F" %in% names(data)) {
      paste0(data$test_temp_F, " F")
    } else {
      ""
    }
    sprintf("%02d | %s | %s | x=%s | y=%s", seq_len(nrow(data)), marker, temperature,
            format(values$x, digits = 4), format(values$y, digits = 5))
  }

  series_index_for_row <- function(row) {
    series <- rv$series_catalog
    if (is.null(series)) return(NULL)
    if (!("marker" %in% names(rv$data))) return(1L)
    match(as.character(rv$data$marker[row]), series$marker_levels)
  }

  select_series_for_row <- function(row) {
    index <- series_index_for_row(row)
    if (is.null(index) || is.na(index)) return(invisible())
    freezeReactiveValue(input, "series")
    updateSelectInput(session, "series", selected = as.character(index))
  }

  refresh_controls <- function(selected = 1L) {
    req(rv$data, rv$dataset)
    selected <- max(1L, min(as.integer(selected), nrow(rv$data)))
    rv$selected <- selected
    labels <- point_labels(rv$data, rv$dataset$calibration)
    updateSelectInput(
      session, "point",
      choices = setNames(seq_len(nrow(rv$data)), labels),
      selected = selected
    )

    rv$series_catalog <- build_series_catalog(rv$data, rv$dataset$calibration)
    series <- rv$series_catalog
    freezeReactiveValue(input, "series")
    updateSelectInput(
      session, "series",
      choices = series$choices,
      selected = as.character(series_index_for_row(selected))
    )
  }

  set_add_mode <- function(active, series = NULL) {
    rv$add_mode <- active
    rv$add_series <- if (active) as.integer(series) else NULL
    updateActionButton(
      session, "add_point",
      label = NULL,
      icon = icon(if (active) "xmark" else "plus")
    )
  }

  push_undo <- function(row) {
    rv$undo_stack[[length(rv$undo_stack) + 1L]] <- list(
      data = rv$data,
      row_lines = rv$row_lines,
      selected = row,
      dirty = rv$dirty
    )
    if (length(rv$undo_stack) > 100L) rv$undo_stack <- tail(rv$undo_stack, 100L)
  }

  mark_changed <- function(status = "저장되지 않은 변경") {
    rv$dirty <- TRUE
    rv$status <- status
  }

  updated_value_columns <- function() {
    calibration <- rv$dataset$calibration
    columns <- c("pixel_x", "pixel_y")
    if (calibration$type == "formula") {
      columns <- c(
        columns, calibration$x$column, calibration$y$column,
        "YS_ksi", "YS_MPa", "UTS_ksi", "UTS_MPa",
        "strain_rate_min", "strain_rate_s", "test_temp_C", "test_temp_F"
      )
    }
    unique(intersect(columns, rv$columns_on_disk))
  }

  update_row_line <- function(row, columns = updated_value_columns()) {
    rv$row_lines[row] <- update_csv_row(
      rv$row_lines[row], rv$data, row, columns, rv$columns_on_disk
    )
  }

  load_dataset <- function(key) {
    req(key, key %in% names(catalog()))
    dataset <- catalog()[[key]]
    lines <- readLines(dataset$path, warn = FALSE)
    data <- read.csv(dataset$path, comment.char = "#", check.names = FALSE)
    columns_on_disk <- names(data)
    calibration <- dataset$calibration

    if (calibration$type == "formula") {
      calculated_pixel_x <- forward_x(data[[calibration$x$column]], calibration$x)
      calculated_pixel_y <- forward_y(data, calibration$y)
      data$pixel_x <- if ("pixel_x" %in% columns_on_disk) data$pixel_x else calculated_pixel_x
      data$pixel_y <- if ("pixel_y" %in% columns_on_disk) data$pixel_y else calculated_pixel_y
    }

    source_image <- png::readPNG(dataset$source_path)
    source_gray <- if (length(dim(source_image)) == 2) source_image else source_image[, , 1]

    header_index <- which(nzchar(lines) & !grepl("^#", lines))[1]
    body_end <- header_index + nrow(data)
    if (is.na(header_index) || body_end > length(lines)) {
      stop("CSV 본문 행을 원문과 대응시킬 수 없습니다: ", dataset$path)
    }

    rv$data <- data
    rv$image <- source_gray
    rv$raster <- as.raster(ifelse(source_gray > 0.5, "white", "black"))
    rv$dataset <- dataset
    rv$prefix_lines <- lines[seq_len(header_index)]
    rv$row_lines <- lines[header_index + seq_len(nrow(data))]
    rv$suffix_lines <- if (body_end < length(lines)) lines[(body_end + 1L):length(lines)] else character()
    rv$columns_on_disk <- columns_on_disk
    rv$dirty <- FALSE
    rv$undo_stack <- list()
    set_add_mode(FALSE)
    rv$status <- if (is.null(rv$pending_switch_status)) {
      ""
    } else {
      rv$pending_switch_status
    }
    rv$pending_switch_status <- NULL
    refresh_controls(1L)
  }

  observeEvent(input$dataset, {
    save_changes(auto = TRUE)
    load_dataset(input$dataset)
  })

  selected_row <- reactive({
    req(rv$data, rv$selected)
    row <- as.integer(rv$selected)
    validate(need(row >= 1 && row <= nrow(rv$data), "Select a point"))
    row
  })

  select_point <- function(row) {
    row <- max(1L, min(as.integer(row), nrow(rv$data)))
    rv$selected <- row
    updateSelectInput(session, "point", selected = row)
    select_series_for_row(row)
  }

  navigate_point <- function(direction) {
    row <- selected_row()
    next_row <- if (direction == "previous") max(1, row - 1) else min(nrow(rv$data), row + 1)
    select_point(next_row)
  }

  observeEvent(input$previous_point, navigate_point("previous"))
  observeEvent(input$next_point, navigate_point("next"))
  observeEvent(input$key_point_nav, navigate_point(input$key_point_nav))

  observeEvent(input$point, {
    req(rv$data, input$point)
    row <- as.integer(input$point)
    if (row >= 1L && row <= nrow(rv$data)) {
      rv$selected <- row
      select_series_for_row(row)
    }
  }, ignoreInit = TRUE)

  recalculate_row <- function(data, row) {
    calibration <- rv$dataset$calibration
    if (calibration$type == "projective") return(data)
    x_column <- calibration$x$column
    y_column <- calibration$y$column
    data[[x_column]][row] <- inverse_x(data$pixel_x[row], calibration$x)
    data <- sync_units(data, row, x_column)
    data[[y_column]][row] <- inverse_y(data$pixel_y[row], data, row, calibration$y)
    sync_units(data, row, y_column)
  }

  move_selected <- function(direction) {
    row <- selected_row()
    push_undo(row)
    data <- rv$data
    width <- ncol(rv$image)
    height <- nrow(rv$image)

    if (direction == "left") data$pixel_x[row] <- max(0, data$pixel_x[row] - 1)
    if (direction == "right") data$pixel_x[row] <- min(width, data$pixel_x[row] + 1)
    if (direction == "up") data$pixel_y[row] <- max(0, data$pixel_y[row] - 1)
    if (direction == "down") data$pixel_y[row] <- min(height, data$pixel_y[row] + 1)

    rv$data <- recalculate_row(data, row)
    update_row_line(row)
    mark_changed()
    refresh_controls(row)
  }

  observeEvent(input$left, move_selected("left"))
  observeEvent(input$right, move_selected("right"))
  observeEvent(input$up, move_selected("up"))
  observeEvent(input$down, move_selected("down"))
  observeEvent(input$key_move, move_selected(input$key_move))

  observeEvent(input$undo, {
    if (!length(rv$undo_stack)) {
      rv$status <- "되돌릴 변경이 없습니다"
      return()
    }
    snapshot <- rv$undo_stack[[length(rv$undo_stack)]]
    rv$undo_stack[[length(rv$undo_stack)]] <- NULL
    rv$data <- snapshot$data
    rv$row_lines <- snapshot$row_lines
    rv$dirty <- snapshot$dirty
    set_add_mode(FALSE)
    rv$status <- if (rv$dirty) "저장되지 않은 변경" else ""
    refresh_controls(snapshot$selected)
  })

  observeEvent(input$reload, {
    req(input$dataset)
    load_dataset(input$dataset)
    rv$status <- "변경 취소됨"
  })

  observeEvent(input$series, {
    req(rv$data, rv$series_catalog, input$series)
    row <- selected_row()
    target_index <- as.integer(input$series)
    current_index <- series_index_for_row(row)
    if (is.na(target_index) || identical(target_index, current_index)) return()

    series <- rv$series_catalog
    source_row <- series$representative_rows[target_index]
    columns <- series$profile_columns
    push_undo(row)

    rv$row_lines[row] <- copy_csv_fields(
      rv$row_lines[row], rv$row_lines[source_row], columns, rv$columns_on_disk
    )
    for (column in columns) rv$data[[column]][row] <- rv$data[[column]][source_row]
    rv$data <- recalculate_row(rv$data, row)
    update_row_line(row)
    mark_changed("Series가 정정되었습니다")
    refresh_controls(row)
  }, ignoreInit = TRUE)

  observeEvent(input$add_point, {
    req(rv$data, input$series)
    if (rv$add_mode) {
      set_add_mode(FALSE)
      rv$status <- if (rv$dirty) "저장되지 않은 변경" else ""
    } else {
      set_add_mode(TRUE, input$series)
      rv$status <- "원본 그림에서 새 포인트의 중심을 클릭하세요"
    }
  })

  observeEvent(input$delete_point, {
    row <- selected_row()
    if (nrow(rv$data) == 1L) {
      rv$status <- "마지막 포인트는 삭제할 수 없습니다"
      return()
    }
    showModal(modalDialog(
      title = "포인트 삭제",
      sprintf("선택한 포인트(%d번)를 삭제하시겠습니까?", row),
      footer = tagList(
        modalButton("취소"),
        actionButton("confirm_delete", "삭제", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_delete, {
    row <- selected_row()
    req(nrow(rv$data) > 1L)
    push_undo(row)
    rv$data <- rv$data[-row, , drop = FALSE]
    rv$row_lines <- rv$row_lines[-row]
    mark_changed("포인트가 삭제되었습니다")
    set_add_mode(FALSE)
    removeModal()
    refresh_controls(min(row, nrow(rv$data)))
  })

  observeEvent(input$overview_click, {
    req(rv$data)
    if (rv$add_mode) {
      series <- rv$series_catalog
      series_index <- rv$add_series
      req(series_index >= 1L, series_index <= nrow(series$profiles))
      source_row <- series$representative_rows[series_index]
      x <- round(max(0, min(ncol(rv$image), input$overview_click$x)))
      y <- round(max(0, min(nrow(rv$image), input$overview_click$y)))
      marker_value <- series$marker_levels[series_index]
      marker_rows <- if ("marker" %in% names(rv$data)) {
        which(as.character(rv$data$marker) == marker_value)
      } else {
        seq_len(nrow(rv$data))
      }
      rows_before <- marker_rows[rv$data$pixel_x[marker_rows] <= x]
      insert_at <- if (length(rows_before)) {
        rows_before[which.max(rv$data$pixel_x[rows_before])] + 1L
      } else {
        min(marker_rows)
      }

      push_undo(selected_row())
      new_row <- rv$data[source_row, , drop = FALSE]
      new_row$pixel_x <- x
      new_row$pixel_y <- y
      new_line <- rv$row_lines[source_row]

      before <- if (insert_at > 1L) seq_len(insert_at - 1L) else integer()
      after <- if (insert_at <= nrow(rv$data)) insert_at:nrow(rv$data) else integer()
      rv$data <- rbind(rv$data[before, , drop = FALSE], new_row, rv$data[after, , drop = FALSE])
      rv$row_lines <- c(rv$row_lines[before], new_line, rv$row_lines[after])
      rv$data <- recalculate_row(rv$data, insert_at)
      update_row_line(insert_at)
      mark_changed("새 포인트가 추가되었습니다")
      set_add_mode(FALSE)
      refresh_controls(insert_at)
      return()
    }

    distance <- (rv$data$pixel_x - input$overview_click$x)^2 +
      (rv$data$pixel_y - input$overview_click$y)^2
    row <- which.min(distance)
    select_point(row)
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
    selected_color <- if (rv$add_mode) "#2f5d50" else "#c23b22"
    points(rv$data$pixel_x[row], rv$data$pixel_y[row], pch = 1, col = selected_color, cex = 1.5, lwd = 2)
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
    values <- axis_values(rv$data, calibration)
    sprintf(
      "pixel_x: %.0f\npixel_y: %.0f\n%s: %s\n%s: %s",
      rv$data$pixel_x[row], rv$data$pixel_y[row],
      calibration$x$column, format(values$x[row], digits = 7),
      calibration$y$column, format(values$y[row], digits = 7)
    )
  })

  observeEvent(input$save, {
    req(!is.null(save_changes(auto = FALSE)))
  })

  output$status <- renderText(rv$status)
}

shinyApp(ui, server)
