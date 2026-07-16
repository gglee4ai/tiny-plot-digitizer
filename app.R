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

round_pixel_coordinate <- function(value) {
  round(suppressWarnings(as.numeric(value)) * 2) / 2
}

format_pixel_coordinate <- function(value) {
  format(round_pixel_coordinate(value), scientific = FALSE, trim = TRUE)
}

project_format <- "Tiny Plot Digitizer 1.0"

parse_projective_calibration <- function(metadata, columns) {
  if (!identical(as.character(metadata$format), project_format)) return(NULL)

  box <- metadata$box_points
  corner_names <- c("origin", "x_axis_end", "xy_axis_end", "y_axis_end")
  if (is.null(box) || !all(corner_names %in% names(box))) return(NULL)
  if (!all(c("pixel_x", "pixel_y") %in% columns)) return(NULL)

  axis_names <- metadata$axis_names
  axis_keys <- c(x = "x_axis", y = "y_axis")
  if (is.null(axis_names) || !all(axis_keys %in% names(axis_names))) return(NULL)
  axis_names <- vapply(axis_keys, function(axis) {
    as.character(axis_names[[axis]])
  }, character(1))
  if (any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", axis_names)) ||
      anyDuplicated(axis_names) || !all(axis_names %in% columns)) {
    return(NULL)
  }

  saved_points <- metadata$axis_points
  point_names <- c("x1", "x2", "y1", "y2")
  if (is.null(saved_points) || !all(point_names %in% names(saved_points))) return(NULL)

  axis_points <- lapply(point_names, function(name) {
    point <- saved_points[[name]]
    coordinate_name <- if (startsWith(name, "x")) "pixel_x" else "pixel_y"
    if (!all(c(coordinate_name, "value") %in% names(point))) return(NULL)
    coordinate <- as.numeric(point[[coordinate_name]])
    value <- as.numeric(point$value)
    if (!is.finite(coordinate) || !is.finite(value)) return(NULL)

    end_name <- if (startsWith(name, "x")) "x_axis_end" else "y_axis_end"
    coordinate_index <- if (startsWith(name, "x")) 1L else 2L
    start <- c(as.numeric(box$origin$pixel_x), as.numeric(box$origin$pixel_y))
    end <- c(as.numeric(box[[end_name]]$pixel_x), as.numeric(box[[end_name]]$pixel_y))
    extent <- end[coordinate_index] - start[coordinate_index]
    if (!all(is.finite(c(start, end))) || abs(extent) <= 1e-8) return(NULL)

    fraction <- (coordinate - start[coordinate_index]) / extent
    if (!is.finite(fraction) || fraction < -1e-8 || fraction > 1 + 1e-8) return(NULL)
    fraction <- max(0, min(1, fraction))
    pixel <- start + fraction * (end - start)
    default_fraction <- if (name %in% c("x1", "y1")) 0 else 1
    list(
      pixel_x = pixel[1], pixel_y = pixel[2], value = value,
      source = if (abs(fraction - default_fraction) <= 1e-8) "box" else "new",
      fraction = fraction
    )
  })
  if (any(vapply(axis_points, is.null, logical(1)))) return(NULL)
  names(axis_points) <- point_names

  axes <- lapply(c("x", "y"), function(axis) {
    point_prefix <- if (axis == "x") "x" else "y"
    minimum_name <- paste0(axis_names[[axis]], "_min")
    maximum_name <- paste0(axis_names[[axis]], "_max")
    list(
      column = axis_names[[axis]],
      minimum_name = minimum_name,
      maximum_name = maximum_name,
      minimum = axis_points[[paste0(point_prefix, "1")]]$value,
      maximum = axis_points[[paste0(point_prefix, "2")]]$value,
      zero_threshold_name = NULL,
      zero_threshold = NULL
    )
  })
  names(axes) <- c("x", "y")
  box[[axes$x$minimum_name]] <- axes$x$minimum
  box[[axes$x$maximum_name]] <- axes$x$maximum
  box[[axes$y$minimum_name]] <- axes$y$minimum
  box[[axes$y$maximum_name]] <- axes$y$maximum

  calibration <- list(
    type = "projective",
    x = axes$x,
    y = axes$y,
    box = box,
    axis_points = axis_points,
    text = sprintf(
      "projective: %s [%s, %s]; %s [%s, %s]",
      axes$x$column, axes$x$minimum, axes$x$maximum,
      axes$y$column, axes$y$minimum, axes$y$maximum
    )
  )
  calibration <- normalize_calibration_axis_points(calibration)
  updated <- rebuild_calibration_ranges(calibration)
  if (is.null(updated) || !valid_projective_calibration(updated)) return(NULL)
  updated
}

axis_values <- function(data, calibration) {
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

calibration_corners <- function(calibration, close = FALSE) {
  corner_names <- c("origin", "x_axis_end", "xy_axis_end", "y_axis_end")
  if (close) corner_names <- c(corner_names, "origin")
  data.frame(
    corner = corner_names,
    pixel_x = vapply(
      corner_names,
      function(name) as.numeric(calibration$box[[name]]$pixel_x),
      numeric(1)
    ),
    pixel_y = vapply(
      corner_names,
      function(name) as.numeric(calibration$box[[name]]$pixel_y),
      numeric(1)
    )
  )
}

calibration_axis_points <- function(calibration) {
  point_names <- c("x1", "x2", "y1", "y2")
  data.frame(
    axis_point = point_names,
    pixel_x = vapply(
      point_names,
      function(name) as.numeric(calibration$axis_points[[name]]$pixel_x),
      numeric(1)
    ),
    pixel_y = vapply(
      point_names,
      function(name) as.numeric(calibration$axis_points[[name]]$pixel_y),
      numeric(1)
    ),
    value = vapply(
      point_names,
      function(name) as.numeric(calibration$axis_points[[name]]$value),
      numeric(1)
    ),
    source = vapply(
      point_names,
      function(name) as.character(calibration$axis_points[[name]]$source),
      character(1)
    ),
    fraction = vapply(
      point_names,
      function(name) as.numeric(calibration$axis_points[[name]]$fraction),
      numeric(1)
    )
  )
}

valid_calibration_axis_points <- function(axis_points) {
  point_names <- c("x1", "x2", "y1", "y2")
  if (is.null(axis_points) || !all(point_names %in% names(axis_points))) return(FALSE)
  all(vapply(point_names, function(name) {
    point <- axis_points[[name]]
    all(c("pixel_x", "pixel_y", "value") %in% names(point)) &&
      all(is.finite(as.numeric(unlist(point[c("pixel_x", "pixel_y", "value")]))))
  }, logical(1)))
}

axis_point_box_corner <- function(axis_point) {
  c(x1 = "origin", x2 = "x_axis_end", y1 = "origin", y2 = "y_axis_end")[[axis_point]]
}

axis_edge <- function(calibration, axis_point) {
  end_name <- if (startsWith(axis_point, "x")) "x_axis_end" else "y_axis_end"
  list(
    start = c(
      as.numeric(calibration$box$origin$pixel_x),
      as.numeric(calibration$box$origin$pixel_y)
    ),
    end = c(
      as.numeric(calibration$box[[end_name]]$pixel_x),
      as.numeric(calibration$box[[end_name]]$pixel_y)
    )
  )
}

project_to_axis_edge <- function(pixel_x, pixel_y, edge) {
  vector <- edge$end - edge$start
  fraction <- sum((c(pixel_x, pixel_y) - edge$start) * vector) / sum(vector^2)
  max(0, min(1, fraction))
}

normalize_calibration_axis_points <- function(calibration) {
  for (name in c("origin", "x_axis_end", "y_axis_end", "xy_axis_end")) {
    calibration$box[[name]]$pixel_x <- round_pixel_coordinate(calibration$box[[name]]$pixel_x)
    calibration$box[[name]]$pixel_y <- round_pixel_coordinate(calibration$box[[name]]$pixel_y)
  }
  for (name in c("x1", "x2", "y1", "y2")) {
    point <- calibration$axis_points[[name]]
    corner <- calibration$box[[axis_point_box_corner(name)]]
    corner_pixel <- c(as.numeric(corner$pixel_x), as.numeric(corner$pixel_y))
    point_pixel <- c(as.numeric(point$pixel_x), as.numeric(point$pixel_y))
    default_fraction <- if (name %in% c("x1", "y1")) 0 else 1
    source <- if (!is.null(point$source) && as.character(point$source) %in% c("box", "new")) {
      as.character(point$source)
    } else if (all(abs(point_pixel - corner_pixel) < 1e-6)) {
      "box"
    } else {
      "new"
    }
    edge <- axis_edge(calibration, name)
    fraction <- if (source == "box") {
      default_fraction
    } else if (!is.null(point$fraction) && is.finite(as.numeric(point$fraction))) {
      max(0, min(1, as.numeric(point$fraction)))
    } else {
      project_to_axis_edge(point_pixel[1], point_pixel[2], edge)
    }
    pixel <- if (source == "box") corner_pixel else edge$start + fraction * (edge$end - edge$start)
    if (source == "new") {
      coordinate_index <- if (startsWith(name, "x")) 1L else 2L
      edge_extent <- edge$end[coordinate_index] - edge$start[coordinate_index]
      if (is.finite(edge_extent) && abs(edge_extent) > 1e-8) {
        coordinate <- round_pixel_coordinate(pixel[coordinate_index])
        fraction <- max(0, min(1, (coordinate - edge$start[coordinate_index]) / edge_extent))
        pixel <- round_pixel_coordinate(edge$start + fraction * (edge$end - edge$start))
      }
    }
    calibration$axis_points[[name]] <- list(
      pixel_x = pixel[1], pixel_y = pixel[2], value = as.numeric(point$value),
      source = source, fraction = fraction
    )
  }
  calibration
}

rebuild_calibration_ranges <- function(calibration) {
  calibration <- normalize_calibration_axis_points(calibration)
  points <- calibration_axis_points(calibration)
  calculate_range <- function(first_name, second_name) {
    first <- points[points$axis_point == first_name, ]
    second <- points[points$axis_point == second_name, ]
    if (second$fraction - first$fraction <= 1e-8 || second$value <= first$value) return(NULL)
    slope <- (second$value - first$value) / (second$fraction - first$fraction)
    c(minimum = first$value - first$fraction * slope,
      maximum = first$value + (1 - first$fraction) * slope)
  }
  x_range <- calculate_range("x1", "x2")
  y_range <- calculate_range("y1", "y2")
  if (is.null(x_range) || is.null(y_range)) return(NULL)

  calibration$x$minimum <- x_range[["minimum"]]
  calibration$x$maximum <- x_range[["maximum"]]
  calibration$y$minimum <- y_range[["minimum"]]
  calibration$y$maximum <- y_range[["maximum"]]
  calibration$box[[calibration$x$minimum_name]] <- calibration$x$minimum
  calibration$box[[calibration$x$maximum_name]] <- calibration$x$maximum
  calibration$box[[calibration$y$minimum_name]] <- calibration$y$minimum
  calibration$box[[calibration$y$maximum_name]] <- calibration$y$maximum
  calibration
}

rename_calibration_axis <- function(calibration, axis, new_name) {
  stopifnot(axis %in% c("x", "y"), calibration$type == "projective")
  new_name <- trimws(new_name)
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", new_name)) {
    stop("축 이름은 영문자로 시작하고 영문, 숫자, 밑줄만 사용할 수 있습니다.")
  }
  if (new_name %in% c("point_id", "group", "series_id", "pixel_x", "pixel_y")) {
    stop("포인트 정보 열과 같은 이름은 축 이름으로 사용할 수 없습니다.")
  }

  other_axis <- if (axis == "x") "y" else "x"
  if (identical(new_name, calibration[[other_axis]]$column)) {
    stop("X축과 Y축 이름은 서로 달라야 합니다.")
  }

  axis_settings <- calibration[[axis]]
  old_name <- axis_settings$column
  if (identical(new_name, old_name)) return(calibration)

  key_fields <- c("minimum_name", "maximum_name", "zero_threshold_name")
  old_keys <- unlist(axis_settings[key_fields], use.names = FALSE)
  old_keys <- old_keys[!is.na(old_keys) & nzchar(old_keys)]
  suffixes <- substring(old_keys, nchar(old_name) + 1L)
  new_keys <- paste0(new_name, suffixes)
  conflicting_keys <- setdiff(new_keys, old_keys)
  if (any(conflicting_keys %in% names(calibration$box))) {
    stop("같은 이름의 축 보정 메타데이터가 이미 있습니다.")
  }

  box_names <- names(calibration$box)
  box_names[match(old_keys, box_names)] <- new_keys
  names(calibration$box) <- box_names
  for (field in key_fields) {
    old_key <- axis_settings[[field]]
    if (!is.null(old_key)) {
      axis_settings[[field]] <- new_keys[match(old_key, old_keys)]
    }
  }
  axis_settings$column <- new_name
  calibration[[axis]] <- axis_settings
  calibration$text <- sprintf(
    "projective: %s [%s, %s]; %s [%s, %s]",
    calibration$x$column, calibration$x$minimum, calibration$x$maximum,
    calibration$y$column, calibration$y$minimum, calibration$y$maximum
  )
  calibration
}

project_unit_to_pixels <- function(x_fraction, y_fraction, calibration) {
  corner_names <- c("origin", "x_axis_end", "xy_axis_end", "y_axis_end")
  unit_corners <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1), ncol = 2, byrow = TRUE)
  pixel_corners <- calibration_corners(calibration)
  system <- matrix(0, nrow = 8, ncol = 8)
  response <- numeric(8)

  for (index in seq_along(corner_names)) {
    u <- unit_corners[index, 1]
    v <- unit_corners[index, 2]
    x <- pixel_corners$pixel_x[index]
    y <- pixel_corners$pixel_y[index]
    system[2 * index - 1, ] <- c(u, v, 1, 0, 0, 0, -x * u, -x * v)
    system[2 * index, ] <- c(0, 0, 0, u, v, 1, -y * u, -y * v)
    response[2 * index - 1] <- x
    response[2 * index] <- y
  }

  projective <- solve(system, response)
  denominator <- projective[7] * x_fraction + projective[8] * y_fraction + 1
  data.frame(
    pixel_x = (
      projective[1] * x_fraction + projective[2] * y_fraction + projective[3]
    ) / denominator,
    pixel_y = (
      projective[4] * x_fraction + projective[5] * y_fraction + projective[6]
    ) / denominator
  )
}

draw_calibration_grid <- function(
  calibration, selected_box_point = NULL, selected_axis_point = NULL,
  box_only = FALSE
) {
  if (is.null(calibration) || calibration$type != "projective") return(invisible())

  corners <- calibration_corners(calibration, close = TRUE)
  if (box_only) {
    lines(corners$pixel_x, corners$pixel_y, col = "#1f5fbf", lwd = 1.4)
    return(invisible())
  }

  for (fraction in c(0.25, 0.5, 0.75)) {
    vertical <- project_unit_to_pixels(c(fraction, fraction), c(0, 1), calibration)
    horizontal <- project_unit_to_pixels(c(0, 1), c(fraction, fraction), calibration)
    lines(vertical$pixel_x, vertical$pixel_y, col = "#4f81bd80", lwd = 0.8)
    lines(horizontal$pixel_x, horizontal$pixel_y, col = "#4f81bd80", lwd = 0.8)
  }

  lines(corners$pixel_x, corners$pixel_y, col = "#1f5fbf", lwd = 1.4)
  corner_points <- corners[seq_len(4), ]
  unselected_corners <- is.null(selected_box_point) |
    corner_points$corner != selected_box_point
  points(
    corner_points$pixel_x[unselected_corners], corner_points$pixel_y[unselected_corners],
    pch = 16, col = "#1f5fbf", cex = 0.55
  )

  axis_points <- calibration_axis_points(calibration)
  triangle_size <- 0.0875
  triangle_half_width <- xinch(triangle_size / 2)
  triangle_half_height <- yinch(triangle_size / 2)
  for (index in seq_len(nrow(axis_points))) {
    x <- axis_points$pixel_x[index]
    y <- axis_points$pixel_y[index]
    point_color <- if (
      !is.null(selected_axis_point) && axis_points$axis_point[index] == selected_axis_point
    ) "#d62728" else "#1f5fbf"
    if (startsWith(axis_points$axis_point[index], "x")) {
      polygon(
        c(x, x - triangle_half_width, x + triangle_half_width),
        c(y + triangle_half_height, y - triangle_half_height, y - triangle_half_height),
        col = point_color, border = point_color
      )
    } else {
      polygon(
        c(x + triangle_half_width, x - triangle_half_width, x - triangle_half_width),
        c(y, y + triangle_half_height, y - triangle_half_height),
        col = point_color, border = point_color
      )
    }
  }

  if (!is.null(selected_box_point) && selected_box_point %in% corner_points$corner) {
    selected <- corner_points[corner_points$corner == selected_box_point, ]
    points(selected$pixel_x, selected$pixel_y,
           pch = 16, col = "#d62728", cex = 0.7)
  }

  invisible()
}

valid_projective_calibration <- function(calibration) {
  corners <- calibration_corners(calibration)
  if (nrow(unique(corners[c("pixel_x", "pixel_y")])) != 4L) return(FALSE)

  next_index <- c(2L, 3L, 4L, 1L)
  following_index <- c(3L, 4L, 1L, 2L)
  cross_products <- vapply(seq_len(4), function(index) {
    first <- c(
      corners$pixel_x[next_index[index]] - corners$pixel_x[index],
      corners$pixel_y[next_index[index]] - corners$pixel_y[index]
    )
    second <- c(
      corners$pixel_x[following_index[index]] - corners$pixel_x[next_index[index]],
      corners$pixel_y[following_index[index]] - corners$pixel_y[next_index[index]]
    )
    first[1] * second[2] - first[2] * second[1]
  }, numeric(1))
  convex <- all(cross_products > 1e-6) || all(cross_products < -1e-6)
  if (!convex) return(FALSE)

  projected <- try(project_unit_to_pixels(0.5, 0.5, calibration), silent = TRUE)
  !inherits(projected, "try-error") && all(is.finite(unlist(projected)))
}

marker_glyph <- function(marker) {
  pch <- suppressWarnings(as.integer(marker))
  pch_glyphs <- c(
    "□", "○", "△", "+", "×", "◇", "▽", "⊠", "*", "◇+",
    "⊕", "△▽", "⊞", "⊗", "□△", "■", "●", "▲", "◆"
  )
  if (!is.na(pch) && pch >= 0L && pch <= 18L) return(pch_glyphs[pch + 1L])
  glyphs <- c(
    circle = "○", triangle_down = "▽", triangle_left = "◁", square = "□",
    diamond = "◇", triangle_up = "△", triangle_right = "▷",
    circle_filled = "●", square_filled = "■", cross = "×"
  )
  ifelse(marker %in% names(glyphs), unname(glyphs[marker]), "·")
}

marker_pch <- function(marker) {
  pch <- suppressWarnings(as.integer(marker))
  if (!is.na(pch) && pch >= 0L && pch <= 25L) return(pch)
  marker_glyph(marker)
}

source_image_metadata <- function(metadata) {
  value <- metadata$source_image
  if (is.null(value) || is.null(value$filename) || is.null(value$size) ||
      !all(c("width", "height") %in% names(value$size))) {
    return(NULL)
  }
  width <- suppressWarnings(as.numeric(value$size$width))
  height <- suppressWarnings(as.numeric(value$size$height))
  if (!is.finite(width) || !is.finite(height) || width <= 0 || height <= 0 ||
      width != round(width) || height != round(height)) {
    return(NULL)
  }
  list(
    filename = basename(as.character(value$filename)),
    width = as.integer(width),
    height = as.integer(height)
  )
}

is_digitizing_project_file <- function(path, source_path) {
  if (!file.exists(path)) return(FALSE)
  metadata <- try(read_csv_metadata(path), silent = TRUE)
  if (inherits(metadata, "try-error") ||
      !identical(as.character(metadata$format), project_format)) {
    return(FALSE)
  }
  source_metadata <- source_image_metadata(metadata)
  if (is.null(source_metadata) ||
      !identical(source_metadata$filename, basename(source_path))) {
    return(FALSE)
  }
  columns <- try(
    names(read.csv(path, comment.char = "#", check.names = FALSE, nrows = 1)),
    silent = TRUE
  )
  if (inherits(columns, "try-error")) return(FALSE)
  all(c("pixel_x", "pixel_y") %in% columns) &&
    "group" %in% columns &&
    !is.null(metadata$axis_names) && !is.null(metadata$box_points) &&
    !is.null(metadata$axis_points) && !is.null(metadata$display_styles)
}

discover_images <- function(folder_path) {
  files <- list.files(
    folder_path, pattern = "\\.png$", recursive = FALSE,
    full.names = TRUE, ignore.case = TRUE
  )
  images <- lapply(files, function(path) {
    normalized <- normalizePath(path)
    list(
      source_path = normalized,
      label = basename(normalized)
    )
  })
  keys <- vapply(images, `[[`, character(1), "source_path")
  setNames(images, keys)
}

discover_projects <- function(folder_path) {
  files <- list.files(
    folder_path, pattern = "\\.csv$", recursive = FALSE,
    full.names = TRUE, ignore.case = TRUE
  )
  projects <- lapply(files, function(path) {
    metadata <- try(read_csv_metadata(path), silent = TRUE)
    if (inherits(metadata, "try-error")) return(NULL)
    source_metadata <- source_image_metadata(metadata)
    if (is.null(source_metadata)) return(NULL)
    source_path <- file.path(folder_path, source_metadata$filename)
    if (!file.exists(source_path) || !is_digitizing_project_file(path, source_path)) {
      return(NULL)
    }
    project_path <- normalizePath(path)
    list(
      key = project_path,
      source_path = normalizePath(source_path),
      load_path = project_path,
      label = basename(project_path)
    )
  })
  projects <- Filter(Negate(is.null), projects)
  keys <- vapply(projects, `[[`, character(1), "key")
  setNames(projects, keys)
}

empty_points <- function() {
  data.frame(
    point_id = integer(), series_id = integer(),
    pixel_x = numeric(), pixel_y = numeric()
  )
}

empty_series <- function() {
  data.frame(
    id = integer(), name = character(), marker = character(), color = character(),
    size = numeric(), alpha = numeric(),
    stringsAsFactors = FALSE
  )
}

new_project_calibration <- function(width, height) {
  box <- list(
    origin = list(pixel_x = 0, pixel_y = height),
    x_axis_end = list(pixel_x = width, pixel_y = height),
    xy_axis_end = list(pixel_x = width, pixel_y = 0),
    y_axis_end = list(pixel_x = 0, pixel_y = 0)
  )
  parse_projective_calibration(
    list(
      format = project_format,
      axis_names = list(x_axis = "x", y_axis = "y"),
      box_points = box,
      axis_points = list(
        x1 = list(pixel_x = 0, value = 0),
        x2 = list(pixel_x = width, value = 1),
        y1 = list(pixel_y = height, value = 0),
        y2 = list(pixel_y = 0, value = 1)
      )
    ),
    c("pixel_x", "pixel_y", "x", "y")
  )
}

series_marker_choices <- c(
  "+ 십자" = "3",
  "× 엑스" = "4",
  "○ 원" = "1",
  "□ 사각형" = "0",
  "◇ 마름모" = "5"
)

group_color_choices <- c(
  "빨강" = "#d62728",
  "파랑" = "#1f77b4",
  "초록" = "#2ca02c",
  "주황" = "#ff7f0e",
  "보라" = "#9467bd",
  "청록" = "#17becf"
)
series_palette <- unname(group_color_choices)

group_style_defaults <- function(id) {
  combination <- (as.integer(id) - 1L) %%
    (length(series_palette) * length(series_marker_choices))
  color_index <- combination %% length(series_palette) + 1L
  marker_index <- combination %/% length(series_palette) + 1L
  list(
    marker = unname(series_marker_choices[marker_index]),
    color = series_palette[color_index],
    size = 1,
    alpha = 1
  )
}

default_groups <- function() {
  style <- group_style_defaults(1L)
  data.frame(
    id = 1L,
    name = "group01",
    marker = style$marker,
    color = style$color,
    size = style$size,
    alpha = style$alpha,
    stringsAsFactors = FALSE
  )
}

series_for_save <- function(series, data) {
  used_ids <- if (nrow(data)) unique(as.integer(data$series_id)) else integer()
  series[series$id %in% used_ids, , drop = FALSE]
}

restore_default_groups <- function(series) {
  defaults <- default_groups()
  missing_defaults <- defaults[!defaults$id %in% series$id, , drop = FALSE]
  series <- rbind(series, missing_defaults)
  series <- series[order(series$id), , drop = FALSE]
  rownames(series) <- NULL
  series
}

series_from_metadata <- function(value) {
  if (is.null(value) || !length(value)) return(empty_series())
  item_names <- names(value)
  if (is.null(item_names) || any(!nzchar(item_names))) {
    stop("display_styles 메타데이터의 그룹 명칭을 읽을 수 없습니다")
  }
  rows <- lapply(seq_along(value), function(index) {
    item <- value[[index]]
    required <- c("symbol", "color", "size", "alpha")
    if (!all(required %in% names(item))) {
      stop("display_styles 메타데이터의 필드를 확인하세요")
    }
    data.frame(
      id = index,
      name = item_names[index],
      marker = as.character(item$symbol),
      color = as.character(item$color),
      size = as.numeric(item$size),
      alpha = as.numeric(item$alpha),
      stringsAsFactors = FALSE
    )
  })
  series <- do.call(rbind, rows)
  rownames(series) <- NULL
  if (anyDuplicated(series$name)) stop("그룹 명칭이 중복되어 있습니다")
  series
}

yaml_quote <- function(value) {
  encodeString(as.character(value), quote = '"', na.encode = FALSE)
}

format_yaml_number <- function(value) {
  value <- as.numeric(value)
  if (length(value) != 1L || !is.finite(value)) {
    stop("YAML 숫자가 유효하지 않습니다")
  }
  format(value, scientific = FALSE, trim = TRUE, digits = 15)
}

serialize_project_metadata <- function(
    source_image, image_width, image_height, calibration, series) {
  if (anyDuplicated(series$name)) stop("그룹 명칭이 중복되어 저장할 수 없습니다")
  number <- format_yaml_number
  axis_lines <- vapply(c("x1", "x2", "y1", "y2"), function(name) {
    point <- calibration$axis_points[[name]]
    coordinate <- if (startsWith(name, "x")) "pixel_x" else "pixel_y"
    sprintf(
      "  %s: {%s: %s, value: %s}",
      name, coordinate, number(point[[coordinate]]), number(point$value)
    )
  }, character(1))

  corner_names <- c("origin", "x_axis_end", "y_axis_end", "xy_axis_end")
  corner_lines <- vapply(corner_names, function(name) {
    point <- calibration$box[[name]]
    sprintf(
      "  %s: {pixel_x: %s, pixel_y: %s}",
      name, number(point$pixel_x), number(point$pixel_y)
    )
  }, character(1))
  style_lines <- if (nrow(series)) {
    vapply(seq_len(nrow(series)), function(index) {
      sprintf(
        "  %s: {symbol: %s, color: %s, size: %s, alpha: %s}",
        yaml_quote(series$name[index]),
        number(series$marker[index]), yaml_quote(series$color[index]),
        number(series$size[index]), number(series$alpha[index])
      )
    }, character(1))
  } else {
    character()
  }

  c(
    paste0("format: ", yaml_quote(project_format)),
    "source_image:",
    paste0("  filename: ", yaml_quote(basename(source_image))),
    sprintf(
      "  size: {width: %s, height: %s}",
      number(image_width), number(image_height)
    ),
    sprintf(
      "axis_names: {x_axis: %s, y_axis: %s}",
      yaml_quote(calibration$x$column), yaml_quote(calibration$y$column)
    ),
    "box_points:", corner_lines,
    "axis_points:", axis_lines,
    if (length(style_lines)) {
      c("display_styles:", style_lines)
    } else {
      "display_styles: {}"
    }
  )
}

atomic_write_lines <- function(lines, path) {
  target_dir <- normalizePath(dirname(path), mustWork = TRUE)
  target_path <- file.path(target_dir, basename(path))
  temp_path <- tempfile(
    pattern = paste0(".", basename(path), "-"),
    tmpdir = target_dir,
    fileext = ".tmp"
  )
  on.exit(unlink(temp_path), add = TRUE)

  connection <- file(temp_path, open = "wb")
  tryCatch(
    writeLines(lines, connection, useBytes = TRUE),
    finally = close(connection)
  )
  if (!file.rename(temp_path, target_path)) {
    stop("임시 파일을 최종 CSV로 교체하지 못했습니다: ", target_path)
  }
  invisible(target_path)
}

box_point_display_labels <- c(
  origin = "원점", x_axis_end = "X 끝점",
  y_axis_end = "Y 끝점", xy_axis_end = "XY 끝점"
)

ui <- fluidPage(
  tags$head(
    tags$title("Tiny Plot Digitizer 1.0"),
    tags$style(HTML("
      body { background: #f7f7f5; color: #202124; }
      .container-fluid { padding: 12px 18px; }
      h3 { margin: 0 0 12px; font-size: 20px; font-weight: 600; }
      h4 { margin: 12px 0 8px; font-size: 14px; font-weight: 600; }
      .editor-layout { display: grid; grid-template-columns: 300px minmax(0, 2fr) minmax(280px, 1fr); margin: 0; }
      .editor-layout::before, .editor-layout::after { display: none; }
      .editor-layout > .editor-column { float: none; width: auto; }
      .editor-layout > .control-column { padding-left: 0; }
      .editor-layout > .detail-column { padding-right: 0; }
      .control-panel { border-right: 1px solid #d8d8d4; padding-right: 18px; min-height: calc(100vh - 48px); }
      .form-group { margin-bottom: 10px; }
      .project-source-group { margin-bottom: 9px; }
      .project-source-title { margin-bottom: 4px; font-weight: 600; }
      .project-source-control-row { display: grid; grid-template-columns: minmax(0, 1fr) 52px; gap: 6px; align-items: center; }
      .project-source-control-row > * { min-width: 0; }
      .selected-folder-box { display: flex; align-items: center; height: 34px; padding: 4px 8px; overflow: hidden; border: 1px solid #ccc; border-radius: 4px; background: #fff; font-size: 13px; }
      .selected-folder-line { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
      .project-source-control-row .btn { width: 100%; height: 34px; padding: 5px 4px; }
      .project-source-control-row .form-group { width: 100%; margin: 0; }
      .project-source-control-row select { width: 100%; height: 34px; padding: 4px 6px; }
      #folder-modal .sF-breadcrumps { display: none; }
      #folder-modal .folder-picker-breadcrumb { display: flex; align-items: center; gap: 5px; margin: 7px 0 2px; padding: 5px 8px; min-height: 30px; overflow-x: auto; white-space: nowrap; border: 1px solid #ccc; border-radius: 4px; background: #fff; font-size: 12px; }
      #folder-modal .folder-picker-separator { color: #777; }
      .move-button-row { display: grid; grid-template-columns: repeat(5, minmax(0, 1fr)); gap: 4px; margin: 6px 0 9px; }
      .calibration-move-button-row { grid-template-columns: repeat(5, minmax(0, 1fr)); }
      .move-button-row .btn { width: 100%; height: 36px; padding: 3px; font-size: 18px; border-radius: 4px; }
      .status-line { min-height: 22px; margin-top: 8px; font-size: 12px; }
      .status-line, .point-values, .status-line .shiny-text-output, .point-values .shiny-text-output { max-width: 100%; min-width: 0; overflow-wrap: anywhere; word-break: break-word; }
      .save-name-options .form-group { margin: 8px 0 5px; }
      .save-name-options .control-label { font-weight: 600; }
      .compact-control-row { display: grid; gap: 5px; align-items: center; margin-bottom: 9px; }
      .compact-control-row > *, .compact-control-row .shiny-input-container { min-width: 0; }
      .compact-control-row .shiny-input-container { width: 100% !important; margin: 0; }
      .compact-control-row > label { margin: 0; font-weight: 400; white-space: nowrap; }
      .compact-control-row input, .compact-control-row select { height: 34px; padding: 4px 6px; }
      .point-section-title { display: block; margin: 3px 0 5px; font-weight: 600; }
      .point-select-input .shiny-input-container { width: 100% !important; margin-bottom: 6px; }
      .point-action-row { display: grid; grid-template-columns: 2fr 1fr 1fr 1fr; gap: 6px; margin-bottom: 10px; }
      .point-action-row .btn { width: 100%; height: 34px; padding: 5px 2px; border-radius: 4px; font-size: 12px; }
      #add_point.add-mode-active { color: #fff; background: #24483e; border-color: #19352f; }
      #add_point.add-mode-active:hover { background: #19352f; border-color: #10241f; }
      .movement-focus-target { outline: none; }
      .group-section-title { margin: 3px 0 5px; font-weight: 600; }
      .movement-section-title { margin: 3px 0 5px; font-weight: 600; }
      .group-select-row { display: grid; grid-template-columns: 34px minmax(0, 1fr); gap: 5px; align-items: center; margin-bottom: 6px; }
      .group-select-row > *, .group-select-row .shiny-input-container { min-width: 0; }
      .group-select-input .shiny-input-container { width: 100% !important; margin: 0; }
      .panel-divider { margin: 3px 0 8px; border: 0; border-top: 1px solid #d8d8d4; }
      .group-action-row { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 6px; margin-bottom: 7px; }
      .group-action-row .btn { width: 100%; height: 34px; padding: 5px 2px; border-radius: 4px; font-size: 12px; }
      .symbol-swatch-frame { width: 34px; height: 34px; box-sizing: border-box; border: 1px solid #aaa; border-radius: 4px; overflow: hidden; background: white; }
      .symbol-swatch-frame .shiny-plot-output { width: 100% !important; height: 100% !important; }
      .calibration-axis-name-row { display: grid; grid-template-columns: auto minmax(0, 1fr) auto minmax(0, 1fr); gap: 5px; align-items: center; margin-bottom: 8px; }
      .calibration-axis-name-row > *, .calibration-axis-name-row .form-group { min-width: 0; }
      .calibration-axis-name-row .form-group { width: 100%; margin: 0; }
      .calibration-axis-name-row input { width: 100%; min-width: 0; height: 28px; padding: 3px 6px; }
      .calibration-setting-title { margin: 8px 0 5px; font-weight: 600; }
      .calibration-setting-table { margin-bottom: 9px; }
      .calibration-setting-row { display: grid; align-items: center; min-height: 31px; gap: 5px; }
      .box-setting-row { grid-template-columns: minmax(82px, 1fr) auto 75px auto 75px; }
      .axis-setting-row { grid-template-columns: minmax(40px, 1fr) auto 75px auto 75px; gap: 5px; }
      .calibration-setting-radio, .calibration-setting-radio label { margin: 0; font-weight: 400; }
      .calibration-setting-radio input { margin-top: 2px; }
      .calibration-coordinate { font-family: Menlo, Consolas, monospace; font-size: 11px; white-space: nowrap; }
      .box-setting-row .form-group { margin: 0; }
      .axis-setting-row .form-group { margin: 0; }
      .box-setting-row input[type='number'], .axis-setting-row input[type='number'] { height: 28px; padding: 3px 6px; }
      .move-point-option .form-group { display: grid; grid-template-columns: 1fr 120px; gap: 8px; align-items: center; margin-bottom: 7px; }
      .move-step-option .form-group { grid-template-columns: 1fr 190px; }
      .zoom-option .form-group { grid-template-columns: 1fr 90px; }
      .zoom-option .selectize-control { width: 90px !important; justify-self: end; }
      .move-point-option .control-label { margin: 0; font-size: 14px; font-weight: 400; }
      .zoom-option .control-label { font-weight: 600; }
      .zoom-divider { margin-top: 2px; }
      .move-point-option .form-control, .move-point-option .selectize-control { width: 100%; min-width: 0; }
      .move-step-option .shiny-options-group { text-align: right; white-space: nowrap; }
      .save-actions { width: 100%; margin: 0; }
      .save-actions .btn { display: block; width: 100%; }
      .save-actions .btn + .btn { margin-top: 6px; }
      .point-values { font-family: Menlo, Consolas, monospace; font-size: 12px; line-height: 1.55; white-space: pre-wrap; }
      .plot-title { margin: 0 0 4px; font-size: 13px; font-weight: 600; }
      .plot-stack { position: relative; height: calc(100vh - 82px); min-height: 420px; }
      .plot-stack .shiny-plot-output { position: absolute; inset: 0; width: 100% !important; height: 100% !important; }
      #overview_image { z-index: 1; pointer-events: none; }
      #overview { z-index: 2; background: transparent; }
      .btn-primary { background: #2f5d50; border-color: #2f5d50; }
      .btn-primary:hover { background: #24483e; border-color: #24483e; }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('update-point-label', function(message) {
        var element = document.getElementById('point');
        if (!element) return;
        var value = String(message.value);
        if (!element.selectize) {
          Array.from(element.options).forEach(function(option) {
            if (String(option.value) === value) option.textContent = message.label;
          });
          return;
        }
        var option = element.selectize.options[value];
        if (!option) return;
        option = Object.assign({}, option, {value: value, text: message.label});
        element.selectize.updateOption(value, option);
        element.selectize.refreshItems();
        element.selectize.$control.children('.item').each(function() {
          if (String(this.getAttribute('data-value')) === value) {
            this.textContent = message.label;
          }
        });
      });

      Shiny.addCustomMessageHandler('update-point-choices', function(message) {
        var element = document.getElementById('point');
        if (!element) return;
        element.replaceChildren();
        (message.choices || []).forEach(function(choice) {
          var option = document.createElement('option');
          option.value = String(choice.value);
          option.textContent = choice.label;
          element.appendChild(option);
        });
        if (message.selected) {
          element.value = String(message.selected);
          Shiny.setInputValue('point', String(message.selected), {priority: 'event'});
        } else {
          Shiny.setInputValue('point', null, {priority: 'event'});
        }
      });

      Shiny.addCustomMessageHandler('set-add-mode-state', function(message) {
        var button = document.getElementById('add_point');
        if (button) button.classList.toggle('add-mode-active', Boolean(message.active));
      });

      document.addEventListener('change', function(event) {
        if (event.target.matches('input[type=radio][name=calibration_point]')) {
          Shiny.setInputValue('calibration_point', event.target.value, {priority: 'event'});
        }
      });

      document.addEventListener('focusout', function(event) {
        if (/^axis_value_[xy][12]$/.test(event.target.id)) {
          Shiny.setInputValue('axis_value_commit', {
            point: event.target.id.replace('axis_value_', ''),
            value: event.target.value,
            nonce: Math.random()
          }, {priority: 'event'});
        }
        if (/^axis_pixel_[xy][12]$/.test(event.target.id)) {
          Shiny.setInputValue('axis_pixel_commit', {
            point: event.target.id.replace('axis_pixel_', ''),
            value: event.target.value,
            nonce: Math.random()
          }, {priority: 'event'});
        }
        if (/^box_(origin|x_axis_end|y_axis_end|xy_axis_end)_[xy]$/.test(event.target.id)) {
          Shiny.setInputValue('box_coordinate_commit', {
            id: event.target.id,
            value: event.target.value,
            nonce: Math.random()
          }, {priority: 'event'});
        }
      });

      document.addEventListener('keydown', function(event) {
        var commitOnEnter = /^axis_(value|pixel)_[xy][12]$/.test(event.target.id) ||
          /^box_(origin|x_axis_end|y_axis_end|xy_axis_end)_[xy]$/.test(event.target.id);
        if (commitOnEnter && event.key === 'Enter') {
          event.preventDefault();
          event.target.blur();
          return;
        }
        var activeElement = document.activeElement;
        var tag = activeElement && activeElement.tagName;
        var calibrationRadio = activeElement && activeElement.matches(
          'input[type=radio][name=calibration_point]'
        );
        var editingField = !calibrationRadio && (
          tag === 'INPUT' || tag === 'SELECT' || tag === 'TEXTAREA' ||
          (activeElement && activeElement.isContentEditable)
        );
        if (editingField) return;
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
          var movementButton = document.getElementById(directions[event.key]);
          if (movementButton) movementButton.click();
        }
      });

      function focusActiveMovementTarget() {
        window.setTimeout(function() {
          var movementControls = document.querySelector(
            '.editor-tabs .tab-pane.active .movement-focus-target'
          );
          if (movementControls) movementControls.focus({preventScroll: true});
        }, 0);
      }

      document.addEventListener('change', function(event) {
        if (!event.target.matches('input[type=radio], #point, #move_step, #zoom')) return;
        focusActiveMovementTarget();
      });

      document.addEventListener('click', function(event) {
        if (!event.target.matches(
          'input[type=radio][name=calibration_point]'
        )) return;
        focusActiveMovementTarget();
      });

      document.addEventListener('focusin', function(event) {
        var row = event.target.closest('.calibration-setting-row');
        if (!row || event.target.matches('input[type=radio]')) return;
        var radio = row.querySelector('input[type=radio]');
        if (!radio) return;
        radio.checked = true;
        Shiny.setInputValue(radio.name, radio.value, {priority: 'event'});
      });

      var folderPickerTarget = null;

      function renderFolderPickerPath() {
        var modal = document.getElementById('folder-modal');
        if (!modal || !folderPickerTarget) return;
        var modalBody = modal.querySelector('.modal-body');
        var modalControl = modal.querySelector('.sF-navigation');
        if (!modalBody || !modalControl) return;
        var breadcrumb = modal.querySelector('.folder-picker-breadcrumb');
        if (!breadcrumb) {
          breadcrumb = document.createElement('div');
          breadcrumb.className = 'folder-picker-breadcrumb';
          modalControl.insertAdjacentElement('afterend', breadcrumb);
        }
        var pathKey = JSON.stringify(folderPickerTarget);
        if (breadcrumb.dataset.pathKey === pathKey) return;
        breadcrumb.dataset.pathKey = pathKey;
        breadcrumb.replaceChildren();
        ['홈'].concat(folderPickerTarget.components || []).forEach(function(name, index, path) {
          var folder = document.createElement('span');
          folder.className = 'folder-picker-component';
          folder.textContent = name;
          breadcrumb.appendChild(folder);
          if (index < path.length - 1) {
            var separator = document.createElement('span');
            separator.className = 'folder-picker-separator';
            separator.textContent = '›';
            breadcrumb.appendChild(separator);
          }
        });
        breadcrumb.scrollLeft = breadcrumb.scrollWidth;
      }

      function initializeFolderPickerPath() {
        var modal = document.getElementById('folder-modal');
        if (!modal || !folderPickerTarget) return;
        renderFolderPickerPath();
        var root = modal.querySelector('.sF-dirList > .sF-directory');
        if (!root) return;
        var targetKey = JSON.stringify(folderPickerTarget);
        if (modal.dataset.folderPickerTarget === targetKey) return;

        var tree = {name: '', expanded: true, empty: false, children: []};
        var branch = tree;
        (folderPickerTarget.components || []).forEach(function(name) {
          var child = {name: name, expanded: true, empty: false, children: []};
          branch.children = [child];
          branch = child;
        });
        modal.dataset.folderPickerTarget = targetKey;
        Shiny.setInputValue('folder-modal', {
          tree: tree,
          selectedRoot: folderPickerTarget.root,
          contentPath: [''].concat(folderPickerTarget.components || []),
          nonce: Date.now()
        }, {priority: 'event'});
      }

      function revealFolderPickerTarget() {
        var modal = document.getElementById('folder-modal');
        if (!modal || !folderPickerTarget) return;
        var selected = modal.querySelector('.sF-dirList .sF-directory.selected');
        if (!selected) return;
        var targetKey = JSON.stringify(folderPickerTarget);
        if (modal.dataset.folderPickerVisible === targetKey) return;
        modal.dataset.folderPickerVisible = targetKey;
        selected.scrollIntoView({block: 'center'});
      }

      Shiny.addCustomMessageHandler('set-folder-picker-path', function(message) {
        folderPickerTarget = message;
        var modal = document.getElementById('folder-modal');
        if (modal) {
          delete modal.dataset.folderPickerTarget;
          delete modal.dataset.folderPickerVisible;
        }
        renderFolderPickerPath();
        window.setTimeout(initializeFolderPickerPath, 0);
      });

      var folderModalObserver = new MutationObserver(function() {
        renderFolderPickerPath();
        initializeFolderPickerPath();
        revealFolderPickerTarget();
      });
      folderModalObserver.observe(document.documentElement, {childList: true, subtree: true});
    "))
  ),
  h3("Tiny Plot Digitizer 1.0"),
  fluidRow(
    class = "editor-layout",
    column(
      width = 3,
      class = "editor-column control-column",
      div(
        class = "control-panel",
        div(
          class = "project-source-group",
          div(class = "project-source-title", "작업 폴더"),
          div(
            class = "project-source-control-row",
            div(
              class = "selected-folder-box",
              div(class = "selected-folder-line", textOutput("folder_path", inline = TRUE))
            ),
            shinyFiles::shinyDirButton(
              "folder", "선택", "작업 폴더를 선택하세요"
            )
          )
        ),
        div(
          class = "project-source-group",
          div(class = "project-source-title", "작업 파일"),
          div(
            class = "project-source-control-row",
            selectInput(
              "dataset", NULL, choices = setNames("", "CSV 파일 없음"),
              selected = "", selectize = FALSE, width = "100%"
            ),
            actionButton("new_project", "신규")
          )
        ),
        div(
          class = "editor-tabs",
          tabsetPanel(
            id = "edit_mode", selected = "point",
            tabPanel(
              title = "포인트", value = "point",
              div(
                div(class = "group-section-title", "그룹 정보"),
                div(
                  class = "group-select-row",
                  div(
                    class = "symbol-swatch-frame", title = "선택한 심볼 예시",
                    plotOutput("symbol_swatch", width = "100%", height = "100%")
                  ),
                  div(
                    class = "group-select-input",
                    selectInput("setting_series", NULL, choices = NULL, selectize = FALSE)
                  )
                )
              ),
              div(
                class = "group-action-row",
                actionButton("edit_series", "정보변경"),
                actionButton("add_series", "그룹추가"),
                actionButton("delete_series", "그룹제거")
              ),
              tags$label(`for` = "point", class = "point-section-title", "포인트 목록"),
              div(
                class = "point-select-input",
                selectInput("point", NULL, choices = NULL, selectize = FALSE)
              ),
              div(
                class = "point-action-row",
                actionButton("add_point", "포인트 연속추가",
                             title = "선택한 그룹에 포인트 연속 추가 시작"),
                actionButton("previous_point", "이전 [", title = "이전 포인트 ([)"),
                actionButton("next_point", "다음 ]", title = "다음 포인트 (])"),
                actionButton("delete_point", "제거", title = "선택한 포인트 제거")
              ),
              tags$hr(class = "panel-divider"),
              div(class = "movement-section-title", "포인트 이동"),
              div(
                id = "movement_controls",
                class = "move-button-row movement-focus-target",
                tabindex = "-1",
                actionButton("left", "←", title = "왼쪽으로 이동"),
                actionButton("down", "↓", title = "아래로 이동"),
                actionButton("up", "↑", title = "위로 이동"),
                actionButton("right", "→", title = "오른쪽으로 이동"),
                actionButton("undo", "↺", title = "선택한 대상의 직전 변경만 되돌리기")
              ),
              div(
                class = "move-point-option move-step-option",
                radioButtons(
                  "move_step", "이동 간격", choices = c(0.5, 1, 5, 10),
                  selected = 1, inline = TRUE, width = "100%"
                )
              )
            ),
            tabPanel(
              title = "좌표설정", value = "calibration",
              div(
                class = "calibration-point-group movement-focus-target",
                role = "radiogroup",
                tabindex = "-1",
                div(class = "calibration-setting-title", "축 명칭"),
                div(
                  class = "calibration-axis-name-row",
                  span("X"),
                  textInput("x_axis_name", NULL, value = ""),
                  span("Y"),
                  textInput("y_axis_name", NULL, value = "")
                ),
                div(class = "calibration-setting-title", "박스 설정"),
                div(
                  id = "box_point",
                  class = "calibration-setting-table",
                  div(
                    class = "shiny-options-group",
                    lapply(
                      c("origin", "x_axis_end", "y_axis_end", "xy_axis_end"),
                      function(point_name) {
                        label <- box_point_display_labels[[point_name]]
                        div(
                          class = "calibration-setting-row box-setting-row",
                          div(
                            class = "radio calibration-setting-radio",
                            tags$label(
                              tags$input(
                                type = "radio", name = "calibration_point",
                                value = paste("box", point_name, sep = ":")
                              ),
                              tags$span(label)
                            )
                          ),
                          span(class = "calibration-coordinate", "x"),
                          numericInput(paste0("box_", point_name, "_x"), NULL, value = 0, width = "100%"),
                          span(class = "calibration-coordinate", "y"),
                          numericInput(paste0("box_", point_name, "_y"), NULL, value = 0, width = "100%")
                        )
                      }
                    )
                  )
                ),
              div(class = "calibration-setting-title", "축 설정"),
              div(
                id = "axis_point",
                class = "calibration-setting-table",
                div(
                  class = "shiny-options-group",
                  lapply(c("x1", "x2", "y1", "y2"), function(point_name) {
                    axis_letter <- substr(point_name, 1, 1)
                    div(
                      class = "calibration-setting-row axis-setting-row",
                      div(
                        class = "radio calibration-setting-radio",
                        tags$label(
                          tags$input(
                            type = "radio", name = "calibration_point",
                            value = paste("axis", point_name, sep = ":")
                          ),
                          tags$span(toupper(point_name))
                        )
                      ),
                      span(class = "calibration-coordinate", "값"),
                      numericInput(paste0("axis_value_", point_name), NULL, value = 0, width = "100%"),
                      span(class = "calibration-coordinate", axis_letter),
                      numericInput(paste0("axis_pixel_", point_name), NULL, value = 0, width = "100%")
                    )
                  })
                )
              )
              ),
              tags$hr(class = "panel-divider"),
              div(class = "movement-section-title", "포인트 이동"),
              div(
                id = "calibration_movement_controls",
                class = "move-button-row calibration-move-button-row",
                actionButton("calibration_left", "←", title = "왼쪽으로 이동"),
                actionButton("calibration_down", "↓", title = "아래로 이동"),
                actionButton("calibration_up", "↑", title = "위로 이동"),
                actionButton("calibration_right", "→", title = "오른쪽으로 이동"),
                actionButton(
                  "calibration_undo", "↺",
                  title = "선택한 설정 포인트의 직전 변경만 되돌리기"
                )
              ),
              div(
                class = "move-point-option move-step-option",
                radioButtons(
                  "calibration_move_step", "이동 간격", choices = c(0.5, 1, 5, 10),
                  selected = 1, inline = TRUE, width = "100%"
                )
              )
            )
          )
        ),
        tags$hr(class = "panel-divider zoom-divider"),
        div(
          class = "move-point-option zoom-option",
          selectInput(
            "zoom", "확대 반경",
            choices = c(20, 40, 80), selected = 40, width = "100%"
          )
        ),
        div(class = "point-values", textOutput("point_values")),
        div(
          class = "save-name-options",
          radioButtons(
            "save_name_mode", "저장 파일명",
            choices = c("동일파일명.csv" = "same", "*-digitized.csv" = "digitized"),
            selected = "same", inline = TRUE, width = "100%"
          )
        ),
        div(
          class = "save-actions",
          actionButton("save", "변경 저장", class = "btn-primary"),
          actionButton("reload", "변경 취소")
        ),
        div(class = "status-line", textOutput("status"))
      )
    ),
    column(
      width = 6,
      class = "editor-column source-column",
      div(class = "plot-title", "원본 이미지"),
      div(
        class = "plot-stack",
        plotOutput("overview_image", height = "100%"),
        plotOutput("overview", height = "100%", click = "overview_click")
      )
    ),
    column(
      width = 3,
      class = "editor-column detail-column",
      div(class = "plot-title", textOutput("detail_title")),
      plotOutput("zoom_plot", height = "430px")
    )
  )
)

server <- function(input, output, session) {
  initial_folder <- normalizePath(
    file.path(data_raw_dir, "2004_MRP79R1"), mustWork = TRUE
  )
  home_dir <- normalizePath("~", mustWork = TRUE)
  selected_folder <- reactiveVal(initial_folder)
  folder_roots <- c("홈" = home_dir)
  catalog <- reactiveVal(list())
  rv <- reactiveValues(
    data = NULL, image_width = NULL, image_height = NULL,
    raster_matrix = NULL, dataset = NULL,
    calibration = NULL,
    point_baseline_data = NULL,
    calibration_baseline = NULL, series = NULL, series_baseline = NULL,
    pending_cancel_mode = NULL, pending_series_edit = NULL,
    point_dirty = FALSE, calibration_dirty = FALSE, point_undo = NULL,
    calibration_undo = NULL,
    add_mode = FALSE, add_series = NULL,
    selected = NULL, status = "", pending_switch_status = NULL,
    updating_calibration_inputs = FALSE,
    calibration_target = NULL, calibration_point = NULL,
    active_edit_mode = "point", pending_edit_mode = NULL,
    updating_edit_mode = FALSE
  )

  mode_changes_pending <- function(mode) {
    if (identical(mode, "point")) return(isTRUE(rv$point_dirty))
    if (identical(mode, "calibration")) return(isTRUE(rv$calibration_dirty))
    stop("알 수 없는 편집 모드입니다: ", mode)
  }

  unsaved_changes_pending <- function() {
    mode_changes_pending("point") || mode_changes_pending("calibration")
  }

  unsaved_status <- function() {
    if (unsaved_changes_pending()) "저장되지 않은 변경" else ""
  }

  mark_mode_changed <- function(mode, status = "저장되지 않은 변경") {
    if (identical(mode, "point")) {
      rv$point_dirty <- TRUE
    } else if (identical(mode, "calibration")) {
      rv$calibration_dirty <- TRUE
    } else {
      stop("알 수 없는 편집 모드입니다: ", mode)
    }
    rv$status <- status
    invisible()
  }

  capture_mode_baseline <- function(mode) {
    if (identical(mode, "point")) {
      rv$point_baseline_data <- rv$data
      rv$series_baseline <- rv$series
      rv$point_dirty <- FALSE
      rv$point_undo <- NULL
    } else if (identical(mode, "calibration")) {
      rv$calibration_baseline <- rv$calibration
      rv$calibration_dirty <- FALSE
      rv$calibration_undo <- NULL
    } else {
      stop("알 수 없는 편집 모드입니다: ", mode)
    }
    invisible()
  }

  capture_all_baselines <- function() {
    capture_mode_baseline("point")
    capture_mode_baseline("calibration")
    invisible()
  }

  project_choices <- function(projects) {
    if (!length(projects)) return(setNames("", "CSV 파일 없음"))
    labels <- vapply(projects, `[[`, character(1), "label")
    setNames(names(projects), labels)
  }

  update_project_input <- function(projects, selected = NULL, freeze = FALSE) {
    choices <- project_choices(projects)
    if (is.null(selected)) selected <- unname(choices[1])
    if (freeze) freezeReactiveValue(input, "dataset")
    updateSelectInput(session, "dataset", choices = choices, selected = selected)
    invisible()
  }

  save_path_for_dataset <- function(dataset) {
    if (is.null(dataset) || is.null(dataset$source_path)) return(NULL)
    if (!is.null(dataset$load_path)) return(dataset$load_path)
    mode <- if (length(input$save_name_mode)) input$save_name_mode else "same"
    source_stem <- tools::file_path_sans_ext(dataset$source_path)
    if (identical(mode, "same")) return(paste0(source_stem, ".csv"))
    paste0(source_stem, "-digitized.csv")
  }

  relative_repo_path <- function(path) {
    normalized <- normalizePath(path, mustWork = FALSE)
    prefix <- paste0(normalizePath(repo_dir), .Platform$file.sep)
    if (startsWith(normalized, prefix)) {
      return(substring(normalized, nchar(prefix) + 1L))
    }
    normalized
  }

  update_save_name_inputs <- function(dataset) {
    project_path <- dataset$load_path
    mode <- "same"
    if (!is.null(project_path)) {
      source_stem <- tools::file_path_sans_ext(basename(dataset$source_path))
      project_stem <- tools::file_path_sans_ext(basename(project_path))
      loaded_postfix <- substring(project_stem, nchar(source_stem) + 1L)
      if (nzchar(loaded_postfix)) mode <- "digitized"
    }
    freezeReactiveValue(input, "save_name_mode")
    updateRadioButtons(session, "save_name_mode", selected = mode)
    invisible()
  }

  save_changes <- function(auto = FALSE) {
    if (is.null(rv$data) || is.null(rv$dataset) || !unsaved_changes_pending()) return(NULL)
    save_path <- tryCatch(
      save_path_for_dataset(rv$dataset),
      error = function(error) {
        rv$status <- conditionMessage(error)
        NULL
      }
    )
    if (is.null(save_path)) return(NULL)
    if (is.null(rv$dataset$load_path) && file.exists(save_path)) {
      rv$status <- paste0("같은 이름의 CSV가 이미 있습니다: ", basename(save_path))
      return(NULL)
    }
    finalize_point_order()
    saved_series <- series_for_save(rv$series, rv$data)
    yaml_lines <- serialize_project_metadata(
      rv$dataset$source_path, rv$image_width, rv$image_height,
      rv$calibration, saved_series
    )

    body <- rv$data
    group_rows <- match(body$series_id, saved_series$id)
    if (anyNA(group_rows)) stop("포인트의 그룹 정보를 찾을 수 없습니다")
    body$group <- saved_series$name[group_rows]
    values <- axis_values(body, rv$calibration)
    body[[rv$calibration$x$column]] <- values$x
    body[[rv$calibration$y$column]] <- values$y
    body <- body[c(
      "group", "pixel_x", "pixel_y",
      rv$calibration$x$column, rv$calibration$y$column
    )]
    csv_lines <- capture.output(
      utils::write.csv(body, row.names = FALSE, na = "")
    )
    saved <- tryCatch(
      {
        atomic_write_lines(
          c("# ---", paste0("# ", yaml_lines), "# ---", csv_lines),
          save_path
        )
        TRUE
      },
      error = function(error) {
        rv$pending_switch_status <- NULL
        rv$status <- paste0("저장 실패: ", conditionMessage(error))
        FALSE
      }
    )
    if (!saved) return(NULL)
    saved_path <- normalizePath(save_path, mustWork = TRUE)
    previous_key <- rv$dataset$key
    rv$dataset$key <- saved_path
    rv$dataset$load_path <- saved_path
    rv$dataset$label <- basename(saved_path)
    current_catalog <- catalog()
    if (!is.null(previous_key) && previous_key %in% names(current_catalog)) {
      current_catalog[[previous_key]] <- NULL
    }
    current_catalog[[saved_path]] <- rv$dataset
    catalog(current_catalog)
    if (!auto) {
      update_project_input(current_catalog, selected = saved_path, freeze = TRUE)
    }
    capture_all_baselines()

    prefix <- if (auto) "자동 저장됨:" else "저장됨:"
    message <- paste(prefix, relative_repo_path(save_path))
    if (auto) {
      rv$pending_switch_status <- message
    } else {
      rv$status <- message
    }
    message
  }

  save_before_navigation <- function() {
    if (!unsaved_changes_pending()) return(TRUE)
    !is.null(save_changes(auto = TRUE))
  }

  clear_dataset <- function() {
    rv$data <- NULL
    rv$image_width <- NULL
    rv$image_height <- NULL
    rv$raster_matrix <- NULL
    rv$dataset <- NULL
    rv$calibration <- NULL
    rv$point_baseline_data <- NULL
    rv$calibration_baseline <- NULL
    rv$series <- NULL
    rv$series_baseline <- NULL
    rv$pending_cancel_mode <- NULL
    rv$pending_series_edit <- NULL
    rv$pending_edit_mode <- NULL
    rv$point_dirty <- FALSE
    rv$calibration_dirty <- FALSE
    rv$point_undo <- NULL
    rv$calibration_undo <- NULL
    rv$add_mode <- FALSE
    rv$add_series <- NULL
    rv$selected <- NULL
    rv$status <- ""
    updateSelectInput(session, "setting_series", choices = character(), selected = character())
    session$sendCustomMessage(
      "update-point-choices", list(choices = list(), selected = NULL)
    )
  }

  update_catalog <- function(folder_path, selected = NULL) {
    clear_dataset()
    projects <- discover_projects(folder_path)
    catalog(projects)
    if (!length(projects)) {
      selected <- ""
      if (!is.null(rv$pending_switch_status)) {
        rv$status <- rv$pending_switch_status
        rv$pending_switch_status <- NULL
      }
    }
    update_project_input(projects, selected = selected)
  }

  folder_picker_target <- function(path) {
    path <- normalizePath(path, mustWork = TRUE)
    home_prefix <- paste0(home_dir, .Platform$file.sep)
    if (identical(path, home_dir)) {
      relative_path <- ""
    } else if (startsWith(path, home_prefix)) {
      relative_path <- substring(path, nchar(home_prefix) + 1L)
    } else {
      stop("홈 폴더 밖의 경로는 선택할 수 없습니다: ", path)
    }
    components <- if (nzchar(relative_path)) {
      strsplit(relative_path, .Platform$file.sep, fixed = TRUE)[[1]]
    } else {
      character()
    }
    list(root = "홈", components = unname(components))
  }

  update_folder_picker_target <- function(path) {
    session$sendCustomMessage(
      "set-folder-picker-path", folder_picker_target(path)
    )
  }

  shinyFiles::shinyDirChoose(
    input, "folder", session = session, roots = folder_roots,
    defaultRoot = "홈", defaultPath = "",
    allowDirCreate = FALSE
  )

  output$folder_path <- renderText({
    basename(selected_folder())
  })

  observeEvent(TRUE, {
    update_catalog(initial_folder)
    update_folder_picker_target(initial_folder)
  }, once = TRUE)

  observeEvent(input$folder, {
    folder_path <- shinyFiles::parseDirPath(folder_roots, input$folder)
    if (!length(folder_path) || !dir.exists(folder_path)) return()
    folder_path <- normalizePath(folder_path, mustWork = TRUE)
    if (identical(folder_path, selected_folder())) {
      update_folder_picker_target(folder_path)
      return()
    }
    if (!save_before_navigation()) {
      update_folder_picker_target(selected_folder())
      return()
    }
    selected_folder(folder_path)
    update_folder_picker_target(folder_path)
    update_catalog(folder_path)
  }, ignoreInit = TRUE)

  observeEvent(input$new_project, {
    images <- discover_images(selected_folder())
    if (!length(images)) {
      showModal(modalDialog(
        title = "신규 CSV 파일 제작",
        "현재 작업 폴더에 PNG 파일이 없습니다.",
        footer = modalButton("닫기"),
        easyClose = TRUE
      ))
      return()
    }
    labels <- vapply(images, `[[`, character(1), "label")
    showModal(modalDialog(
      title = "신규 CSV 파일 제작",
      selectInput(
        "new_project_image", "그림 파일 선택",
        choices = setNames(names(images), labels), selectize = FALSE,
        width = "100%"
      ),
      footer = tagList(
        modalButton("취소"),
        actionButton("confirm_new_project", "선택", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
  })

  observeEvent(input$confirm_new_project, {
    req(input$new_project_image)
    source_path <- normalizePath(input$new_project_image, mustWork = TRUE)
    if (!save_before_navigation()) return()
    removeModal()

    key <- paste0("new::", source_path)
    dataset <- list(
      key = key,
      source_path = source_path,
      load_path = NULL,
      label = paste0("[신규] ", basename(source_path))
    )
    projects <- catalog()
    if (!is.null(rv$dataset) && is.null(rv$dataset$load_path) &&
        rv$dataset$key %in% names(projects)) {
      projects[[rv$dataset$key]] <- NULL
    }
    projects[[key]] <- dataset
    catalog(projects)
    update_project_input(projects, selected = key, freeze = TRUE)
    load_dataset(key)
  })

  series_choices <- function() {
    if (is.null(rv$series) || !nrow(rv$series)) return(character())
    labels <- rv$series$name
    setNames(as.character(rv$series$id), labels)
  }

  series_row <- function(id) {
    if (is.null(rv$series) || !length(id)) return(NA_integer_)
    match(as.integer(id), rv$series$id)
  }

  refresh_series_choices <- function(setting_selected = NULL) {
    choices <- series_choices()
    ids <- unname(choices)
    if (is.null(setting_selected) && length(input$setting_series)) {
      setting_selected <- input$setting_series
    }
    if (!length(ids)) {
      setting_selected <- character()
    } else {
      if (is.null(setting_selected) || !as.character(setting_selected) %in% ids) {
        setting_selected <- ids[1]
      }
    }
    freezeReactiveValue(input, "setting_series")
    updateSelectInput(
      session, "setting_series", choices = choices, selected = setting_selected
    )
    invisible()
  }

  point_label <- function(row, values = axis_values(rv$data, rv$calibration)) {
    style_row <- match(rv$data$series_id[row], rv$series$id)
    if (is.na(style_row)) {
      series_label <- paste("그룹", rv$data$series_id[row])
    } else {
      series_label <- rv$series$name[style_row]
    }
    group_rows <- which(rv$data$series_id == rv$data$series_id[row])
    group_number <- match(row, group_rows)
    x_value <- format(round(values$x[row], 3), scientific = FALSE, trim = TRUE)
    y_value <- format(round(values$y[row], 3), scientific = FALSE, trim = TRUE)
    sprintf(
      "%d-%d %s X: %s Y: %s",
      rv$data$point_id[row], group_number, series_label,
      x_value, y_value
    )
  }

  point_choices <- function() {
    if (is.null(rv$data) || !nrow(rv$data)) return(character())
    values <- axis_values(rv$data, rv$calibration)
    labels <- vapply(
      seq_len(nrow(rv$data)),
      function(row) point_label(row, values),
      character(1)
    )
    setNames(as.character(rv$data$point_id), labels)
  }

  update_point_choices <- function() {
    choices <- point_choices()
    selected_id <- if (!is.null(rv$selected) && nrow(rv$data)) {
      as.character(rv$data$point_id[rv$selected])
    } else {
      character()
    }
    choice_items <- lapply(seq_along(choices), function(index) {
      list(value = unname(choices[index]), label = names(choices)[index])
    })
    session$sendCustomMessage(
      "update-point-choices",
      list(
        choices = choice_items,
        selected = if (length(selected_id)) selected_id else NULL
      )
    )
    invisible()
  }

  refresh_controls <- function(selected = NULL) {
    req(rv$data, rv$dataset)
    if (!nrow(rv$data)) {
      rv$selected <- NULL
    } else if (is.null(selected)) {
      if (is.null(rv$selected)) rv$selected <- 1L
    } else {
      rv$selected <- max(1L, min(as.integer(selected), nrow(rv$data)))
    }
    setting_series <- if (length(input$setting_series)) input$setting_series else NULL
    refresh_series_choices(setting_series)
    update_point_choices()
    invisible()
  }

  refresh_point_choices <- function() update_point_choices()
  update_point_label <- function(row) {
    if (is.null(rv$data) || !nrow(rv$data) || row < 1L || row > nrow(rv$data)) {
      return(invisible())
    }
    session$sendCustomMessage(
      "update-point-label",
      list(value = rv$data$point_id[row], label = point_label(row))
    )
    invisible()
  }
  update_calibration_inputs <- function() {
    calibration <- rv$calibration
    if (is.null(calibration) || calibration$type != "projective") return(invisible())
    axis_points <- c("x1", "x2", "y1", "y2")
    box_points <- c("origin", "x_axis_end", "y_axis_end", "xy_axis_end")
    input_ids <- c(
      paste0("axis_pixel_", axis_points), paste0("axis_value_", axis_points),
      paste0("box_", box_points, "_x"), paste0("box_", box_points, "_y"),
      "x_axis_name", "y_axis_name"
    )

    rv$updating_calibration_inputs <- TRUE
    for (input_id in input_ids) {
      freezeReactiveValue(input, input_id)
    }
    for (axis_point in axis_points) {
      point <- calibration$axis_points[[axis_point]]
      pixel_value <- if (startsWith(axis_point, "x")) point$pixel_x else point$pixel_y
      updateNumericInput(
        session, paste0("axis_pixel_", axis_point),
        value = round_pixel_coordinate(pixel_value), step = 0.5
      )
      updateNumericInput(
        session, paste0("axis_value_", axis_point),
        value = point$value
      )
    }
    for (box_point in box_points) {
      point <- calibration$box[[box_point]]
      updateNumericInput(
        session, paste0("box_", box_point, "_x"),
        value = round_pixel_coordinate(point$pixel_x),
        min = 0, max = rv$image_width, step = 0.5
      )
      updateNumericInput(
        session, paste0("box_", box_point, "_y"),
        value = round_pixel_coordinate(point$pixel_y),
        min = 0, max = rv$image_height, step = 0.5
      )
    }
    updateTextInput(session, "x_axis_name", value = calibration$x$column)
    updateTextInput(session, "y_axis_name", value = calibration$y$column)
    session$onFlushed(function() {
      rv$updating_calibration_inputs <- FALSE
    }, once = TRUE)
    invisible()
  }

  set_add_mode <- function(active, series = NULL) {
    rv$add_mode <- active
    rv$add_series <- if (active) as.integer(series) else NULL
    updateActionButton(
      session, "add_point",
      label = if (active) "연속추가 종료" else "포인트 연속추가",
      icon = NULL
    )
    session$sendCustomMessage("set-add-mode-state", list(active = isTRUE(active)))
  }

  remember_point_change <- function(row) {
    rv$point_undo <- list(
      data = rv$data,
      point_id = rv$data$point_id[row],
      point_dirty = rv$point_dirty
    )
  }

  calibration_target_key <- function() {
    if (is.null(rv$calibration_target) || is.null(rv$calibration_point)) return(NULL)
    paste(rv$calibration_target, rv$calibration_point, sep = ":")
  }

  remember_calibration_change <- function() {
    rv$calibration_undo <- list(
      target = calibration_target_key(),
      calibration = rv$calibration,
      calibration_dirty = rv$calibration_dirty
    )
  }

  selected_point_id <- function() {
    if (is.null(rv$data) || !nrow(rv$data) || is.null(rv$selected) ||
        rv$selected < 1L || rv$selected > nrow(rv$data)) return(NULL)
    as.integer(rv$data$point_id[rv$selected])
  }

  sort_points <- function(selected_point_id = NULL) {
    if (!nrow(rv$data)) {
      rv$selected <- NULL
      return(invisible())
    }
    values <- axis_values(rv$data, rv$calibration)
    series_order <- match(rv$data$series_id, rv$series$id)
    order_index <- order(series_order, values$x, rv$data$point_id, na.last = TRUE)
    rv$data <- rv$data[order_index, , drop = FALSE]
    rownames(rv$data) <- NULL
    if (!is.null(selected_point_id)) {
      rv$selected <- match(as.integer(selected_point_id), rv$data$point_id)
    }
    invisible()
  }

  finalize_point_order <- function(point_id = selected_point_id()) {
    sort_points(point_id)
    update_point_choices()
    invisible()
  }

  edit_mode_label <- function(mode) {
    if (identical(mode, "point")) return("포인트")
    if (identical(mode, "calibration")) return("박스")
    stop("알 수 없는 편집 모드입니다: ", mode)
  }

  active_mode_is <- function(mode) {
    identical(rv$active_edit_mode, mode)
  }

  discard_edit_mode_changes <- function(mode) {
    if (identical(mode, "calibration")) {
      rv$calibration <- rv$calibration_baseline
      capture_mode_baseline("calibration")
      update_calibration_inputs()
    } else if (identical(mode, "point")) {
      selected <- if (is.null(rv$selected)) 1L else rv$selected
      rv$data <- rv$point_baseline_data
      rv$series <- rv$series_baseline
      capture_mode_baseline("point")
      set_add_mode(FALSE)
      refresh_controls(if (nrow(rv$data)) min(selected, nrow(rv$data)) else NULL)
    } else {
      stop("알 수 없는 편집 모드입니다: ", mode)
    }
    invisible()
  }

  update_edit_mode_controls <- function(mode) {
    updateActionButton(session, "reload", label = "변경 취소")
    if (identical(mode, "calibration")) set_add_mode(FALSE)
    invisible()
  }

  update_edit_mode_input <- function(mode) {
    rv$updating_edit_mode <- TRUE
    freezeReactiveValue(input, "edit_mode")
    updateTabsetPanel(session, "edit_mode", selected = mode)
    invisible()
  }

  activate_edit_mode <- function(mode, update_input = FALSE) {
    rv$active_edit_mode <- mode
    capture_mode_baseline(mode)
    update_edit_mode_controls(mode)
    if (update_input) update_edit_mode_input(mode)
    invisible()
  }

  load_dataset <- function(key) {
    req(key, key %in% names(catalog()))
    dataset <- catalog()[[key]]
    source_image <- png::readPNG(dataset$source_path)
    image_height <- dim(source_image)[1]
    image_width <- dim(source_image)[2]

    calibration <- new_project_calibration(image_width, image_height)
    series <- default_groups()
    data <- empty_points()
    loaded_project <- FALSE
    project_path <- dataset$load_path

    if (!is.null(project_path) && file.exists(project_path)) {
      metadata <- read_csv_metadata(project_path)
      source_metadata <- source_image_metadata(metadata)
      if (is.null(source_metadata) ||
          !identical(source_metadata$filename, basename(dataset$source_path))) {
        stop("원본 이미지 정보를 읽을 수 없습니다: ", project_path)
      }
      if (source_metadata$width != image_width || source_metadata$height != image_height) {
        stop(sprintf(
          "원본 이미지 크기가 프로젝트와 다릅니다: 저장 %dx%d, 현재 %dx%d",
          source_metadata$width, source_metadata$height, image_width, image_height
        ))
      }
      saved_data <- read.csv(project_path, comment.char = "#", check.names = FALSE)
      required <- c("group", "pixel_x", "pixel_y")
      if (!all(required %in% names(saved_data))) {
        stop("독립 프로젝트 형식이 아닌 CSV입니다: ", project_path)
      }
      saved_calibration <- parse_projective_calibration(metadata, names(saved_data))
      if (is.null(saved_calibration)) {
        stop("축 설정을 읽을 수 없습니다: ", project_path)
      }
      persisted_series <- series_from_metadata(metadata$display_styles)
      data <- saved_data[required]
      group_rows <- match(as.character(data$group), persisted_series$name)
      if (anyNA(group_rows)) {
        stop("CSV의 그룹 명칭을 display_styles 메타데이터에서 찾을 수 없습니다")
      }
      data$group <- persisted_series$id[group_rows]
      names(data)[names(data) == "group"] <- "series_id"
      data$point_id <- if ("point_id" %in% names(saved_data)) {
        as.integer(saved_data$point_id)
      } else {
        seq_len(nrow(data))
      }
      data <- data[c("point_id", "series_id", "pixel_x", "pixel_y")]
      data$series_id <- as.integer(data$series_id)
      data$pixel_x <- as.numeric(data$pixel_x)
      data$pixel_y <- as.numeric(data$pixel_y)
      if (anyDuplicated(data$point_id)) stop("point_id가 중복되어 있습니다")
      if (nrow(data) && any(!data$series_id %in% persisted_series$id)) {
        stop("display_styles 메타데이터에 없는 그룹을 사용하는 포인트가 있습니다")
      }
      saved_series <- restore_default_groups(persisted_series)
      calibration <- saved_calibration
      series <- saved_series
      loaded_project <- TRUE
    }

    rv$data <- data
    rv$image_width <- image_width
    rv$image_height <- image_height
    rv$raster_matrix <- as.matrix(as.raster(source_image))
    rv$dataset <- dataset
    rv$calibration <- calibration
    rv$series <- series
    sort_points()
    capture_all_baselines()
    rv$pending_cancel_mode <- NULL
    rv$pending_edit_mode <- NULL
    rv$calibration_target <- NULL
    rv$calibration_point <- NULL
    set_add_mode(FALSE)
    update_save_name_inputs(dataset)
    rv$status <- if (is.null(rv$pending_switch_status)) {
      if (loaded_project) {
        paste("프로젝트 불러옴:", basename(project_path))
      } else {
        "새 디지타이징 프로젝트"
      }
    } else {
      rv$pending_switch_status
    }
    rv$pending_switch_status <- NULL
    refresh_controls(if (nrow(rv$data)) 1L else NULL)
    update_calibration_inputs()
  }

  observeEvent(input$dataset, {
    requested_key <- input$dataset
    req(nzchar(requested_key), requested_key %in% names(catalog()))
    if (!save_before_navigation()) {
      if (!is.null(rv$dataset)) {
        freezeReactiveValue(input, "dataset")
        updateSelectInput(
          session, "dataset", selected = rv$dataset$key
        )
      }
      return()
    }
    projects <- catalog()
    if (!is.null(rv$dataset) && is.null(rv$dataset$load_path) &&
        !identical(rv$dataset$key, requested_key) &&
        rv$dataset$key %in% names(projects)) {
      projects[[rv$dataset$key]] <- NULL
      catalog(projects)
      update_project_input(projects, selected = requested_key, freeze = TRUE)
    }
    load_dataset(requested_key)
  })

  observeEvent(input$edit_mode, {
    req(rv$calibration)
    requested_mode <- input$edit_mode
    if (isTRUE(rv$updating_edit_mode)) {
      rv$updating_edit_mode <- FALSE
      return()
    }

    current_mode <- rv$active_edit_mode
    if (identical(requested_mode, current_mode)) return()

    if (!mode_changes_pending(current_mode)) {
      activate_edit_mode(requested_mode)
      rv$status <- ""
      return()
    }

    rv$pending_edit_mode <- requested_mode
    update_edit_mode_input(current_mode)
    current_label <- edit_mode_label(current_mode)
    showModal(modalDialog(
      title = paste0(current_label, " 변경사항"),
      paste0(current_label, " 모드의 변경사항을 저장하시겠습니까?"),
      footer = tagList(
        actionButton("cancel_mode_switch", "전환 취소"),
        actionButton("discard_mode_switch", "저장하지 않음"),
        actionButton("save_mode_switch", "저장", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_mode_switch, {
    removeModal()
    rv$pending_edit_mode <- NULL
  })

  observeEvent(input$discard_mode_switch, {
    req(rv$pending_edit_mode)
    current_mode <- rv$active_edit_mode
    next_mode <- rv$pending_edit_mode
    discard_edit_mode_changes(current_mode)
    removeModal()
    rv$pending_edit_mode <- NULL
    activate_edit_mode(next_mode, update_input = TRUE)
    rv$status <- paste0(
      edit_mode_label(current_mode), " 변경을 취소하고 ",
      edit_mode_label(next_mode), " 모드로 전환했습니다"
    )
  })

  observeEvent(input$save_mode_switch, {
    req(rv$pending_edit_mode)
    current_mode <- rv$active_edit_mode
    next_mode <- rv$pending_edit_mode
    req(!is.null(save_changes(auto = FALSE)))
    removeModal()
    rv$pending_edit_mode <- NULL
    activate_edit_mode(next_mode, update_input = TRUE)
    rv$status <- paste0(
      edit_mode_label(current_mode), " 변경을 저장하고 ",
      edit_mode_label(next_mode), " 모드로 전환했습니다"
    )
  })

  observeEvent(input$setting_series, {
    if (rv$add_mode && length(input$setting_series)) {
      rv$add_series <- as.integer(input$setting_series)
    }
  }, ignoreInit = TRUE)

  show_series_edit_modal <- function(title, name, style) {
    showModal(modalDialog(
      title = title,
      textInput("new_series_name", "이름", value = name),
      fluidRow(
        column(
          6,
          selectInput(
            "new_series_color", "색상", choices = group_color_choices,
            selected = style$color, selectize = FALSE, width = "100%"
          )
        ),
        column(
          6,
          selectInput(
            "new_series_marker", "형태", choices = series_marker_choices,
            selected = style$marker, selectize = FALSE, width = "100%"
          )
        )
      ),
      fluidRow(
        column(
          6,
          numericInput(
            "new_series_size", "크기", value = style$size,
            min = 0.2, max = 5, step = 0.1, width = "100%"
          )
        ),
        column(
          6,
          numericInput(
            "new_series_alpha", "불투명도", value = style$alpha,
            min = 0, max = 1, step = 0.1, width = "100%"
          )
        )
      ),
      size = "s",
      easyClose = TRUE,
      footer = tagList(
        modalButton("취소"),
        actionButton("confirm_series_edit", "적용", class = "btn-primary")
      )
    ))
    invisible()
  }

  observeEvent(input$edit_series, {
    id <- suppressWarnings(as.integer(input$setting_series))
    row <- series_row(id)
    if (is.na(row)) {
      rv$status <- "정보를 변경할 그룹을 선택하세요"
      return()
    }
    rv$pending_series_edit <- list(action = "edit", id = id)
    show_series_edit_modal(
      "그룹 정보 변경", rv$series$name[row],
      as.list(rv$series[row, c("marker", "color", "size", "alpha")])
    )
  }, ignoreInit = TRUE)

  observeEvent(input$add_series, {
    req(rv$series)
    id <- if (nrow(rv$series)) max(rv$series$id) + 1L else 1L
    name <- sprintf("group%02d", id)
    while (name %in% rv$series$name) {
      id <- id + 1L
      name <- sprintf("group%02d", id)
    }
    rv$pending_series_edit <- list(action = "add", id = id)
    show_series_edit_modal("그룹 추가", name, group_style_defaults(id))
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_series_edit, {
    pending <- rv$pending_series_edit
    if (is.null(pending) || !pending$action %in% c("edit", "add")) {
      removeModal()
      rv$pending_series_edit <- NULL
      return()
    }
    id <- as.integer(pending$id)
    row <- if (identical(pending$action, "edit")) series_row(id) else NA_integer_
    if (identical(pending$action, "edit") && is.na(row)) {
      removeModal()
      rv$pending_series_edit <- NULL
      return()
    }
    name <- trimws(input$new_series_name)
    marker <- as.character(input$new_series_marker)
    color <- tolower(as.character(input$new_series_color))
    size <- suppressWarnings(as.numeric(input$new_series_size))
    alpha <- suppressWarnings(as.numeric(input$new_series_alpha))
    if (length(name) != 1L || !nzchar(name)) {
      showNotification("그룹 이름을 입력하세요", type = "warning")
      return()
    }
    other_names <- if (identical(pending$action, "edit")) {
      rv$series$name[-row]
    } else {
      rv$series$name
    }
    if (name %in% other_names) {
      showNotification("그룹 이름은 서로 달라야 합니다", type = "warning")
      return()
    }
    if (length(marker) != 1L || !marker %in% unname(series_marker_choices) ||
        length(color) != 1L || !color %in% tolower(unname(group_color_choices)) ||
        length(size) != 1L || !is.finite(size) || size < 0.2 || size > 5 ||
        length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) {
      showNotification("그룹 설정값을 확인하세요", type = "warning")
      return()
    }
    if (identical(pending$action, "add")) {
      rv$series <- rbind(
        rv$series,
        data.frame(
          id = id, name = name, marker = marker, color = color,
          size = size, alpha = alpha, stringsAsFactors = FALSE
        )
      )
      mark_mode_changed("point", paste0(name, " 그룹이 추가되었습니다"))
      refresh_series_choices(as.character(id))
      removeModal()
      rv$pending_series_edit <- NULL
      return()
    }
    series <- rv$series
    name_changed <- !identical(series$name[row], name)
    changed <- name_changed || !identical(series$marker[row], marker) ||
      !identical(tolower(series$color[row]), color) ||
      !isTRUE(all.equal(series$size[row], size)) ||
      !isTRUE(all.equal(series$alpha[row], alpha))
    if (changed) {
      series$name[row] <- name
      series$marker[row] <- marker
      series$color[row] <- color
      series$size[row] <- size
      series$alpha[row] <- alpha
      rv$series <- series
      mark_mode_changed("point", "그룹 정보가 변경되었습니다")
      if (name_changed) {
        refresh_series_choices(as.character(id))
        update_point_choices()
      }
    }
    removeModal()
    rv$pending_series_edit <- NULL
  }, ignoreInit = TRUE)

  observeEvent(input$delete_series, {
    id <- suppressWarnings(as.integer(input$setting_series))
    row <- series_row(id)
    if (is.na(row)) {
      rv$status <- "제거할 그룹을 선택하세요"
      return()
    }
    if (nrow(rv$data) && any(rv$data$series_id == id)) {
      showModal(modalDialog(
        title = "그룹 제거 불가",
        paste0("'", rv$series$name[row], "' 그룹에 포인트가 있어 제거할 수 없습니다."),
        easyClose = TRUE,
        footer = modalButton("확인")
      ))
      return()
    }
    removed_name <- rv$series$name[row]
    if (rv$add_mode && identical(as.integer(rv$add_series), id)) {
      set_add_mode(FALSE)
    }
    rv$series <- rv$series[-row, , drop = FALSE]
    selected_id <- if (nrow(rv$series)) {
      as.character(rv$series$id[min(row, nrow(rv$series))])
    } else {
      NULL
    }
    mark_mode_changed("point", paste0(removed_name, " 그룹이 제거되었습니다"))
    refresh_series_choices(selected_id)
  }, ignoreInit = TRUE)

  change_axis_name <- function(axis, new_name) {
    req(rv$calibration, rv$calibration$type == "projective")
    if (rv$updating_calibration_inputs) return()
    input_id <- paste0(axis, "_axis_name")
    current_name <- rv$calibration[[axis]]$column
    new_name <- trimws(new_name)
    if (identical(new_name, current_name)) return()

    calibration <- tryCatch(
      rename_calibration_axis(rv$calibration, axis, new_name),
      error = function(error) {
        rv$status <- conditionMessage(error)
        updateTextInput(session, input_id, value = current_name)
        NULL
      }
    )
    if (is.null(calibration)) return()
    apply_calibration_change(
      calibration,
      paste0(toupper(axis), "축 이름이 변경되었습니다")
    )
  }

  observeEvent(input$x_axis_name, {
    change_axis_name("x", input$x_axis_name)
  }, ignoreInit = TRUE)

  observeEvent(input$y_axis_name, {
    change_axis_name("y", input$y_axis_name)
  }, ignoreInit = TRUE)

  observeEvent(input$calibration_point, {
    req(rv$calibration, rv$calibration$type == "projective")
    selection <- strsplit(input$calibration_point, ":", fixed = TRUE)[[1]]
    req(length(selection) == 2L, selection[1] %in% c("box", "axis"))
    valid_points <- if (selection[1] == "box") {
      names(rv$calibration$box)
    } else {
      names(rv$calibration$axis_points)
    }
    req(selection[2] %in% valid_points)
    rv$calibration_target <- selection[1]
    rv$calibration_point <- selection[2]
  }, ignoreInit = TRUE)

  observeEvent(input$axis_pixel_commit, {
    req(rv$calibration, rv$calibration$type == "projective")
    point_name <- as.character(input$axis_pixel_commit$point)
    req(point_name %in% c("x1", "x2", "y1", "y2"))
    if (rv$updating_calibration_inputs) return()

    pixel_input <- paste0("axis_pixel_", point_name)
    calibration <- rv$calibration
    point <- calibration$axis_points[[point_name]]
    current_value <- if (startsWith(point_name, "x")) point$pixel_x else point$pixel_y
    current_value <- round_pixel_coordinate(current_value)
    value <- suppressWarnings(as.numeric(input$axis_pixel_commit$value))
    if (!is.finite(value)) {
      updateNumericInput(session, pixel_input, value = current_value)
      rv$status <- paste0(toupper(point_name), " 픽셀 위치에 정수를 입력하세요")
      return()
    }
    value <- round_pixel_coordinate(value)
    if (isTRUE(all.equal(value, current_value, tolerance = 0))) {
      updateNumericInput(session, pixel_input, value = current_value)
      return()
    }

    edge <- axis_edge(calibration, point_name)
    coordinate_index <- if (startsWith(point_name, "x")) 1L else 2L
    edge_extent <- edge$end[coordinate_index] - edge$start[coordinate_index]
    fraction <- (value - edge$start[coordinate_index]) / edge_extent
    if (!is.finite(fraction) || fraction < 0 || fraction > 1) {
      updateNumericInput(session, pixel_input, value = current_value)
      rv$status <- "축 포인트의 픽셀 위치는 박스 축 범위 안에 있어야 합니다"
      return()
    }
    calibration$axis_points[[point_name]]$source <- "new"
    calibration$axis_points[[point_name]]$fraction <- fraction
    calibration <- rebuild_calibration_ranges(calibration)
    if (is.null(calibration)) {
      updateNumericInput(session, pixel_input, value = current_value)
      rv$status <- "X1/X2 또는 Y1/Y2의 픽셀 위치 순서를 확인하세요"
      return()
    }
    apply_calibration_change(calibration, "축 포인트의 픽셀 위치가 변경되었습니다")
    update_calibration_inputs()
  }, ignoreInit = TRUE)

  observeEvent(input$axis_value_commit, {
    req(rv$calibration, rv$calibration$type == "projective")
    point_name <- as.character(input$axis_value_commit$point)
    req(point_name %in% c("x1", "x2", "y1", "y2"))
    if (rv$updating_calibration_inputs) return()

    value_input <- paste0("axis_value_", point_name)
    calibration <- rv$calibration
    current_value <- as.numeric(calibration$axis_points[[point_name]]$value)
    value <- suppressWarnings(as.numeric(input$axis_value_commit$value))
    if (!is.finite(value)) {
      updateNumericInput(session, value_input, value = current_value)
      rv$status <- paste0(toupper(point_name), " 값에 숫자를 입력하세요")
      return()
    }
    if (isTRUE(all.equal(value, current_value, tolerance = 0))) return()

    axis_name <- substr(point_name, 1, 1)
    point_number <- substr(point_name, 2, 2)
    other_point_name <- paste0(axis_name, if (point_number == "1") "2" else "1")
    other_value <- as.numeric(calibration$axis_points[[other_point_name]]$value)
    invalid_order <- if (point_number == "1") value >= other_value else value <= other_value
    if (invalid_order) {
      updateNumericInput(session, value_input, value = current_value)
      axis_label <- toupper(axis_name)
      comparison <- if (point_number == "1") "작아야" else "커야"
      rv$status <- paste0(
        axis_label, point_number, " 값은 ",
        axis_label, if (point_number == "1") "2" else "1",
        " 값보다 ", comparison, " 합니다"
      )
      return()
    }

    calibration$axis_points[[point_name]]$value <- value
    calibration <- rebuild_calibration_ranges(calibration)
    if (is.null(calibration)) {
      updateNumericInput(session, value_input, value = current_value)
      rv$status <- "X1은 X2보다, Y1은 Y2보다 작은 값이어야 합니다"
      return()
    }
    apply_calibration_change(calibration, "축 포인트 값이 변경되었습니다")
    update_calibration_inputs()
  }, ignoreInit = TRUE)

  selected_row <- reactive({
    req(rv$data, rv$selected)
    validate(need(nrow(rv$data) > 0, "포인트를 선택하세요"))
    row <- as.integer(rv$selected)
    validate(need(row >= 1 && row <= nrow(rv$data), "포인트를 선택하세요"))
    row
  })

  select_point <- function(row) {
    row <- max(1L, min(as.integer(row), nrow(rv$data)))
    rv$selected <- row
    update_point_choices()
    group_id <- as.character(rv$data$series_id[row])
    refresh_series_choices(group_id)
  }

  select_point_id <- function(point_id, finalize_order = TRUE) {
    point_id <- as.integer(point_id)
    if (finalize_order) sort_points(point_id)
    row <- match(point_id, rv$data$point_id)
    if (!is.na(row)) select_point(row)
  }

  navigate_point <- function(direction) {
    current_id <- selected_point_id()
    sort_points(current_id)
    row <- selected_row()
    next_row <- if (direction == "previous") max(1, row - 1) else min(nrow(rv$data), row + 1)
    select_point(next_row)
  }

  observeEvent(input$previous_point, navigate_point("previous"), ignoreInit = TRUE)
  observeEvent(input$next_point, navigate_point("next"), ignoreInit = TRUE)
  observeEvent(input$key_point_nav, navigate_point(input$key_point_nav))

  observeEvent(input$point, {
    req(rv$data, input$point)
    point_id <- as.integer(input$point)
    if (identical(point_id, selected_point_id())) return()
    select_point_id(point_id)
  })

  move_selected <- function(direction) {
    row <- selected_row()
    remember_point_change(row)
    data <- rv$data
    width <- rv$image_width
    height <- rv$image_height
    step <- as.numeric(input$move_step)

    if (direction == "left") data$pixel_x[row] <- max(0, data$pixel_x[row] - step)
    if (direction == "right") data$pixel_x[row] <- min(width, data$pixel_x[row] + step)
    if (direction == "up") data$pixel_y[row] <- max(0, data$pixel_y[row] - step)
    if (direction == "down") data$pixel_y[row] <- min(height, data$pixel_y[row] + step)

    rv$data <- data
    mark_mode_changed("point")
    update_point_label(row)
  }

  apply_calibration_change <- function(calibration, status) {
    remember_calibration_change()
    rv$calibration <- calibration
    mark_mode_changed("calibration", status)
    refresh_point_choices()
    TRUE
  }

  set_calibration_box_point <- function(box_point, pixel_x, pixel_y) {
    calibration <- rv$calibration
    req(calibration$type == "projective", box_point %in% names(calibration$box))
    pixel_x <- round_pixel_coordinate(pixel_x)
    pixel_y <- round_pixel_coordinate(pixel_y)
    if (!is.finite(pixel_x) || pixel_x < 0 || pixel_x > rv$image_width) {
      rv$status <- sprintf("박스 x 좌표는 0~%d 범위 안에 있어야 합니다", rv$image_width)
      update_calibration_inputs()
      return(FALSE)
    }
    if (!is.finite(pixel_y) || pixel_y < 0 || pixel_y > rv$image_height) {
      rv$status <- sprintf("박스 y 좌표는 0~%d 범위 안에 있어야 합니다", rv$image_height)
      update_calibration_inputs()
      return(FALSE)
    }
    calibration$box[[box_point]]$pixel_x <- pixel_x
    calibration$box[[box_point]]$pixel_y <- pixel_y
    if (is.null(calibration) || !valid_projective_calibration(calibration)) {
      rv$status <- "네 모서리가 교차하지 않는 박스가 되도록 지정하세요"
      return(FALSE)
    }
    calibration <- rebuild_calibration_ranges(calibration)
    if (is.null(calibration)) {
      rv$status <- "축 포인트의 위치와 값을 확인하세요"
      return(FALSE)
    }
    changed <- apply_calibration_change(calibration, "보정 박스가 변경되었습니다")
    update_calibration_inputs()
    changed
  }

  observeEvent(input$box_coordinate_commit, {
    req(rv$calibration, rv$calibration$type == "projective")
    input_id <- as.character(input$box_coordinate_commit$id)
    matched <- regmatches(
      input_id,
      regexec("^box_(origin|x_axis_end|y_axis_end|xy_axis_end)_([xy])$", input_id)
    )[[1]]
    req(length(matched) == 3L)
    if (rv$updating_calibration_inputs) return()

    point_name <- matched[2]
    coordinate <- matched[3]
    point <- rv$calibration$box[[point_name]]
    coordinate_name <- paste0("pixel_", coordinate)
    current_value <- as.numeric(point[[coordinate_name]])
    value <- round_pixel_coordinate(input$box_coordinate_commit$value)
    if (isTRUE(all.equal(value, current_value, tolerance = 0))) {
      updateNumericInput(session, input_id, value = current_value)
      return()
    }

    pixel_x <- if (coordinate == "x") value else point$pixel_x
    pixel_y <- if (coordinate == "y") value else point$pixel_y
    set_calibration_box_point(point_name, pixel_x, pixel_y)
  }, ignoreInit = TRUE)

  set_calibration_axis_fraction <- function(axis_point, fraction) {
    calibration <- rv$calibration
    req(calibration$type == "projective", axis_point %in% names(calibration$axis_points))
    calibration$axis_points[[axis_point]]$source <- "new"
    calibration$axis_points[[axis_point]]$fraction <- max(0, min(1, fraction))
    calibration <- rebuild_calibration_ranges(calibration)
    if (is.null(calibration)) {
      rv$status <- "X1/X2 또는 Y1/Y2가 서로 다른 위치와 값을 갖도록 지정하세요"
      update_calibration_inputs()
      return(FALSE)
    }
    changed <- apply_calibration_change(calibration, "축 포인트가 변경되었습니다")
    update_calibration_inputs()
    changed
  }

  move_axis_point <- function(direction) {
    req(
      rv$calibration$type == "projective",
      identical(rv$calibration_target, "axis"), rv$calibration_point
    )
    axis_point <- rv$calibration_point
    point <- rv$calibration$axis_points[[axis_point]]
    step <- as.numeric(input$calibration_move_step)
    edge <- axis_edge(rv$calibration, axis_point)
    if (startsWith(axis_point, "x")) {
      if (!direction %in% c("left", "right")) return()
      coordinate_index <- 1L
      coordinate <- round_pixel_coordinate(point$pixel_x)
      coordinate <- coordinate + if (direction == "left") -step else step
    } else {
      if (!direction %in% c("up", "down")) return()
      coordinate_index <- 2L
      coordinate <- round_pixel_coordinate(point$pixel_y)
      coordinate <- coordinate + if (direction == "up") -step else step
    }
    edge_extent <- edge$end[coordinate_index] - edge$start[coordinate_index]
    fraction <- (coordinate - edge$start[coordinate_index]) / edge_extent
    set_calibration_axis_fraction(axis_point, fraction)
  }

  move_box_point <- function(direction) {
    req(
      rv$calibration$type == "projective",
      identical(rv$calibration_target, "box"), rv$calibration_point
    )
    point <- rv$calibration$box[[rv$calibration_point]]
    step <- as.numeric(input$calibration_move_step)
    pixel_x <- as.numeric(point$pixel_x)
    pixel_y <- as.numeric(point$pixel_y)
    if (direction == "left") pixel_x <- max(0, pixel_x - step)
    if (direction == "right") pixel_x <- min(rv$image_width, pixel_x + step)
    if (direction == "up") pixel_y <- max(0, pixel_y - step)
    if (direction == "down") pixel_y <- min(rv$image_height, pixel_y + step)
    set_calibration_box_point(rv$calibration_point, pixel_x, pixel_y)
  }

  move_target <- function(direction) {
    if (active_mode_is("calibration")) {
      if (identical(rv$calibration_target, "axis")) {
        move_axis_point(direction)
      } else {
        move_box_point(direction)
      }
    } else {
      move_selected(direction)
    }
  }

  observeEvent(input$left, move_target("left"))
  observeEvent(input$right, move_target("right"))
  observeEvent(input$up, move_target("up"))
  observeEvent(input$down, move_target("down"))
  observeEvent(input$calibration_left, move_target("left"))
  observeEvent(input$calibration_right, move_target("right"))
  observeEvent(input$calibration_up, move_target("up"))
  observeEvent(input$calibration_down, move_target("down"))

  undo_target <- function() {
    if (active_mode_is("calibration")) {
      snapshot <- rv$calibration_undo
      if (is.null(snapshot) || !identical(snapshot$target, calibration_target_key())) {
        rv$status <- "선택한 보정 대상에 되돌릴 직전 변경이 없습니다"
        return()
      }
      rv$calibration <- snapshot$calibration
      rv$calibration_dirty <- snapshot$calibration_dirty
      rv$calibration_undo <- NULL
      rv$status <- unsaved_status()
      update_calibration_inputs()
      refresh_point_choices()
      return()
    }

    row <- selected_row()
    snapshot <- rv$point_undo
    if (is.null(snapshot) || snapshot$point_id != rv$data$point_id[row]) {
      rv$status <- "선택한 포인트에 되돌릴 직전 변경이 없습니다"
      return()
    }

    rv$data <- snapshot$data
    rv$selected <- match(snapshot$point_id, rv$data$point_id)
    rv$point_dirty <- snapshot$point_dirty
    rv$point_undo <- NULL
    rv$status <- unsaved_status()
    refresh_controls(rv$selected)
  }

  observeEvent(input$undo, undo_target())
  observeEvent(input$calibration_undo, undo_target())

  observeEvent(input$reload, {
    req(rv$dataset)
    cancel_mode <- rv$active_edit_mode
    mode_label <- edit_mode_label(cancel_mode)
    has_changes <- mode_changes_pending(cancel_mode)
    if (!has_changes) {
      rv$status <- paste0("취소할 ", mode_label, " 변경이 없습니다")
      return()
    }
    rv$pending_cancel_mode <- cancel_mode
    showModal(modalDialog(
      title = paste0(mode_label, " 변경 취소"),
      paste0(
        "마지막 저장 또는 모드 전환 이후의 ", mode_label,
        " 변경만 취소하시겠습니까? 다른 모드의 변경은 유지됩니다."
      ),
      footer = tagList(
        actionButton("cancel_reload", "아니오"),
        actionButton("confirm_reload", "예", class = "btn-danger")
      ),
      easyClose = FALSE
    ))
  })

  observeEvent(input$cancel_reload, {
    removeModal()
    rv$pending_cancel_mode <- NULL
  })

  observeEvent(input$confirm_reload, {
    req(rv$dataset, rv$pending_cancel_mode)
    cancel_mode <- rv$pending_cancel_mode
    removeModal()
    discard_edit_mode_changes(cancel_mode)
    rv$status <- paste0(edit_mode_label(cancel_mode), " 변경 취소됨")
    rv$pending_cancel_mode <- NULL
  })

  toggle_add_point <- function() {
    req(rv$data)
    if (!length(input$setting_series) || is.na(series_row(input$setting_series))) {
      rv$status <- "포인트를 추가할 그룹을 선택하세요"
      return()
    }
    if (rv$add_mode) {
      point_id <- selected_point_id()
      set_add_mode(FALSE)
      sort_points(point_id)
      refresh_controls(rv$selected)
      rv$status <- unsaved_status()
    } else {
      set_add_mode(TRUE, input$setting_series)
      rv$status <- "원본 그림을 클릭하여 포인트를 연속으로 입력하세요"
    }
  }

  observeEvent(input$add_point, toggle_add_point(), ignoreInit = TRUE)

  observeEvent(input$delete_point, {
    if (is.null(rv$selected) || !nrow(rv$data)) {
      rv$status <- "제거할 포인트를 선택하세요"
      return()
    }
    row <- selected_row()
    rv$point_undo <- NULL
    rv$data <- rv$data[-row, , drop = FALSE]
    next_point_id <- if (nrow(rv$data)) {
      rv$data$point_id[min(row, nrow(rv$data))]
    } else {
      NULL
    }
    sort_points(next_point_id)
    mark_mode_changed("point", "포인트가 제거되었습니다")
    set_add_mode(FALSE)
    refresh_controls(rv$selected)
  }, ignoreInit = TRUE)

  observeEvent(input$overview_click, {
    req(rv$data)
    if (active_mode_is("calibration")) {
      req(rv$calibration$type == "projective")
      x <- round_pixel_coordinate(max(0, min(rv$image_width, input$overview_click$x)))
      y <- round_pixel_coordinate(max(0, min(rv$image_height, input$overview_click$y)))
      if (identical(rv$calibration_target, "axis")) {
        req(rv$calibration_point)
        fraction <- project_to_axis_edge(x, y, axis_edge(rv$calibration, rv$calibration_point))
        set_calibration_axis_fraction(rv$calibration_point, fraction)
      } else {
        req(identical(rv$calibration_target, "box"), rv$calibration_point)
        set_calibration_box_point(rv$calibration_point, x, y)
      }
      return()
    }

    if (rv$add_mode) {
      series_id <- as.integer(rv$add_series)
      req(!is.na(series_row(series_id)))
      x <- round_pixel_coordinate(max(0, min(rv$image_width, input$overview_click$x)))
      y <- round_pixel_coordinate(max(0, min(rv$image_height, input$overview_click$y)))
      rv$point_undo <- NULL
      point_id <- if (nrow(rv$data)) max(rv$data$point_id) + 1L else 1L
      new_row <- data.frame(
        point_id = point_id, series_id = series_id,
        pixel_x = x, pixel_y = y
      )
      rv$data <- rbind(rv$data, new_row)
      rv$selected <- nrow(rv$data)
      mark_mode_changed(
        "point", "새 포인트가 추가되었습니다. 계속 추가하거나 연속추가 종료를 누르세요"
      )
      refresh_controls(rv$selected)
      return()
    }

    if (!nrow(rv$data)) {
      rv$status <- "그룹을 선택하고 + 버튼을 눌러 포인트를 추가하세요"
      return()
    }
    distance <- (rv$data$pixel_x - input$overview_click$x)^2 +
      (rv$data$pixel_y - input$overview_click$y)^2
    row <- which.min(distance)
    select_point_id(rv$data$point_id[row])
  })

  draw_plot_window <- function(xlim = NULL, ylim = NULL, background = "white") {
    width <- rv$image_width
    height <- rv$image_height
    if (is.null(xlim)) xlim <- c(0, width)
    if (is.null(ylim)) ylim <- c(height, 0)

    par(mar = c(0, 0, 0, 0), bg = background)
    plot.new()
    plot.window(xlim, ylim, xaxs = "i", yaxs = "i", asp = 1)
  }

  draw_series_points <- function(rows, cex = 0.9) {
    if (!length(rows) || is.null(rv$series) || !nrow(rv$series)) return(invisible())
    for (series_id in unique(rv$data$series_id[rows])) {
      style_row <- match(series_id, rv$series$id)
      if (is.na(style_row)) next
      series_rows <- rows[rv$data$series_id[rows] == series_id]
      points(
        rv$data$pixel_x[series_rows], rv$data$pixel_y[series_rows],
        pch = marker_pch(rv$series$marker[style_row]),
        col = grDevices::adjustcolor(
          rv$series$color[style_row], alpha.f = rv$series$alpha[style_row]
        ),
        cex = cex * rv$series$size[style_row], lwd = 1.4
      )
    }
    invisible()
  }

  draw_selected_point <- function(row, cex = 1.35) {
    if (is.null(row) || !length(row) || !nrow(rv$data)) return(invisible())
    points(
      rv$data$pixel_x[row], rv$data$pixel_y[row],
      pch = 1, col = "#d62728", cex = cex, lwd = 2
    )
    invisible()
  }

  output$overview_image <- renderPlot({
    req(rv$image_width, rv$image_height, rv$raster_matrix)
    width <- rv$image_width
    height <- rv$image_height
    draw_plot_window()
    rasterImage(rv$raster_matrix, 0, height, width, 0)
  }, res = 110)

  output$overview <- renderPlot({
    req(rv$data, rv$image_width, rv$image_height)
    draw_plot_window(background = NA)
    selected_box_point <- if (
      active_mode_is("calibration") && identical(rv$calibration_target, "box")
    ) rv$calibration_point else NULL
    selected_axis_point <- if (
      active_mode_is("calibration") && identical(rv$calibration_target, "axis")
    ) rv$calibration_point else NULL
    draw_calibration_grid(
      rv$calibration, selected_box_point, selected_axis_point,
      box_only = !active_mode_is("calibration")
    )
    if (nrow(rv$data)) draw_series_points(seq_len(nrow(rv$data)))
    if (!active_mode_is("calibration") && !is.null(rv$selected)) {
      draw_selected_point(rv$selected, cex = 1.5)
    }
  }, res = 110, bg = "transparent")

  selected_target <- reactive({
    req(rv$data, rv$image_width, rv$image_height)
    if (active_mode_is("calibration")) {
      req(rv$calibration$type == "projective")
      if (identical(rv$calibration_target, "axis")) {
        req(rv$calibration_point)
        point <- rv$calibration$axis_points[[rv$calibration_point]]
      } else {
        req(identical(rv$calibration_target, "box"), rv$calibration_point)
        point <- rv$calibration$box[[rv$calibration_point]]
      }
      return(c(pixel_x = as.numeric(point$pixel_x), pixel_y = as.numeric(point$pixel_y)))
    }
    row <- selected_row()
    c(pixel_x = rv$data$pixel_x[row], pixel_y = rv$data$pixel_y[row])
  })

  output$zoom_plot <- renderPlot({
    req(rv$data, rv$image_width, rv$image_height, rv$raster_matrix)
    radius <- as.numeric(input$zoom)
    target <- selected_target()
    x <- target[["pixel_x"]]
    y <- target[["pixel_y"]]
    xlim <- c(x - radius, x + radius)
    ylim <- c(y + radius, y - radius)
    draw_plot_window(xlim, ylim)

    width <- rv$image_width
    height <- rv$image_height
    x_left <- max(0L, floor(xlim[1]))
    x_right <- min(width, ceiling(xlim[2]))
    y_top <- max(0L, floor(ylim[2]))
    y_bottom <- min(height, ceiling(ylim[1]))
    crop <- rv$raster_matrix[
      (y_top + 1L):y_bottom,
      (x_left + 1L):x_right,
      drop = FALSE
    ]
    rasterImage(crop, x_left, y_bottom, x_right, y_top)
    selected_box_point <- if (
      active_mode_is("calibration") && identical(rv$calibration_target, "box")
    ) rv$calibration_point else NULL
    selected_axis_point <- if (
      active_mode_is("calibration") && identical(rv$calibration_target, "axis")
    ) rv$calibration_point else NULL
    draw_calibration_grid(
      rv$calibration, selected_box_point, selected_axis_point,
      box_only = !active_mode_is("calibration")
    )

    nearby <- rv$data$pixel_x >= xlim[1] & rv$data$pixel_x <= xlim[2] &
      rv$data$pixel_y >= ylim[2] & rv$data$pixel_y <= ylim[1]
    nearby_rows <- which(nearby)
    draw_series_points(nearby_rows, cex = 1.15)
    if (!active_mode_is("calibration") && !is.null(rv$selected)) {
      selected_cex <- 8 * 20 / radius
      draw_selected_point(rv$selected, cex = selected_cex)
    }
  }, res = 130)

  output$symbol_swatch <- renderPlot({
    req(rv$series)
    row <- series_row(input$setting_series)
    req(!is.na(row))
    par(mar = c(0, 0, 0, 0), bg = "transparent")
    plot.new()
    plot.window(c(0, 1), c(0, 1), xaxs = "i", yaxs = "i", asp = 1)
    points(
      0.5, 0.5,
      pch = marker_pch(rv$series$marker[row]),
      col = grDevices::adjustcolor(
        rv$series$color[row], alpha.f = rv$series$alpha[row]
      ),
      cex = 1.35 * rv$series$size[row], lwd = 1.5
    )
  }, width = 30, height = 34, res = 96, bg = "transparent")

  output$point_values <- renderText({
    req(rv$data, rv$calibration)
    if (active_mode_is("calibration")) {
      req(rv$calibration$type == "projective")
      if (identical(rv$calibration_target, "box")) {
        req(rv$calibration_point)
        point <- rv$calibration$box[[rv$calibration_point]]
        return(sprintf(
          "%s\npixel x: %s\npixel y: %s",
          box_point_display_labels[[rv$calibration_point]],
          format_pixel_coordinate(point$pixel_x),
          format_pixel_coordinate(point$pixel_y)
        ))
      }
      req(identical(rv$calibration_target, "axis"), rv$calibration_point)
      point <- rv$calibration$axis_points[[rv$calibration_point]]
      axis_name <- if (startsWith(rv$calibration_point, "x")) {
        rv$calibration$x$column
      } else {
        rv$calibration$y$column
      }
      return(sprintf(
        "%s\npixel x: %s\npixel y: %s\n%s: %s",
        toupper(rv$calibration_point),
        format_pixel_coordinate(point$pixel_x),
        format_pixel_coordinate(point$pixel_y),
        axis_name, format(as.numeric(point$value), digits = 7)
      ))
    }

    row <- selected_row()
    calibration <- rv$calibration
    values <- axis_values(rv$data, calibration)
    point_series_row <- match(rv$data$series_id[row], rv$series$id)
    group_name <- if (is.na(point_series_row)) "알 수 없음" else rv$series$name[point_series_row]
    group_number <- match(row, which(rv$data$series_id == rv$data$series_id[row]))
    sprintf(
      "그룹: %s\n포인트 번호: %d-%d\npixel x: %s\npixel y: %s\n%s: %s\n%s: %s",
      group_name,
      rv$data$point_id[row], group_number,
      format_pixel_coordinate(rv$data$pixel_x[row]),
      format_pixel_coordinate(rv$data$pixel_y[row]),
      calibration$x$column, format(values$x[row], digits = 7),
      calibration$y$column, format(values$y[row], digits = 7)
    )
  })

  output$detail_title <- renderText({
    if (!active_mode_is("calibration")) {
      if (is.null(rv$selected)) return("포인트 미선택")
      return("선택한 포인트")
    }
    if (is.null(rv$calibration_target)) return("설정 포인트 미선택")
    if (identical(rv$calibration_target, "box")) "선택한 박스 포인트" else "선택한 축 포인트"
  })

  observeEvent(input$save, {
    req(!is.null(save_changes(auto = FALSE)))
  })

  output$status <- renderText(rv$status)
}

shinyApp(ui, server)
