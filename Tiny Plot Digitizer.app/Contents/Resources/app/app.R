required_packages <- c("shiny", "png")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages)) {
  stop(
    paste0(
      "필요한 R 패키지가 없습니다: ", paste(missing_packages, collapse = ", "),
      "\n다음 명령으로 설치하세요:\ninstall.packages(c(",
      paste(sprintf("%s", encodeString(missing_packages, quote = '"')), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

library(shiny)

decode_project_header_string <- function(text) {
  expression <- try(parse(text = text, keep.source = FALSE), silent = TRUE)
  if (inherits(expression, "try-error") || length(expression) != 1L ||
      !is.character(expression[[1]]) || length(expression[[1]]) != 1L) {
    stop("CSV 설정 정보의 문자열 형식을 확인하세요")
  }
  expression[[1]]
}

parse_project_header_key <- function(text) {
  text <- trimws(text)
  key <- if (startsWith(text, '"')) {
    decode_project_header_string(text)
  } else {
    if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", text)) {
      stop("CSV 설정 정보의 항목 이름을 확인하세요")
    }
    text
  }
  if (!nzchar(key)) stop("CSV 설정 정보에 빈 항목 이름이 있습니다")
  key
}

parse_project_header_scalar <- function(text) {
  text <- trimws(text)
  if (startsWith(text, '"')) return(decode_project_header_string(text))
  number_pattern <- paste0(
    "^[+-]?(?:[0-9]+(?:[.][0-9]*)?|[.][0-9]+)",
    "(?:[eE][+-]?[0-9]+)?$"
  )
  if (grepl(number_pattern, text, perl = TRUE)) return(as.numeric(text))
  if (grepl("^[A-Za-z][A-Za-z0-9_.-]*$", text)) return(text)
  stop("CSV 설정 정보에 지원하지 않는 값이 있습니다")
}

split_project_header_pair <- function(text) {
  pattern <- paste0(
    '^("(?:\\\\.|[^"\\\\])*"|[A-Za-z_][A-Za-z0-9_]*):',
    "[[:space:]]*(.*)$"
  )
  fields <- regmatches(text, regexec(pattern, trimws(text), perl = TRUE))[[1]]
  if (length(fields) != 3L) stop("CSV 설정 정보의 항목 구분자를 확인하세요")
  list(key = parse_project_header_key(fields[[2]]), value = fields[[3]])
}

parse_project_header_value <- function(text) {
  text <- trimws(text)
  if (identical(text, "{}")) return(list())
  if (startsWith(text, "{") && substr(text, nchar(text), nchar(text)) == "}") {
    content <- trimws(substr(text, 2L, nchar(text) - 1L))
    if (!nzchar(content)) return(list())
    fields <- strsplit(content, ",[[:space:]]*", perl = TRUE)[[1]]
    values <- lapply(fields, function(field) {
      pair <- split_project_header_pair(field)
      list(key = pair$key, value = parse_project_header_scalar(pair$value))
    })
    keys <- vapply(values, `[[`, character(1), "key")
    if (anyDuplicated(keys)) stop("CSV 설정 정보의 항목 이름이 중복되어 있습니다")
    return(setNames(lapply(values, `[[`, "value"), keys))
  }
  parse_project_header_scalar(text)
}

parse_project_header <- function(lines) {
  metadata <- list()
  section <- NULL
  for (line in lines) {
    if (!nzchar(trimws(line))) next
    content <- sub("^ +", "", line)
    indentation <- nchar(line) - nchar(content)
    if (indentation == 0L) {
      pair <- split_project_header_pair(content)
      if (pair$key %in% names(metadata)) {
        stop("CSV 설정 정보의 최상위 항목이 중복되어 있습니다")
      }
      if (!nzchar(pair$value)) {
        metadata[pair$key] <- list(list())
        section <- pair$key
      } else {
        metadata[pair$key] <- list(parse_project_header_value(pair$value))
        section <- NULL
      }
    } else if (indentation == 2L && !is.null(section)) {
      pair <- split_project_header_pair(content)
      section_data <- metadata[[section]]
      if (pair$key %in% names(section_data)) {
        stop("CSV 설정 정보의 하위 항목이 중복되어 있습니다")
      }
      section_data[pair$key] <- list(parse_project_header_value(pair$value))
      metadata[section] <- list(section_data)
    } else {
      stop("CSV 설정 정보의 들여쓰기를 확인하세요")
    }
  }
  metadata
}

read_project_header_lines <- function(path) {
  connection <- file(path, open = "r", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  first_line <- readLines(connection, n = 1L, warn = FALSE)
  if (!length(first_line) || trimws(first_line) != "# ---") {
    return(character())
  }
  header_lines <- character()
  repeat {
    line <- readLines(connection, n = 1L, warn = FALSE)
    if (!length(line)) return(character())
    if (trimws(line) == "# ---") {
      return(sub("^# ?", "", header_lines))
    }
    if (nzchar(line) && !startsWith(line, "#")) return(character())
    header_lines <- c(header_lines, line)
  }
}

read_csv_metadata <- function(path) {
  metadata_lines <- read_project_header_lines(path)
  if (!length(metadata_lines)) return(list())
  format_lines <- metadata_lines[grepl("^format[[:space:]]*:", metadata_lines)]
  if (length(format_lines) != 1L) return(list())
  format_value <- tryCatch({
    pair <- split_project_header_pair(format_lines[[1]])
    if (!identical(pair$key, "format")) return(list())
    parse_project_header_scalar(pair$value)
  }, error = function(error) NULL)
  if (!identical(as.character(format_value), project_format)) return(list())
  parse_project_header(metadata_lines)
}

solve_projective_coefficients <- function(source_points, target_points) {
  corner_names <- c("origin", "x_axis_end", "xy_axis_end", "y_axis_end")
  projective_matrix <- matrix(0, nrow = 8, ncol = 8)
  projective_target <- as.vector(t(target_points))

  for (index in seq_along(corner_names)) {
    x <- source_points[index, 1]
    y <- source_points[index, 2]
    u <- target_points[index, 1]
    v <- target_points[index, 2]
    projective_matrix[2 * index - 1L, ] <- c(
      x, y, 1, 0, 0, 0, -u * x, -u * y
    )
    projective_matrix[2 * index, ] <- c(
      0, 0, 0, x, y, 1, -v * x, -v * y
    )
  }
  solve(projective_matrix, projective_target)
}

projective_transform_cache <- new.env(parent = emptyenv())
projective_transform_cache$key <- NULL
projective_transform_cache$value <- NULL

projective_transforms <- function(calibration_box) {
  corner_names <- c("origin", "x_axis_end", "xy_axis_end", "y_axis_end")
  if (!all(corner_names %in% names(calibration_box))) {
    stop("박스 설정에 네 모서리 좌표가 필요합니다", call. = FALSE)
  }
  pixel_corners <- t(vapply(
    corner_names,
    function(corner) {
      c(
        pixel_x = as.numeric(calibration_box[[corner]]$pixel_x),
        pixel_y = as.numeric(calibration_box[[corner]]$pixel_y)
      )
    },
    numeric(2)
  ))
  cache_key <- as.vector(t(pixel_corners))
  if (identical(projective_transform_cache$key, cache_key)) {
    return(projective_transform_cache$value)
  }
  unit_corners <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1))
  value <- list(
    pixel_to_unit = solve_projective_coefficients(pixel_corners, unit_corners),
    unit_to_pixel = solve_projective_coefficients(unit_corners, pixel_corners)
  )
  projective_transform_cache$key <- cache_key
  projective_transform_cache$value <- value
  value
}

apply_projective_transform <- function(first, second, coefficients, column_names) {
  denominator <- coefficients[7] * first + coefficients[8] * second + 1
  result <- data.frame(
    first = (
      coefficients[1] * first + coefficients[2] * second + coefficients[3]
    ) / denominator,
    second = (
      coefficients[4] * first + coefficients[5] * second + coefficients[6]
    ) / denominator
  )
  names(result) <- column_names
  result
}

project_pixels_to_unit <- function(pixel_x, pixel_y, calibration_box) {
  if (length(pixel_x) != length(pixel_y)) {
    stop("pixel_x와 pixel_y의 길이가 서로 다릅니다", call. = FALSE)
  }

  apply_projective_transform(
    pixel_x, pixel_y,
    projective_transforms(calibration_box)$pixel_to_unit,
    c("x_fraction", "y_fraction")
  )
}

round_pixel_coordinate <- function(value) {
  round(suppressWarnings(as.numeric(value)) * 2) / 2
}

format_pixel_coordinate <- function(value) {
  format(round_pixel_coordinate(value), scientific = FALSE, trim = TRUE)
}

project_format <- "Tiny Plot Digitizer 2026.07"

parse_projective_calibration <- function(metadata, columns) {
  if (!identical(as.character(metadata$format), project_format)) return(NULL)

  box <- metadata$box_points
  corner_names <- c("origin", "x_axis_end", "xy_axis_end", "y_axis_end")
  if (is.null(box) || !all(corner_names %in% names(box))) return(NULL)
  if (!all(c("pixel_x", "pixel_y") %in% columns)) return(NULL)

  axes_metadata <- list(x = metadata$x_axis, y = metadata$y_axis)
  required_axis_fields <- c("name", "scale", "position")
  if (any(vapply(axes_metadata, function(axis) {
    is.null(axis) || !is.list(axis) || !all(required_axis_fields %in% names(axis)) ||
      any(vapply(axis[required_axis_fields], length, integer(1)) != 1L)
  }, logical(1)))) {
    return(NULL)
  }

  axis_names <- vapply(axes_metadata, function(axis) {
    as.character(axis$name)
  }, character(1))
  if (any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", axis_names)) ||
      anyDuplicated(axis_names) || !all(axis_names %in% columns)) {
    return(NULL)
  }

  axis_scales <- vapply(axes_metadata, function(axis) {
    as.character(axis$scale)
  }, character(1))
  if (any(!axis_scales %in% c("linear", "log10"))) return(NULL)

  axis_positions <- vapply(axes_metadata, function(axis) {
    as.character(axis$position)
  }, character(1))
  if (!axis_positions[["x"]] %in% c("bottom", "top") ||
      !axis_positions[["y"]] %in% c("left", "right")) {
    return(NULL)
  }

  saved_points <- list(
    x1 = axes_metadata$x$x1,
    x2 = axes_metadata$x$x2,
    y1 = axes_metadata$y$y1,
    y2 = axes_metadata$y$y2
  )
  point_names <- c("x1", "x2", "y1", "y2")
  if (any(vapply(saved_points, is.null, logical(1)))) return(NULL)

  axis_points <- lapply(point_names, function(name) {
    point <- saved_points[[name]]
    if (!is.list(point) || !all(c("pixel", "value") %in% names(point)) ||
        length(point$pixel) != 1L || length(point$value) != 1L) {
      return(NULL)
    }
    coordinate <- as.numeric(point$pixel)
    value <- as.numeric(point$value)
    if (!is.finite(coordinate) || !is.finite(value)) return(NULL)

    edge_calibration <- list(
      x = list(position = axis_positions[["x"]]),
      y = list(position = axis_positions[["y"]]),
      box = box
    )
    edge <- axis_edge(edge_calibration, name)
    coordinate_index <- if (startsWith(name, "x")) 1L else 2L
    start <- edge$start
    end <- edge$end
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
      scale = axis_scales[[axis]],
      position = axis_positions[[axis]],
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

interpolate_axis_values <- function(fraction, axis) {
  if (identical(axis$scale, "log10")) {
    return(10^(
      log10(axis$minimum) + fraction * (log10(axis$maximum) - log10(axis$minimum))
    ))
  }
  axis$minimum + fraction * (axis$maximum - axis$minimum)
}

axis_values <- function(data, calibration) {
  position <- project_pixels_to_unit(
    data$pixel_x,
    data$pixel_y,
    calibration$box
  )
  position$x_fraction <- pmax(0, pmin(1, position$x_fraction))
  position$y_fraction <- pmax(0, pmin(1, position$y_fraction))
  x <- interpolate_axis_values(position$x_fraction, calibration$x)
  if (!is.null(calibration$x$zero_threshold)) {
    x[abs(x) < calibration$x$zero_threshold] <- 0
  }
  y <- interpolate_axis_values(position$y_fraction, calibration$y)
  data.frame(x = x, y = y)
}

crop_raster <- function(image, rows, columns) {
  if (!inherits(image, "nativeRaster")) {
    return(image[rows, columns, drop = FALSE])
  }
  width <- dim(image)[2]
  indices <- unlist(lapply(
    rows,
    function(row) (as.integer(row) - 1L) * width + as.integer(columns)
  ), use.names = FALSE)
  crop <- as.vector(image)[indices]
  dim(crop) <- c(length(rows), length(columns))
  class(crop) <- "nativeRaster"
  attr(crop, "channels") <- attr(image, "channels")
  crop
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

axis_edge_corner_names <- function(calibration, axis_point) {
  if (startsWith(axis_point, "x")) {
    position <- if (!is.null(calibration$x$position)) calibration$x$position else "bottom"
    if (identical(position, "top")) {
      return(c("y_axis_end", "xy_axis_end"))
    }
    return(c("origin", "x_axis_end"))
  }

  position <- if (!is.null(calibration$y$position)) calibration$y$position else "left"
  if (identical(position, "right")) {
    return(c("x_axis_end", "xy_axis_end"))
  }
  c("origin", "y_axis_end")
}

axis_point_box_corner <- function(calibration, axis_point) {
  edge_corners <- axis_edge_corner_names(calibration, axis_point)
  edge_corners[if (axis_point %in% c("x1", "y1")) 1L else 2L]
}

axis_edge <- function(calibration, axis_point) {
  edge_corners <- axis_edge_corner_names(calibration, axis_point)
  start <- calibration$box[[edge_corners[1]]]
  end <- calibration$box[[edge_corners[2]]]
  list(
    start = c(
      as.numeric(start$pixel_x),
      as.numeric(start$pixel_y)
    ),
    end = c(
      as.numeric(end$pixel_x),
      as.numeric(end$pixel_y)
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
    corner <- calibration$box[[axis_point_box_corner(calibration, name)]]
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
  calculate_range <- function(first_name, second_name, scale) {
    first <- points[points$axis_point == first_name, ]
    second <- points[points$axis_point == second_name, ]
    if (second$fraction - first$fraction <= 1e-8 || second$value <= first$value) return(NULL)
    if (identical(scale, "log10") && (first$value <= 0 || second$value <= 0)) return(NULL)
    transformed <- if (identical(scale, "log10")) {
      log10(c(first$value, second$value))
    } else {
      c(first$value, second$value)
    }
    slope <- (transformed[2] - transformed[1]) / (second$fraction - first$fraction)
    transformed_range <- c(
      minimum = transformed[1] - first$fraction * slope,
      maximum = transformed[1] + (1 - first$fraction) * slope
    )
    range <- if (identical(scale, "log10")) 10^transformed_range else transformed_range
    if (any(!is.finite(range))) return(NULL)
    range
  }
  x_range <- calculate_range("x1", "x2", calibration$x$scale)
  y_range <- calculate_range("y1", "y2", calibration$y$scale)
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
    stop("같은 이름의 축 설정 정보가 이미 있습니다.")
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
  if (length(x_fraction) != length(y_fraction)) {
    stop("x_fraction과 y_fraction의 길이가 서로 다릅니다", call. = FALSE)
  }

  apply_projective_transform(
    x_fraction, y_fraction,
    projective_transforms(calibration$box)$unit_to_pixel,
    c("pixel_x", "pixel_y")
  )
}

axis_point_marker <- function(calibration, axis_point) {
  if (startsWith(axis_point, "x")) {
    if (identical(calibration$x$position, "top")) "triangle_down" else "triangle_up"
  } else {
    if (identical(calibration$y$position, "right")) "triangle_left" else "triangle_right"
  }
}

draw_axis_point_marker <- function(x, y, marker, color, cex = 1.35) {
  angles <- switch(
    marker,
    triangle_up = c(90, 210, 330),
    triangle_right = c(0, 120, 240),
    triangle_down = c(-90, 30, 150),
    triangle_left = c(180, 300, 60)
  )
  if (is.null(angles)) return(invisible())

  radius <- 0.055 * cex
  x_scale <- diff(par("usr")[1:2]) / par("pin")[1]
  y_scale <- diff(par("usr")[3:4]) / par("pin")[2]
  radians <- angles * pi / 180
  polygon(
    x + radius * cos(radians) * x_scale,
    y + radius * sin(radians) * y_scale,
    border = color, col = color, xpd = NA
  )
  invisible()
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
  for (index in seq_len(nrow(axis_points))) {
    x <- axis_points$pixel_x[index]
    y <- axis_points$pixel_y[index]
    point_color <- if (
      !is.null(selected_axis_point) && axis_points$axis_point[index] == selected_axis_point
    ) "#d62728" else "#1f5fbf"
    draw_axis_point_marker(
      x, y,
      marker = axis_point_marker(calibration, axis_points$axis_point[index]),
      color = point_color
    )
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

is_digitizing_project_file <- function(path, source_path, metadata = NULL) {
  if (!file.exists(path)) return(FALSE)
  if (is.null(metadata)) metadata <- try(read_csv_metadata(path), silent = TRUE)
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
    !is.null(metadata$box_points) && !is.null(metadata$x_axis) &&
    !is.null(metadata$y_axis) && !is.null(metadata$display_styles)
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
    if (!file.exists(source_path) ||
        !is_digitizing_project_file(path, source_path, metadata)) {
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
      box_points = box,
      x_axis = list(
        name = "x", scale = "linear", position = "bottom",
        x1 = list(pixel = 0, value = 0),
        x2 = list(pixel = width, value = 1)
      ),
      y_axis = list(
        name = "y", scale = "linear", position = "left",
        y1 = list(pixel = height, value = 0),
        y2 = list(pixel = 0, value = 1)
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
    stop("CSV의 그룹 정보를 읽을 수 없습니다")
  }
  rows <- lapply(seq_along(value), function(index) {
    item <- value[[index]]
    required <- c("symbol", "color", "size", "alpha")
    if (!all(required %in% names(item))) {
      stop("CSV의 그룹 표시 설정을 확인하세요")
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

quote_project_header <- function(value) {
  value <- as.character(value)
  if (length(value) != 1L || is.na(value)) {
    stop("CSV 설정 정보에 올바르지 않은 문자열이 있습니다")
  }
  value <- gsub("\\", "\\\\", value, fixed = TRUE)
  value <- gsub('"', '\\"', value, fixed = TRUE)
  value <- gsub("\n", "\\n", value, fixed = TRUE)
  value <- gsub("\r", "\\r", value, fixed = TRUE)
  value <- gsub("\t", "\\t", value, fixed = TRUE)
  paste0('"', value, '"')
}

format_project_number <- function(value) {
  value <- as.numeric(value)
  if (length(value) != 1L || !is.finite(value)) {
    stop("CSV 설정 정보에 올바르지 않은 숫자가 있습니다")
  }
  format(value, scientific = FALSE, trim = TRUE, digits = 15)
}

serialize_project_metadata <- function(
    source_image, image_width, image_height, calibration, series) {
  if (anyDuplicated(series$name)) stop("그룹 명칭이 중복되어 저장할 수 없습니다")
  number <- format_project_number
  axis_point_line <- function(name) {
    point <- calibration$axis_points[[name]]
    coordinate <- if (startsWith(name, "x")) "pixel_x" else "pixel_y"
    sprintf(
      "  %s: {pixel: %s, value: %s}",
      name, number(point[[coordinate]]), number(point$value)
    )
  }

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
        quote_project_header(series$name[index]),
        number(series$marker[index]), quote_project_header(series$color[index]),
        number(series$size[index]), number(series$alpha[index])
      )
    }, character(1))
  } else {
    character()
  }

  c(
    paste0("format: ", quote_project_header(project_format)),
    "source_image:",
    paste0("  filename: ", quote_project_header(basename(source_image))),
    sprintf(
      "  size: {width: %s, height: %s}",
      number(image_width), number(image_height)
    ),
    "box_points:", corner_lines,
    "x_axis:",
    paste0("  name: ", quote_project_header(calibration$x$column)),
    paste0("  scale: ", calibration$x$scale),
    paste0("  position: ", calibration$x$position),
    axis_point_line("x1"), axis_point_line("x2"),
    "y_axis:",
    paste0("  name: ", quote_project_header(calibration$y$column)),
    paste0("  scale: ", calibration$y$scale),
    paste0("  position: ", calibration$y$position),
    axis_point_line("y1"), axis_point_line("y2"),
    if (length(style_lines)) {
      c("display_styles:", style_lines)
    } else {
      "display_styles: {}"
    }
  )
}

serialize_project_csv <- function(
    source_image, image_width, image_height, data, calibration, series) {
  metadata_lines <- serialize_project_metadata(
    source_image, image_width, image_height, calibration, series
  )
  body <- data
  group_rows <- match(body$series_id, series$id)
  if (anyNA(group_rows)) stop("포인트의 그룹 정보를 찾을 수 없습니다")
  body$group <- series$name[group_rows]
  values <- axis_values(body, calibration)
  body[[calibration$x$column]] <- values$x
  body[[calibration$y$column]] <- values$y
  body <- body[c(
    "point_id", "group", "pixel_x", "pixel_y",
    calibration$x$column, calibration$y$column
  )]
  csv_lines <- capture.output(
    utils::write.csv(body, row.names = FALSE, na = "")
  )
  c("# ---", paste0("# ", metadata_lines), "# ---", csv_lines)
}

atomic_replace <- function(path, write_temp) {
  target_dir <- normalizePath(dirname(path), mustWork = TRUE)
  target_path <- file.path(target_dir, basename(path))
  temp_path <- tempfile(
    pattern = paste0(".", basename(path), "-"),
    tmpdir = target_dir,
    fileext = ".tmp"
  )
  on.exit(unlink(temp_path), add = TRUE)

  write_temp(temp_path)
  if (!file.rename(temp_path, target_path)) {
    stop("임시 파일을 최종 CSV로 교체하지 못했습니다: ", target_path)
  }
  invisible(target_path)
}

atomic_write_lines <- function(lines, path) {
  atomic_replace(path, function(temp_path) {
    connection <- file(temp_path, open = "wb")
    tryCatch(
      writeLines(lines, connection, useBytes = TRUE),
      finally = close(connection)
    )
  })
}

read_file_bytes <- function(path) {
  size <- file.info(path)$size
  if (!is.finite(size)) stop("파일 크기를 확인할 수 없습니다: ", path)
  readBin(path, what = "raw", n = size)
}

file_matches_snapshot <- function(path, snapshot) {
  !is.null(snapshot) && file.exists(path) &&
    identical(read_file_bytes(path), snapshot)
}

atomic_write_bytes <- function(bytes, path) {
  atomic_replace(path, function(temp_path) {
    connection <- file(temp_path, open = "wb")
    tryCatch(
      writeBin(bytes, connection),
      finally = close(connection)
    )
  })
}

recovery_draft_version <- 1L

validate_recovery_draft <- function(draft) {
  required <- c(
    "version", "saved_at", "dataset", "image_width", "image_height",
    "data", "series", "calibration", "point_baseline_data",
    "series_baseline", "calibration_baseline", "point_dirty",
    "calibration_dirty", "selected_point_id", "active_edit_mode",
    "calibration_target", "calibration_point", "save_name_mode",
    "save_name_suffix", "save_name_custom", "initial_file_snapshot",
    "latest_saved_snapshot", "disk_file_snapshot"
  )
  if (!is.list(draft) || !all(required %in% names(draft)) ||
      !identical(draft$version, recovery_draft_version)) {
    stop("복구 draft 형식을 확인할 수 없습니다")
  }
  if (!is.list(draft$dataset) ||
      !all(c("key", "source_path", "load_path", "label") %in% names(draft$dataset)) ||
      length(draft$dataset$source_path) != 1L ||
      !nzchar(as.character(draft$dataset$source_path))) {
    stop("복구 draft의 작업 파일 정보를 확인할 수 없습니다")
  }
  if (!is.data.frame(draft$data) ||
      !all(c("point_id", "series_id", "pixel_x", "pixel_y") %in% names(draft$data)) ||
      !is.data.frame(draft$series) ||
      !all(c("id", "name", "marker", "color", "size", "alpha") %in% names(draft$series)) ||
      is.null(draft$calibration) ||
      !valid_projective_calibration(draft$calibration)) {
    stop("복구 draft의 포인트 또는 좌표 설정을 확인할 수 없습니다")
  }
  if (!isTRUE(draft$point_dirty) && !isTRUE(draft$calibration_dirty)) {
    stop("복구 draft에 저장되지 않은 변경사항이 없습니다")
  }
  if (!draft$active_edit_mode %in% c("point", "calibration")) {
    stop("복구 draft의 편집 모드를 확인할 수 없습니다")
  }
  draft
}

read_recovery_draft <- function(path) {
  if (!file.exists(path)) return(NULL)
  validate_recovery_draft(readRDS(path))
}

atomic_write_recovery_draft <- function(draft, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  atomic_replace(path, function(temp_path) {
    saveRDS(validate_recovery_draft(draft), temp_path, version = 3)
  })
}

append_history <- function(history, snapshot, limit = 50L) {
  history <- c(history, list(snapshot))
  if (length(history) > limit) tail(history, limit) else history
}

app_dirty_sessions <- new.env(parent = emptyenv())

box_point_display_labels <- c(
  origin = "원점", x_axis_end = "X 끝점",
  y_axis_end = "Y 끝점", xy_axis_end = "XY 끝점"
)

ui <- fluidPage(
  tags$head(
    tags$title("Tiny Plot Digitizer"),
    tags$style(HTML("
      body { background: #f7f7f5; color: #202124; }
      .container-fluid { padding: 12px 18px; }
      .app-header { margin: 0 0 10px; }
      .app-title { margin: 0; text-align: right; font-size: 20px; font-weight: 600; line-height: 1.2; }
      .app-subtitle { margin-top: 1px; text-align: right; font-size: 12px; line-height: 1.35; color: #555; }
      .app-version { text-align: right; font-size: 12px; line-height: 1.35; color: #777; }
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
      .folder-picker-toolbar { display: grid; grid-template-columns: 70px 70px minmax(0, 1fr); gap: 6px; align-items: center; margin-bottom: 8px; }
      .folder-picker-toolbar .btn { width: 100%; }
      .folder-picker-current { min-width: 0; height: 34px; padding: 6px 8px; overflow: hidden; border: 1px solid #ccc; border-radius: 4px; background: #f7f7f7; font-size: 12px; }
      .folder-picker-current .shiny-text-output { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
      .folder-picker-list { height: 412px; overflow-y: auto; border: 1px solid #ccc; border-radius: 4px; background: #fff; }
      .folder-picker-entry { display: flex; align-items: center; gap: 8px; width: 100%; min-height: 36px; padding: 7px 10px; overflow: hidden; border: 0; border-bottom: 1px solid #eee; border-radius: 0; background: #fff; text-align: left; }
      .folder-picker-entry:last-child { border-bottom: 0; }
      .folder-picker-entry:hover, .folder-picker-entry:focus { background: #eef5f2; outline: none; }
      .folder-picker-folder-icon { color: #f2c94c; font-size: 17px; }
      .folder-picker-entry-name { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
      .folder-picker-file { display: flex; align-items: center; gap: 8px; min-height: 34px; padding: 7px 10px; overflow: hidden; border-bottom: 1px solid #eee; background: #fafafa; color: #666; }
      .folder-picker-file-icon { color: #66809a; font-size: 15px; }
      .folder-picker-file-name { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
      .folder-picker-empty { padding: 12px; color: #777; text-align: center; }
      .move-button-row { display: grid; grid-template-columns: repeat(6, minmax(0, 1fr)); gap: 6px; margin: 6px 0 9px; }
      .calibration-move-button-row { grid-template-columns: repeat(6, minmax(0, 1fr)); }
      .move-button-row .btn { width: 100%; height: 34px; padding: 3px; font-size: 18px; border-radius: 4px; }
      .status-line { min-height: 22px; margin-top: 8px; font-size: 12px; }
      .status-line, .status-line .shiny-text-output { max-width: 100%; min-width: 0; white-space: normal; overflow-wrap: normal; word-break: break-all; }
      .point-values, .point-values .shiny-text-output { max-width: 100%; min-width: 0; overflow-wrap: anywhere; word-break: break-word; }
      .compact-control-row { display: grid; gap: 5px; align-items: center; margin-bottom: 9px; }
      .compact-control-row > *, .compact-control-row .shiny-input-container { min-width: 0; }
      .compact-control-row .shiny-input-container { width: 100% !important; margin: 0; }
      .compact-control-row > label { margin: 0; font-weight: 400; white-space: nowrap; }
      .compact-control-row input, .compact-control-row select { height: 34px; padding: 4px 6px; }
      .point-section-title { display: block; margin: 3px 0 5px; font-weight: 600; }
      .point-select-input .shiny-input-container { width: 100% !important; margin-bottom: 6px; }
      .point-action-row { display: grid; grid-template-columns: repeat(12, minmax(0, 1fr)); gap: 6px; margin-bottom: 10px; }
      .point-action-row #add_point, .point-action-row #change_point_series, .point-action-row #delete_point { grid-column: span 4; }
      .point-action-row #previous_series, .point-action-row #previous_point, .point-action-row #next_point, .point-action-row #next_series { grid-column: span 3; }
      .point-action-row .btn { width: 100%; height: 34px; padding: 5px 2px; border-radius: 4px; font-size: 12px; }
      #add_point.add-mode-active { color: #fff; background: #24483e; border-color: #19352f; }
      #add_point.add-mode-active:hover { background: #19352f; border-color: #10241f; }
      .movement-focus-target { outline: none; }
      .editor-tabs .nav-tabs > li > a:focus,
      .editor-tabs .nav-tabs > li > a:focus-visible {
        outline: none !important;
        box-shadow: none !important;
      }
      .editor-tabs > .tabbable > .tab-content > .tab-pane:focus,
      .editor-tabs > .tabbable > .tab-content > .tab-pane:focus-visible {
        outline: none !important;
        box-shadow: none !important;
      }
      .editor-tabs > .tabbable > .tab-content { display: grid; }
      .editor-tabs > .tabbable > .tab-content > .tab-pane {
        display: block;
        grid-area: 1 / 1;
        visibility: hidden;
        pointer-events: none;
      }
      .editor-tabs > .tabbable > .tab-content > .tab-pane.active {
        visibility: visible;
        pointer-events: auto;
      }
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
      .calibration-setting-title { margin: 8px 0 5px; font-weight: 600; }
      .calibration-point-group > .calibration-setting-title:first-child { margin-top: 3px; }
      .calibration-setting-table { margin-bottom: 9px; }
      .axis-name-row { display: grid; grid-template-columns: 30px minmax(0, 1fr); column-gap: 12px; align-items: center; min-height: 30px; margin-bottom: 1px; }
      .axis-name-row > *, .axis-name-row .form-group { min-width: 0; }
      .axis-name-row .axis-name-label { font-weight: 400; white-space: nowrap; }
      .axis-name-row .form-group { width: 100%; margin: 0; }
      .axis-name-row input { width: 100%; min-width: 0; height: 28px; padding: 3px 6px; }
      .axis-option-row { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); column-gap: 10px; align-items: center; min-height: 30px; margin: 1px 0 3px; }
      .axis-option-row > *, .axis-option-row .form-group { min-width: 0; }
      .axis-option-row .form-group { width: 100% !important; margin: 0; }
      .axis-option-row .shiny-options-group { display: grid; grid-template-columns: repeat(2, max-content); column-gap: 8px; justify-content: center; align-items: center; white-space: nowrap; }
      .axis-option-row .radio-inline { display: block; margin: 0; padding-left: 16px; font-size: 12px; }
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
      .move-point-option .form-control, .move-point-option .selectize-control { width: 100%; min-width: 0; }
      .move-step-option .shiny-options-group { text-align: right; white-space: nowrap; }
      .save-actions { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 6px; width: 100%; margin: 0; }
      .save-actions .btn { display: block; width: 100%; }
      .save-actions .btn + .btn { margin-top: 0; }
      .save-actions #save { grid-column: 1 / -1; }
      .save-actions #save_options, .save-actions #restore_saved, .save-actions #reload { padding-right: 4px; padding-left: 4px; font-size: 12px; }
      .save-option-file-info { margin-bottom: 20px; line-height: 1.65; }
      .save-option-file-info > div { display: flex; align-items: baseline; gap: 4px; }
      .save-option-file-info .save-option-label { flex: 0 0 auto; font-weight: 600; }
      .save-option-modal .save-name-mode-options { width: 100% !important; max-width: none; margin-bottom: 8px; }
      .save-name-option-row { display: flex; align-items: center; min-height: 34px; }
      .save-name-option-row > .radio { flex: 0 0 auto; margin: 0; }
      .save-name-option-row > .radio label { padding-top: 4px; padding-bottom: 4px; }
      .save-option-inline-input { flex: 0 0 auto; margin-left: 6px; }
      .save-option-inline-input .form-group { margin: 0; }
      .save-option-inline-input input { height: 28px; padding: 3px 6px; }
      .save-option-suffix-input { width: 158px; }
      .save-option-filename-box { width: 100%; margin-top: 10px; }
      .save-option-filename-box .form-group, .save-option-filename-box .shiny-input-container { width: 100% !important; max-width: none; margin: 0; }
      .save-option-filename-box input { width: 100%; height: 34px; padding: 4px 8px; }
      .save-option-filename-box input:disabled { background: #f3f3f3; color: #888; }
      .point-values { min-height: 10.8em; margin: -2px 0 -3px; font-family: Menlo, Consolas, monospace; font-size: 12px; line-height: 1.35; }
      .point-values .shiny-text-output { margin: 0; white-space: pre-wrap; }
      .plot-title { margin: 0 0 4px; font-size: 13px; font-weight: 600; }
      .plot-stack { position: relative; height: calc(100vh - 82px); min-height: 420px; }
      .plot-stack .shiny-plot-output { position: absolute; inset: 0; width: 100% !important; height: 100% !important; }
      #overview_image { z-index: 1; pointer-events: none; }
      #overview { z-index: 2; background: transparent; }
      .btn-primary { background: #2f5d50; border-color: #2f5d50; }
      .btn-primary:hover { background: #24483e; border-color: #24483e; }
    ")),
    tags$script(HTML("
      var unsavedChangesPending = false;

      Shiny.addCustomMessageHandler('set-unsaved-state', function(message) {
        unsavedChangesPending = Boolean(message.pending);
      });

      window.addEventListener('beforeunload', function(event) {
        if (!unsavedChangesPending) return;
        event.preventDefault();
        event.returnValue = '';
      });

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
        } else {
          element.value = '';
        }
      });

      Shiny.addCustomMessageHandler('select-point', function(message) {
        var element = document.getElementById('point');
        if (!element) return;
        var selected = message.selected == null ? '' : String(message.selected);
        if (element.value === selected) return;
        element.value = selected;
      });

      Shiny.addCustomMessageHandler('set-add-mode-state', function(message) {
        var button = document.getElementById('add_point');
        if (button) button.classList.toggle('add-mode-active', Boolean(message.active));
      });

      document.addEventListener('change', function(event) {
        if (event.target.id === 'point') {
          Shiny.setInputValue('point_user_selection', {
            value: event.target.value,
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
        }
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
        var editingField = tag === 'INPUT' || tag === 'SELECT' || tag === 'TEXTAREA' ||
          (activeElement && activeElement.isContentEditable);
        if (editingField) return;
        var pointDeleteModal = document.getElementById('point_delete_modal_marker');
        if (pointDeleteModal) {
          var deleteConfirm = event.code === 'KeyY';
          var deleteCancel = event.code === 'KeyN' || event.key === 'Escape';
          if (deleteConfirm || deleteCancel) {
            event.preventDefault();
            event.stopImmediatePropagation();
            var deleteModalButton = document.getElementById(
              deleteConfirm ? 'confirm_point_delete' : 'cancel_point_delete'
            );
            if (deleteModalButton) deleteModalButton.click();
          } else if (event.key === 'Delete' || event.key === 'Backspace') {
            event.preventDefault();
          }
          return;
        }
        var accelerator = event.metaKey || event.ctrlKey;
        var shortcutKey = event.key.toLowerCase();
        if (accelerator && (shortcutKey === 'z' || shortcutKey === 'y')) {
          event.preventDefault();
          event.stopImmediatePropagation();
          var redoRequested = shortcutKey === 'y' || (shortcutKey === 'z' && event.shiftKey);
          var historyButton = document.getElementById(redoRequested ? 'redo' : 'undo');
          if (historyButton) historyButton.click();
          return;
        }
        var directions = {ArrowLeft: 'left', ArrowRight: 'right', ArrowUp: 'up', ArrowDown: 'down'};
        if (directions[event.key]) {
          event.preventDefault();
          event.stopImmediatePropagation();
          Shiny.setInputValue('key_move', {
            direction: directions[event.key],
            start: !event.repeat,
            nonce: Date.now() + Math.random()
          }, {priority: 'event'});
          return;
        }
        var activeEditMode = document.querySelector(
          '.editor-tabs .nav-tabs li.active a'
        );
        var pointModeActive = activeEditMode &&
          activeEditMode.getAttribute('data-value') === 'point';
        if (pointModeActive && !accelerator && !event.altKey &&
            (event.key === 'Delete' || event.key === 'Backspace')) {
          event.preventDefault();
          event.stopImmediatePropagation();
          var deleteButton = document.getElementById('delete_point');
          if (deleteButton) deleteButton.click();
          return;
        }
        if (event.key === '[') {
          if (!pointModeActive) return;
          event.preventDefault();
          Shiny.setInputValue('key_point_nav', 'previous', {priority: 'event'});
          return;
        }
        if (event.key === ']') {
          if (!pointModeActive) return;
          event.preventDefault();
          Shiny.setInputValue('key_point_nav', 'next', {priority: 'event'});
          return;
        }
      }, true);

      function endKeyboardMovement() {
        Shiny.setInputValue('key_move_end', Date.now() + Math.random(), {priority: 'event'});
      }

      document.addEventListener('keyup', function(event) {
        if (!['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown'].includes(event.key)) return;
        endKeyboardMovement();
      }, true);

      window.addEventListener('blur', endKeyboardMovement);

      $(document).on('shown.bs.modal', '.modal', function() {
        if (!document.getElementById('point_delete_modal_marker')) return;
        var cancelButton = document.getElementById('cancel_point_delete');
        if (cancelButton) cancelButton.focus({preventScroll: true});
      });

      function focusActiveMovementTarget() {
        window.setTimeout(function() {
          var movementControls = document.getElementById('movement_controls');
          if (movementControls) movementControls.focus({preventScroll: true});
        }, 0);
      }

      document.addEventListener('click', function(event) {
        if (!event.target.closest('.editor-tabs .nav-tabs a')) return;
        focusActiveMovementTarget();
      });

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

      document.addEventListener('click', function(event) {
        var entry = event.target.closest('.folder-picker-entry');
        if (!entry) return;
        Shiny.setInputValue('folder_picker_open', {
          index: Number(entry.dataset.folderIndex),
          nonce: Date.now()
        }, {priority: 'event'});
      });
    "))
  ),
  fluidRow(
    class = "editor-layout",
    column(
      width = 3,
      class = "editor-column control-column",
      div(
        class = "control-panel",
        div(
          class = "app-header",
          h3(class = "app-title", "Tiny Plot Digitizer"),
          div(class = "app-version", "v2026.07")
        ),
        div(
          class = "project-source-group",
          div(class = "project-source-title", "작업 폴더"),
          div(
            class = "project-source-control-row",
            div(
              class = "selected-folder-box",
              div(class = "selected-folder-line", textOutput("folder_path", inline = TRUE))
            ),
            actionButton("folder", "선택", title = "작업 폴더를 선택하세요")
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
                actionButton("add_point", "연속입력",
                             title = "선택한 그룹에 포인트 연속 입력 시작"),
                actionButton("change_point_series", "그룹변경",
                             title = "선택한 포인트의 그룹 변경"),
                actionButton(
                  "delete_point", "제거",
                  title = "선택한 포인트 제거 (Delete/Backspace)"
                ),
                actionButton(
                  "previous_series", "이전그룹",
                  title = "이전 그룹의 첫 번째 포인트"
                ),
                actionButton(
                  "next_series", "다음그룹",
                  title = "다음 그룹의 첫 번째 포인트"
                ),
                actionButton(
                  "previous_point", "이전 [", title = "이전 포인트 ([)"
                ),
                actionButton(
                  "next_point", "다음 ]", title = "다음 포인트 (])"
                )
              ),
              div(class = "point-values", textOutput("point_values"))
            ),
            tabPanel(
              title = "좌표설정", value = "calibration",
              div(
                class = "calibration-point-group movement-focus-target",
                role = "radiogroup",
                tabindex = "-1",
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
                  class = "axis-name-row",
                  span(class = "axis-name-label", "X축"),
                  textInput("x_axis_name", NULL, value = "")
                ),
                div(
                  class = "axis-option-row",
                  radioButtons(
                    "x_axis_scale", NULL,
                    choices = c("선형" = "linear", "로그" = "log10"),
                    selected = "linear", inline = TRUE, width = "100%"
                  ),
                  radioButtons(
                    "x_axis_position", NULL,
                    choices = c("하단" = "bottom", "상단" = "top"),
                    selected = "bottom", inline = TRUE, width = "100%"
                  )
                ),
                div(
                  class = "shiny-options-group",
                  lapply(c("x1", "x2"), function(point_name) {
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
                ),
                div(
                  class = "axis-name-row",
                  span(class = "axis-name-label", "Y축"),
                  textInput("y_axis_name", NULL, value = "")
                ),
                div(
                  class = "axis-option-row",
                  radioButtons(
                    "y_axis_scale", NULL,
                    choices = c("선형" = "linear", "로그" = "log10"),
                    selected = "linear", inline = TRUE, width = "100%"
                  ),
                  radioButtons(
                    "y_axis_position", NULL,
                    choices = c("좌측" = "left", "우측" = "right"),
                    selected = "left", inline = TRUE, width = "100%"
                  )
                ),
                div(
                  class = "shiny-options-group",
                  lapply(c("y1", "y2"), function(point_name) {
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
              )
            )
          )
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
          actionButton("undo", "↺", title = "이전 변경 되돌리기 (⌘Z / Ctrl+Z)"),
          actionButton("redo", "↻", title = "되돌린 변경 다시 실행 (⇧⌘Z / Ctrl+Y)")
        ),
        div(
          class = "move-point-option move-step-option",
          radioButtons(
            "move_step", "이동 간격", choices = c(0.5, 1, 5, 10),
            selected = 0.5, inline = TRUE, width = "100%"
          )
        ),
        div(
          class = "move-point-option zoom-option",
          selectInput(
            "zoom", "확대 반경",
            choices = c(20, 40, 80), selected = 40, width = "100%"
          )
        ),
        tags$hr(class = "panel-divider"),
        div(
          class = "save-actions",
          actionButton("save", "파일 저장", class = "btn-primary"),
          actionButton("save_options", "다른이름 저장"),
          actionButton("restore_saved", "저장본 복귀"),
          actionButton("reload", "변경 초기화")
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
      plotOutput("zoom_plot", height = "430px", click = "zoom_click")
    )
  )
)

folder_path_key <- function(path) {
  if (identical(.Platform$OS.type, "windows")) tolower(path) else path
}

normalize_folder_in_root <- function(path, root) {
  root <- normalizePath(
    path.expand(root), winslash = .Platform$file.sep, mustWork = TRUE
  )
  path <- normalizePath(
    path.expand(path), winslash = .Platform$file.sep, mustWork = TRUE
  )
  if (!dir.exists(path)) stop("폴더를 찾을 수 없습니다: ", path)
  root_key <- folder_path_key(root)
  path_key <- folder_path_key(path)
  root_prefix <- paste0(root_key, .Platform$file.sep)
  if (!identical(path_key, root_key) && !startsWith(path_key, root_prefix)) {
    stop("홈 폴더 밖의 경로는 선택할 수 없습니다: ", path)
  }
  path
}

list_child_folders <- function(path, root) {
  path <- normalize_folder_in_root(path, root)
  entries <- list.files(
    path, all.files = FALSE, full.names = TRUE, no.. = TRUE
  )
  if (!length(entries)) {
    return(data.frame(name = character(), path = character()))
  }
  info <- file.info(entries)
  entries <- entries[!is.na(info$isdir) & info$isdir]
  rows <- lapply(entries, function(entry) {
    normalized <- tryCatch(
      normalize_folder_in_root(entry, root), error = function(error) NULL
    )
    if (is.null(normalized) || file.access(normalized, 4L) != 0L) return(NULL)
    data.frame(
      name = basename(entry), path = normalized, stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(name = character(), path = character()))
  }
  folders <- do.call(rbind, rows)
  folders <- folders[order(tolower(folders$name), folders$name), , drop = FALSE]
  rownames(folders) <- NULL
  folders
}

list_folder_files <- function(path, root) {
  path <- normalize_folder_in_root(path, root)
  entries <- list.files(
    path, pattern = "[.](png|csv)$", ignore.case = TRUE,
    all.files = FALSE, full.names = TRUE, no.. = TRUE
  )
  if (!length(entries)) {
    return(data.frame(name = character(), path = character()))
  }
  info <- file.info(entries)
  entries <- entries[!is.na(info$isdir) & !info$isdir & file.access(entries, 4L) == 0L]
  if (!length(entries)) {
    return(data.frame(name = character(), path = character()))
  }
  files <- data.frame(
    name = basename(entries), path = entries, stringsAsFactors = FALSE
  )
  files <- files[order(tolower(files$name), files$name), , drop = FALSE]
  rownames(files) <- NULL
  files
}

parent_folder_in_root <- function(path, root) {
  path <- normalize_folder_in_root(path, root)
  root <- normalize_folder_in_root(root, root)
  if (identical(folder_path_key(path), folder_path_key(root))) return(root)
  normalize_folder_in_root(dirname(path), root)
}

default_working_folder <- function(
  home_dir,
  development_folder = file.path(
    home_dir, "github", "R-RVI-2023", "data-raw", "2004_MRP79R1"
  )
) {
  if (dir.exists(development_folder)) {
    normalizePath(development_folder, mustWork = TRUE)
  } else {
    home_dir
  }
}

server <- function(input, output, session) {
  home_dir <- normalizePath("~", mustWork = TRUE)
  home_prefix <- paste0(home_dir, .Platform$file.sep)
  configured_draft <- trimws(Sys.getenv("DIGITIZER_DRAFT_FILE", ""))
  runtime_app_dir <- normalizePath(
    getOption("digitization.point.editor.app_dir", getwd()),
    mustWork = FALSE
  )
  bundled_app <- grepl("[.]app/Contents/Resources/app$", runtime_app_dir)
  recovery_dir <- if (identical(Sys.info()[["sysname"]], "Darwin")) {
    file.path(home_dir, "Library", "Application Support", "Tiny Plot Digitizer")
  } else {
    file.path(home_dir, ".tiny-plot-digitizer")
  }
  recovery_draft_file <- if (nzchar(configured_draft)) {
    path.expand(configured_draft)
  } else {
    file.path(
      recovery_dir,
      if (bundled_app) "recovery-draft.rds" else "development-recovery-draft.rds"
    )
  }
  configured_folder <- trimws(Sys.getenv("DIGITIZER_FOLDER", ""))
  initial_folder <- if (nzchar(configured_folder) && dir.exists(path.expand(configured_folder))) {
    normalizePath(path.expand(configured_folder), mustWork = TRUE)
  } else {
    default_working_folder(home_dir)
  }
  initial_folder <- tryCatch(
    normalize_folder_in_root(initial_folder, home_dir),
    error = function(error) {
      warning("DIGITIZER_FOLDER는 홈 폴더 안의 경로만 지정할 수 있습니다. 홈 폴더에서 시작합니다.")
      home_dir
    }
  )
  selected_folder <- reactiveVal(initial_folder)
  folder_picker_path <- reactiveVal(initial_folder)
  catalog <- reactiveVal(list())
  image_cache <- new.env(parent = emptyenv())
  image_cache$key <- NULL
  image_cache$value <- NULL
  rv <- reactiveValues(
    data = NULL, image_width = NULL, image_height = NULL,
    raster_matrix = NULL, dataset = NULL,
    calibration = NULL,
    point_baseline_data = NULL,
    calibration_baseline = NULL, series = NULL, series_baseline = NULL,
    pending_series_edit = NULL, pending_point_series_change = NULL,
    pending_point_delete = NULL,
    point_dirty = FALSE, calibration_dirty = FALSE,
    add_mode = FALSE, add_series = NULL,
    selected = NULL, status = "", pending_navigation = NULL,
    point_order_dirty = FALSE,
    movement_history_active = FALSE, movement_history_mode = NULL,
    updating_calibration_inputs = FALSE,
    calibration_target = NULL, calibration_point = NULL,
    active_edit_mode = "point", pending_edit_mode = NULL,
    updating_edit_mode = FALSE,
    save_name_mode = "current",
    save_name_suffix = "-digitized",
    save_name_custom = "",
    initial_file_snapshot = NULL,
    latest_saved_snapshot = NULL,
    disk_file_snapshot = NULL,
    point_history = list(), point_redo = list(),
    calibration_history = list(), calibration_redo = list()
  )

  recovery_draft_error <- NULL
  pending_recovery <- reactiveVal(tryCatch(
    read_recovery_draft(recovery_draft_file),
    error = function(error) {
      recovery_draft_error <<- conditionMessage(error)
      NULL
    }
  ))

  remove_recovery_draft <- function() {
    if (file.exists(recovery_draft_file)) unlink(recovery_draft_file)
    invisible()
  }

  load_source_image <- function(path) {
    normalized <- normalizePath(path, mustWork = TRUE)
    info <- file.info(normalized)
    cache_key <- c(
      path = normalized,
      size = as.character(info$size),
      modified = as.character(as.numeric(info$mtime))
    )
    if (identical(image_cache$key, cache_key)) return(image_cache$value)

    source_image <- png::readPNG(normalized, native = TRUE)
    value <- list(
      width = dim(source_image)[2],
      height = dim(source_image)[1],
      raster_matrix = source_image
    )
    image_cache$key <- cache_key
    image_cache$value <- value
    value
  }

  mode_changes_pending <- function(mode) {
    if (identical(mode, "point")) return(isTRUE(rv$point_dirty))
    if (identical(mode, "calibration")) return(isTRUE(rv$calibration_dirty))
    stop("알 수 없는 편집 모드입니다: ", mode)
  }

  unsaved_changes_pending <- function() {
    mode_changes_pending("point") || mode_changes_pending("calibration")
  }

  dirty_state_file <- trimws(Sys.getenv("DIGITIZER_DIRTY_STATE_FILE", ""))
  dirty_session_key <- paste(
    Sys.getpid(), format(Sys.time(), "%Y%m%d%H%M%OS6"),
    sample.int(.Machine$integer.max, 1), sep = "-"
  )
  update_app_dirty_state <- function(pending = NULL, remove = FALSE) {
    if (remove) {
      if (exists(dirty_session_key, envir = app_dirty_sessions, inherits = FALSE)) {
        rm(list = dirty_session_key, envir = app_dirty_sessions)
      }
    } else {
      assign(dirty_session_key, isTRUE(pending), envir = app_dirty_sessions)
    }
    if (nzchar(dirty_state_file)) {
      session_states <- as.list(app_dirty_sessions, all.names = TRUE)
      app_pending <- length(session_states) && any(vapply(session_states, isTRUE, logical(1)))
      try(
        atomic_write_lines(if (app_pending) "dirty" else "clean", dirty_state_file),
        silent = TRUE
      )
    }
    invisible()
  }

  observe({
    pending <- unsaved_changes_pending()
    session$sendCustomMessage(
      "set-unsaved-state", list(pending = pending)
    )
    update_app_dirty_state(pending)
  })
  session$onSessionEnded(function() update_app_dirty_state(remove = TRUE))

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
      rv$point_history <- list()
      rv$point_redo <- list()
    } else if (identical(mode, "calibration")) {
      rv$calibration_baseline <- rv$calibration
      rv$calibration_dirty <- FALSE
      rv$calibration_history <- list()
      rv$calibration_redo <- list()
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

  csv_name_stem <- function(value, label) {
    value <- trimws(as.character(value))
    if (!nzchar(value)) stop(label, "을 입력하세요")
    if (grepl("[/\\\\]", value)) stop(label, "에는 폴더 경로를 입력할 수 없습니다")
    value <- sub("\\.csv$", "", value, ignore.case = TRUE)
    if (!nzchar(value)) stop(label, "을 입력하세요")
    value
  }

  save_path_for_values <- function(dataset, mode, suffix, custom) {
    if (is.null(dataset) || is.null(dataset$source_path)) return(NULL)
    source_dir <- dirname(dataset$source_path)
    source_stem <- tools::file_path_sans_ext(basename(dataset$source_path))
    if (identical(mode, "current")) {
      if (!is.null(dataset$load_path)) return(dataset$load_path)
      return(file.path(source_dir, paste0(source_stem, ".csv")))
    }
    if (identical(mode, "suffix")) {
      suffix <- csv_name_stem(suffix, "접미사")
      return(file.path(source_dir, paste0(source_stem, suffix, ".csv")))
    }
    if (identical(mode, "custom")) {
      custom <- csv_name_stem(custom, "파일이름")
      return(file.path(source_dir, paste0(custom, ".csv")))
    }
    stop("저장 옵션을 선택하세요")
  }

  save_path_for_dataset <- function(dataset) {
    save_path_for_values(
      dataset, "current",
      rv$save_name_suffix, rv$save_name_custom
    )
  }

  display_path <- function(path) {
    normalized <- normalizePath(path, mustWork = FALSE)
    if (identical(normalized, home_dir)) return("~")
    if (startsWith(normalized, home_prefix)) {
      return(file.path("~", substring(normalized, nchar(home_prefix) + 1L)))
    }
    normalized
  }

  update_save_name_inputs <- function(dataset) {
    rv$save_name_mode <- "current"
    rv$save_name_suffix <- "-digitized"
    rv$save_name_custom <- tools::file_path_sans_ext(basename(
      if (is.null(dataset$load_path)) dataset$source_path else dataset$load_path
    ))
    invisible()
  }

  save_changes <- function(target_path = NULL, force = FALSE) {
    if (is.null(rv$data) || is.null(rv$dataset)) return(NULL)
    if (!force && !unsaved_changes_pending()) return(NULL)
    save_path <- if (is.null(target_path)) {
      tryCatch(
        save_path_for_dataset(rv$dataset),
        error = function(error) {
          rv$status <- conditionMessage(error)
          NULL
        }
      )
    } else {
      target_path
    }
    if (is.null(save_path)) return(NULL)
    current_path <- rv$dataset$load_path
    same_target <- !is.null(current_path) && identical(
      normalizePath(save_path, mustWork = FALSE),
      normalizePath(current_path, mustWork = FALSE)
    )
    if (!same_target && file.exists(save_path)) {
      rv$status <- paste0("같은 이름의 CSV가 이미 있습니다: ", basename(save_path))
      return(NULL)
    }
    if (same_target) {
      matches_snapshot <- tryCatch(
        file_matches_snapshot(save_path, rv$disk_file_snapshot),
        error = function(error) {
          rv$status <- paste0("저장 전 파일 확인 실패: ", conditionMessage(error))
          NA
        }
      )
      if (is.na(matches_snapshot)) return(NULL)
      if (!matches_snapshot) {
        rv$status <- paste0(
          "저장하지 못했습니다: ", basename(save_path),
          " 파일이 앱 밖에서 변경되었거나 삭제되었습니다. ",
          "다시 불러오거나 다른이름으로 저장하세요."
        )
        return(NULL)
      }
    }
    finalize_point_order()
    project_lines <- serialize_project_csv(
      rv$dataset$source_path, rv$image_width, rv$image_height,
      rv$data, rv$calibration, rv$series
    )
    saved <- tryCatch(
      {
        atomic_write_lines(project_lines, save_path)
        TRUE
      },
      error = function(error) {
        rv$status <- paste0("저장 실패: ", conditionMessage(error))
        FALSE
      }
    )
    if (!saved) return(NULL)
    saved_path <- normalizePath(save_path, mustWork = TRUE)
    saved_snapshot <- read_file_bytes(saved_path)
    rv$latest_saved_snapshot <- saved_snapshot
    rv$disk_file_snapshot <- saved_snapshot
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
    update_project_input(current_catalog, selected = saved_path, freeze = TRUE)
    capture_all_baselines()
    remove_recovery_draft()
    message <- paste("저장됨:", display_path(save_path))
    rv$status <- message
    message
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
    rv$pending_series_edit <- NULL
    rv$pending_edit_mode <- NULL
    rv$point_dirty <- FALSE
    rv$calibration_dirty <- FALSE
    rv$point_history <- list()
    rv$point_redo <- list()
    rv$calibration_history <- list()
    rv$calibration_redo <- list()
    rv$add_mode <- FALSE
    rv$add_series <- NULL
    rv$selected <- NULL
    rv$point_order_dirty <- FALSE
    rv$movement_history_active <- FALSE
    rv$movement_history_mode <- NULL
    rv$initial_file_snapshot <- NULL
    rv$latest_saved_snapshot <- NULL
    rv$disk_file_snapshot <- NULL
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
    if (!length(projects)) selected <- ""
    update_project_input(projects, selected = selected)
  }

  set_folder_picker_path <- function(path) {
    path <- normalize_folder_in_root(path, home_dir)
    folder_picker_path(path)
    invisible(path)
  }

  folder_picker_entries <- reactive({
    list_child_folders(folder_picker_path(), home_dir)
  })

  folder_picker_files <- reactive({
    list_folder_files(folder_picker_path(), home_dir)
  })

  restore_project_selection <- function(projects) {
    selected <- if (
      !is.null(rv$dataset) && rv$dataset$key %in% names(projects)
    ) rv$dataset$key else NULL
    update_project_input(projects, selected = selected, freeze = TRUE)
    invisible()
  }

  load_dataset_safely <- function(key) {
    tryCatch(
      {
        load_dataset(key)
        TRUE
      },
      error = function(error) {
        rv$status <- paste0("파일을 불러오지 못했습니다: ", conditionMessage(error))
        showNotification(rv$status, type = "error")
        FALSE
      }
    )
  }

  perform_navigation <- function(navigation) {
    kind <- navigation$kind
    target <- navigation$target
    if (identical(kind, "folder")) {
      target <- tryCatch(
        normalize_folder_in_root(target, home_dir),
        error = function(error) NULL
      )
      if (is.null(target)) {
        rv$status <- "선택한 작업 폴더를 찾을 수 없습니다"
        set_folder_picker_path(selected_folder())
        return(invisible(FALSE))
      }
      selected_folder(target)
      set_folder_picker_path(target)
      update_catalog(target)
      return(invisible(TRUE))
    }

    if (identical(kind, "new_project")) {
      if (!file.exists(target)) {
        rv$status <- "선택한 원본 이미지를 찾을 수 없습니다"
        return(invisible(FALSE))
      }
      key <- paste0("new::", target)
      dataset <- list(
        key = key,
        source_path = target,
        load_path = NULL,
        label = paste0("[신규] ", basename(target))
      )
      previous_projects <- catalog()
      projects <- previous_projects
      if (!is.null(rv$dataset) && is.null(rv$dataset$load_path) &&
          rv$dataset$key %in% names(projects)) {
        projects[[rv$dataset$key]] <- NULL
      }
      projects[[key]] <- dataset
      catalog(projects)
      if (!load_dataset_safely(key)) {
        catalog(previous_projects)
        restore_project_selection(previous_projects)
        return(invisible(FALSE))
      }
      update_project_input(projects, selected = key, freeze = TRUE)
      return(invisible(TRUE))
    }

    if (identical(kind, "dataset")) {
      projects <- catalog()
      if (!target %in% names(projects)) {
        rv$status <- "선택한 작업 파일을 찾을 수 없습니다"
        if (!is.null(rv$dataset)) {
          update_project_input(projects, selected = rv$dataset$key, freeze = TRUE)
        }
        return(invisible(FALSE))
      }
      previous_key <- if (is.null(rv$dataset)) NULL else rv$dataset$key
      previous_was_new <- !is.null(rv$dataset) && is.null(rv$dataset$load_path)
      if (!load_dataset_safely(target)) {
        restore_project_selection(projects)
        return(invisible(FALSE))
      }
      if (previous_was_new && !identical(previous_key, target) &&
          previous_key %in% names(projects)) {
        projects[[previous_key]] <- NULL
        catalog(projects)
      }
      update_project_input(projects, selected = target, freeze = TRUE)
      return(invisible(TRUE))
    }

    stop("알 수 없는 작업 전환입니다: ", kind)
  }

  request_navigation <- function(kind, target, label) {
    navigation <- list(kind = kind, target = target, label = label)
    if (!unsaved_changes_pending()) {
      perform_navigation(navigation)
      return(invisible())
    }

    rv$pending_navigation <- navigation
    if (identical(kind, "dataset") && !is.null(rv$dataset)) {
      update_project_input(catalog(), selected = rv$dataset$key, freeze = TRUE)
    }
    if (identical(kind, "folder")) {
      set_folder_picker_path(selected_folder())
    }
    showModal(modalDialog(
      title = "저장되지 않은 변경사항",
      paste0("현재 변경사항을 처리한 뒤 '", label, "'(으)로 이동합니다."),
      footer = tagList(
        actionButton("cancel_navigation", "취소"),
        actionButton("discard_navigation", "저장하지 않음"),
        actionButton("save_navigation", "저장 후 이동", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
    invisible()
  }

  output$folder_path <- renderText({
    basename(selected_folder())
  })

  output$folder_picker_current <- renderText({
    display_path(folder_picker_path())
  })

  output$folder_picker_entries <- renderUI({
    folders <- folder_picker_entries()
    files <- folder_picker_files()
    folder_items <- if (!nrow(folders)) {
      div(class = "folder-picker-empty", "하위 폴더가 없습니다")
    } else lapply(seq_len(nrow(folders)), function(index) {
      tags$button(
        type = "button",
        class = "folder-picker-entry",
        `data-folder-index` = index,
        tags$span(
          class = paste(
            "glyphicon glyphicon-folder-close", "folder-picker-folder-icon"
          ),
          `aria-hidden` = "true"
        ),
        tags$span(class = "folder-picker-entry-name", folders$name[[index]]),
        title = folders$name[[index]]
      )
    })
    file_items <- if (!nrow(files)) {
      div(class = "folder-picker-empty", "PNG/CSV 파일이 없습니다")
    } else lapply(seq_len(nrow(files)), function(index) {
      div(
        class = "folder-picker-file",
        tags$span(
          class = paste("glyphicon glyphicon-file", "folder-picker-file-icon"),
          `aria-hidden` = "true"
        ),
        tags$span(class = "folder-picker-file-name", files$name[[index]]),
        title = files$name[[index]]
      )
    })
    tagList(folder_items, file_items)
  })

  show_folder_picker <- function() {
    set_folder_picker_path(selected_folder())
    showModal(modalDialog(
      title = "작업 폴더 선택",
      div(
        class = "folder-picker-toolbar",
        actionButton("folder_picker_home", "홈"),
        actionButton("folder_picker_up", "상위"),
        div(
          class = "folder-picker-current",
          textOutput("folder_picker_current", inline = TRUE)
        )
      ),
      div(class = "folder-picker-list", uiOutput("folder_picker_entries")),
      footer = tagList(
        modalButton("취소"),
        actionButton(
          "confirm_folder_picker", "현재 폴더 선택", class = "btn-primary"
        )
      ),
      easyClose = FALSE
    ))
  }

  observeEvent(TRUE, {
    update_catalog(initial_folder)
    set_folder_picker_path(initial_folder)
    if (!is.null(recovery_draft_error)) {
      remove_recovery_draft()
      showNotification(
        paste0("복구 draft를 읽지 못해 폐기했습니다: ", recovery_draft_error),
        type = "warning"
      )
    } else if (!is.null(pending_recovery())) {
      show_recovery_modal(pending_recovery())
    }
  }, once = TRUE)

  observeEvent(input$folder, {
    show_folder_picker()
  }, ignoreInit = TRUE)

  observeEvent(input$folder_picker_home, {
    set_folder_picker_path(home_dir)
  }, ignoreInit = TRUE)

  observeEvent(input$folder_picker_up, {
    tryCatch(
      set_folder_picker_path(
        parent_folder_in_root(folder_picker_path(), home_dir)
      ),
      error = function(error) {
        showNotification(conditionMessage(error), type = "warning")
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$folder_picker_open, {
    index <- suppressWarnings(as.integer(input$folder_picker_open$index))
    entries <- folder_picker_entries()
    if (length(index) != 1L || is.na(index) || index < 1L ||
        index > nrow(entries)) {
      showNotification("선택한 폴더를 열 수 없습니다", type = "warning")
      return()
    }
    tryCatch(
      set_folder_picker_path(entries$path[[index]]),
      error = function(error) {
        showNotification("선택한 폴더를 열 수 없습니다", type = "warning")
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_folder_picker, {
    target <- tryCatch(
      normalize_folder_in_root(folder_picker_path(), home_dir),
      error = function(error) {
        showNotification(conditionMessage(error), type = "warning")
        NULL
      }
    )
    if (is.null(target)) return()
    removeModal()
    if (identical(target, selected_folder())) return()
    request_navigation("folder", target, basename(target))
  }, ignoreInit = TRUE)

  observeEvent(input$new_project, {
    images <- discover_images(selected_folder())
    if (!length(images)) {
      showModal(modalDialog(
        title = "신규 CSV 파일 제작",
        "현재 작업 폴더에 PNG 이미지가 없습니다.",
        footer = modalButton("닫기"),
        easyClose = TRUE
      ))
      return()
    }
    labels <- vapply(images, `[[`, character(1), "label")
    showModal(modalDialog(
      title = "신규 CSV 파일 제작",
      selectInput(
        "new_project_image", "이미지 파일 선택",
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
    removeModal()
    request_navigation("new_project", source_path, basename(source_path))
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

  point_label <- function(row, values = NULL) {
    value_row <- row
    if (is.null(values)) {
      values <- axis_values(rv$data[row, , drop = FALSE], rv$calibration)
      value_row <- 1L
    }
    style_row <- match(rv$data$series_id[row], rv$series$id)
    if (is.na(style_row)) {
      series_label <- paste("그룹", rv$data$series_id[row])
    } else {
      series_label <- rv$series$name[style_row]
    }
    group_rows <- which(rv$data$series_id == rv$data$series_id[row])
    group_number <- match(row, group_rows)
    x_value <- format(round(values$x[value_row], 3), scientific = FALSE, trim = TRUE)
    y_value <- format(round(values$y[value_row], 3), scientific = FALSE, trim = TRUE)
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

  update_point_selection <- function() {
    selected_id <- if (!is.null(rv$data) && !is.null(rv$selected) && nrow(rv$data)) {
      as.character(rv$data$point_id[rv$selected])
    } else {
      NULL
    }
    session$sendCustomMessage("select-point", list(selected = selected_id))
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
      "x_axis_name", "y_axis_name",
      "x_axis_scale", "y_axis_scale", "x_axis_position", "y_axis_position"
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
    updateRadioButtons(session, "x_axis_scale", selected = calibration$x$scale)
    updateRadioButtons(session, "y_axis_scale", selected = calibration$y$scale)
    updateRadioButtons(session, "x_axis_position", selected = calibration$x$position)
    updateRadioButtons(session, "y_axis_position", selected = calibration$y$position)
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
      label = if (active) "연속입력 종료" else "연속입력",
      icon = NULL
    )
    session$sendCustomMessage("set-add-mode-state", list(active = isTRUE(active)))
  }

  point_state_snapshot <- function() {
    selected_series_id <- if (length(input$setting_series)) {
      suppressWarnings(as.integer(input$setting_series))
    } else {
      NULL
    }
    list(
      data = rv$data,
      series = rv$series,
      selected_point_id = selected_point_id(),
      selected_series_id = selected_series_id,
      point_dirty = isTRUE(rv$point_dirty),
      point_order_dirty = isTRUE(rv$point_order_dirty)
    )
  }

  begin_movement_history <- function() {
    rv$movement_history_active <- TRUE
    rv$movement_history_mode <- NULL
    invisible()
  }

  end_movement_history <- function() {
    rv$movement_history_active <- FALSE
    rv$movement_history_mode <- NULL
    invisible()
  }

  should_record_movement_history <- function(mode) {
    if (!isTRUE(rv$movement_history_active)) return(TRUE)
    if (identical(rv$movement_history_mode, mode)) return(FALSE)
    rv$movement_history_mode <- mode
    TRUE
  }

  remember_point_change <- function() {
    if (!should_record_movement_history("point")) return(invisible())
    rv$point_history <- append_history(rv$point_history, point_state_snapshot())
    rv$point_redo <- list()
    invisible()
  }

  restore_point_state <- function(snapshot) {
    rv$data <- snapshot$data
    rv$series <- snapshot$series
    rv$point_dirty <- isTRUE(snapshot$point_dirty)
    rv$point_order_dirty <- isTRUE(snapshot$point_order_dirty)
    rv$selected <- if (!is.null(snapshot$selected_point_id) && nrow(rv$data)) {
      match(as.integer(snapshot$selected_point_id), rv$data$point_id)
    } else {
      NULL
    }
    if (length(rv$selected) && is.na(rv$selected)) rv$selected <- NULL
    set_add_mode(FALSE)
    refresh_controls(rv$selected)
    selected_series <- snapshot$selected_series_id
    if (is.null(selected_series) || is.na(series_row(selected_series))) {
      selected_series <- if (!is.null(rv$selected) && nrow(rv$data)) {
        rv$data$series_id[rv$selected]
      } else {
        NULL
      }
    }
    refresh_series_choices(
      if (is.null(selected_series)) NULL else as.character(selected_series)
    )
    invisible()
  }

  calibration_target_key <- function() {
    if (is.null(rv$calibration_target) || is.null(rv$calibration_point)) return(NULL)
    paste(rv$calibration_target, rv$calibration_point, sep = ":")
  }

  calibration_state_snapshot <- function() {
    list(
      target = calibration_target_key(),
      calibration = rv$calibration,
      calibration_dirty = isTRUE(rv$calibration_dirty)
    )
  }

  remember_calibration_change <- function() {
    if (!should_record_movement_history("calibration")) return(invisible())
    rv$calibration_history <- append_history(
      rv$calibration_history, calibration_state_snapshot()
    )
    rv$calibration_redo <- list()
    invisible()
  }

  restore_calibration_state <- function(snapshot) {
    rv$calibration <- snapshot$calibration
    rv$calibration_dirty <- isTRUE(snapshot$calibration_dirty)
    target <- if (is.null(snapshot$target)) "" else snapshot$target
    selection <- strsplit(target, ":", fixed = TRUE)[[1]]
    if (length(selection) == 2L) {
      rv$calibration_target <- selection[1]
      rv$calibration_point <- selection[2]
    } else {
      rv$calibration_target <- NULL
      rv$calibration_point <- NULL
    }
    update_calibration_inputs()
    update_point_choices()
    invisible()
  }

  selected_point_id <- function() {
    if (is.null(rv$data) || !nrow(rv$data) || is.null(rv$selected) ||
        rv$selected < 1L || rv$selected > nrow(rv$data)) return(NULL)
    as.integer(rv$data$point_id[rv$selected])
  }

  current_recovery_draft <- function() {
    if (is.null(rv$dataset) || !unsaved_changes_pending()) return(NULL)
    list(
      version = recovery_draft_version,
      saved_at = as.numeric(Sys.time()),
      dataset = rv$dataset,
      image_width = rv$image_width,
      image_height = rv$image_height,
      data = rv$data,
      series = rv$series,
      calibration = rv$calibration,
      point_baseline_data = rv$point_baseline_data,
      series_baseline = rv$series_baseline,
      calibration_baseline = rv$calibration_baseline,
      point_dirty = isTRUE(rv$point_dirty),
      calibration_dirty = isTRUE(rv$calibration_dirty),
      point_order_dirty = isTRUE(rv$point_order_dirty),
      selected_point_id = selected_point_id(),
      active_edit_mode = rv$active_edit_mode,
      calibration_target = rv$calibration_target,
      calibration_point = rv$calibration_point,
      save_name_mode = rv$save_name_mode,
      save_name_suffix = rv$save_name_suffix,
      save_name_custom = rv$save_name_custom,
      initial_file_snapshot = rv$initial_file_snapshot,
      latest_saved_snapshot = rv$latest_saved_snapshot,
      disk_file_snapshot = rv$disk_file_snapshot
    )
  }

  save_recovery_draft <- function() {
    draft <- current_recovery_draft()
    if (is.null(draft)) {
      remove_recovery_draft()
    } else {
      atomic_write_recovery_draft(draft, recovery_draft_file)
    }
    invisible(draft)
  }

  restore_recovery_draft <- function(draft) {
    draft <- validate_recovery_draft(draft)
    source_path <- normalizePath(draft$dataset$source_path, mustWork = TRUE)
    source_image <- load_source_image(source_path)
    if (source_image$width != draft$image_width ||
        source_image$height != draft$image_height) {
      stop("복구 draft와 원본 이미지의 크기가 다릅니다")
    }

    folder_path <- dirname(source_path)
    normalize_folder_in_root(folder_path, home_dir)
    dataset <- draft$dataset
    dataset$source_path <- source_path
    if (!is.null(dataset$load_path)) {
      dataset$load_path <- normalizePath(dataset$load_path, mustWork = FALSE)
      dataset$key <- dataset$load_path
      dataset$label <- basename(dataset$load_path)
    } else {
      dataset$key <- paste0("new::", source_path)
      dataset$label <- paste0("[복구] ", basename(source_path))
    }

    projects <- discover_projects(folder_path)
    projects[[dataset$key]] <- dataset
    selected_folder(folder_path)
    catalog(projects)
    set_folder_picker_path(folder_path)
    update_project_input(projects, selected = dataset$key, freeze = TRUE)

    rv$data <- draft$data
    rv$image_width <- source_image$width
    rv$image_height <- source_image$height
    rv$raster_matrix <- source_image$raster_matrix
    rv$dataset <- dataset
    rv$calibration <- draft$calibration
    rv$series <- draft$series
    rv$point_baseline_data <- draft$point_baseline_data
    rv$series_baseline <- draft$series_baseline
    rv$calibration_baseline <- draft$calibration_baseline
    rv$point_dirty <- isTRUE(draft$point_dirty)
    rv$calibration_dirty <- isTRUE(draft$calibration_dirty)
    rv$point_order_dirty <- if (is.null(draft$point_order_dirty)) {
      TRUE
    } else {
      isTRUE(draft$point_order_dirty)
    }
    rv$point_history <- list()
    rv$point_redo <- list()
    rv$calibration_history <- list()
    rv$calibration_redo <- list()
    rv$pending_series_edit <- NULL
    rv$pending_navigation <- NULL
    rv$calibration_target <- draft$calibration_target
    rv$calibration_point <- draft$calibration_point
    rv$active_edit_mode <- draft$active_edit_mode
    rv$pending_edit_mode <- NULL
    rv$save_name_mode <- draft$save_name_mode
    rv$save_name_suffix <- draft$save_name_suffix
    rv$save_name_custom <- draft$save_name_custom
    rv$initial_file_snapshot <- draft$initial_file_snapshot
    rv$latest_saved_snapshot <- draft$latest_saved_snapshot
    rv$disk_file_snapshot <- draft$disk_file_snapshot

    selected_id <- suppressWarnings(as.integer(draft$selected_point_id))
    rv$selected <- if (length(selected_id) && nrow(rv$data)) {
      match(selected_id, rv$data$point_id)
    } else {
      NULL
    }
    if (length(rv$selected) && is.na(rv$selected)) rv$selected <- NULL
    set_add_mode(FALSE)
    refresh_controls(rv$selected)
    update_calibration_inputs()
    update_edit_mode_input(rv$active_edit_mode)
    rv$status <- "비정상 종료 전에 저장된 작업을 복구했습니다"
    invisible(TRUE)
  }

  show_recovery_modal <- function(draft) {
    saved_time <- format(
      as.POSIXct(draft$saved_at, origin = "1970-01-01"),
      "%Y-%m-%d %H:%M:%S"
    )
    showModal(modalDialog(
      title = "복구 가능한 작업",
      paste0(
        "'", basename(draft$dataset$source_path), "' 작업이 ", saved_time,
        "에 임시 저장되었습니다. 복구하시겠습니까?"
      ),
      footer = tagList(
        actionButton("discard_recovery", "폐기", class = "btn-warning"),
        actionButton("restore_recovery", "복구", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
  }

  observeEvent(input$discard_recovery, {
    removeModal()
    pending_recovery(NULL)
    remove_recovery_draft()
    rv$status <- "임시 저장된 복구 작업을 폐기했습니다"
  }, ignoreInit = TRUE)

  observeEvent(input$restore_recovery, {
    draft <- pending_recovery()
    req(draft)
    removeModal()
    restored <- tryCatch(
      restore_recovery_draft(draft),
      error = function(error) {
        showNotification(
          paste0("작업 복구 실패: ", conditionMessage(error)), type = "error"
        )
        FALSE
      }
    )
    if (restored) {
      pending_recovery(NULL)
      save_recovery_draft()
    } else {
      show_recovery_modal(draft)
    }
  }, ignoreInit = TRUE)

  recovery_state <- debounce(reactive(current_recovery_draft()), 500)
  observe({
    draft <- recovery_state()
    if (!is.null(pending_recovery())) return()
    tryCatch(
      {
        if (is.null(draft)) remove_recovery_draft() else {
          atomic_write_recovery_draft(draft, recovery_draft_file)
        }
      },
      error = function(error) {
        rv$status <- paste0("복구 draft 저장 실패: ", conditionMessage(error))
      }
    )
  })

  sort_points <- function(selected_point_id = NULL) {
    if (!nrow(rv$data)) {
      rv$selected <- NULL
      rv$point_order_dirty <- FALSE
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
    rv$point_order_dirty <- FALSE
    invisible()
  }

  ensure_point_order <- function(selected_point_id = NULL) {
    if (!isTRUE(rv$point_order_dirty)) return(FALSE)
    sort_points(selected_point_id)
    TRUE
  }

  finalize_point_order <- function(point_id = selected_point_id()) {
    ensure_point_order(point_id)
    update_point_choices()
    invisible()
  }

  edit_mode_label <- function(mode) {
    if (identical(mode, "point")) return("포인트")
    if (identical(mode, "calibration")) return("좌표설정")
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
      rv$point_order_dirty <- FALSE
      capture_mode_baseline("point")
      set_add_mode(FALSE)
      refresh_controls(if (nrow(rv$data)) min(selected, nrow(rv$data)) else NULL)
    } else {
      stop("알 수 없는 편집 모드입니다: ", mode)
    }
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
    if (identical(mode, "calibration")) set_add_mode(FALSE)
    if (update_input) update_edit_mode_input(mode)
    invisible()
  }

  load_dataset <- function(key, reset_file_snapshots = TRUE) {
    req(key, key %in% names(catalog()))
    dataset <- catalog()[[key]]
    source_image <- load_source_image(dataset$source_path)
    image_height <- source_image$height
    image_width <- source_image$width

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
        stop("CSV에서 원본 이미지 정보를 읽을 수 없습니다: ", project_path)
      }
      if (source_metadata$width != image_width || source_metadata$height != image_height) {
        stop(sprintf(
          "원본 이미지 크기가 CSV에 저장된 정보와 다릅니다: 저장 %dx%d, 현재 %dx%d",
          source_metadata$width, source_metadata$height, image_width, image_height
        ))
      }
      saved_data <- read.csv(project_path, comment.char = "#", check.names = FALSE)
      required <- c("group", "pixel_x", "pixel_y")
      if (!all(required %in% names(saved_data))) {
        stop("Tiny Plot Digitizer 형식의 CSV가 아닙니다: ", project_path)
      }
      saved_calibration <- parse_projective_calibration(metadata, names(saved_data))
      if (is.null(saved_calibration)) {
        stop("축 설정을 읽을 수 없습니다: ", project_path)
      }
      persisted_series <- series_from_metadata(metadata$display_styles)
      data <- saved_data[required]
      group_rows <- match(as.character(data$group), persisted_series$name)
      if (anyNA(group_rows)) {
        stop("CSV 데이터와 그룹 정보가 일치하지 않습니다")
      }
      data$group <- persisted_series$id[group_rows]
      names(data)[names(data) == "group"] <- "series_id"
      data$point_id <- if ("point_id" %in% names(saved_data)) {
        point_ids <- suppressWarnings(as.numeric(saved_data$point_id))
        if (any(!is.finite(point_ids)) || any(point_ids != round(point_ids))) {
          stop("포인트 번호는 중복되지 않는 양의 정수여야 합니다")
        }
        as.integer(point_ids)
      } else {
        seq_len(nrow(data))
      }
      data <- data[c("point_id", "series_id", "pixel_x", "pixel_y")]
      data$series_id <- as.integer(data$series_id)
      data$pixel_x <- as.numeric(data$pixel_x)
      data$pixel_y <- as.numeric(data$pixel_y)
      if (anyNA(data$point_id) || any(data$point_id < 1L) || anyDuplicated(data$point_id)) {
        stop("포인트 번호는 중복되지 않는 양의 정수여야 합니다")
      }
      if (nrow(data) && any(!data$series_id %in% persisted_series$id)) {
        stop("CSV 데이터와 그룹 정보가 일치하지 않습니다")
      }
      saved_series <- restore_default_groups(persisted_series)
      calibration <- saved_calibration
      series <- saved_series
      loaded_project <- TRUE
    }

    disk_snapshot <- if (loaded_project) read_file_bytes(project_path) else NULL

    rv$data <- data
    rv$image_width <- image_width
    rv$image_height <- image_height
    rv$raster_matrix <- source_image$raster_matrix
    rv$dataset <- dataset
    rv$calibration <- calibration
    rv$series <- series
    sort_points()
    capture_all_baselines()
    rv$pending_edit_mode <- NULL
    rv$calibration_target <- NULL
    rv$calibration_point <- NULL
    set_add_mode(FALSE)
    update_save_name_inputs(dataset)
    rv$disk_file_snapshot <- disk_snapshot
    if (reset_file_snapshots) {
      rv$initial_file_snapshot <- disk_snapshot
      rv$latest_saved_snapshot <- disk_snapshot
    }
    rv$status <- if (loaded_project) {
      paste("CSV 불러옴:", basename(project_path))
    } else {
      "신규 CSV 파일 작성 중"
    }
    refresh_controls(if (nrow(rv$data)) 1L else NULL)
    update_calibration_inputs()
  }

  observeEvent(input$dataset, {
    requested_key <- input$dataset
    req(nzchar(requested_key), requested_key %in% names(catalog()))
    if (!is.null(rv$dataset) && identical(requested_key, rv$dataset$key)) return()
    request_navigation(
      "dataset", requested_key, catalog()[[requested_key]]$label
    )
  })

  observeEvent(input$cancel_navigation, {
    removeModal()
    rv$pending_navigation <- NULL
  })

  observeEvent(input$discard_navigation, {
    req(rv$pending_navigation)
    navigation <- rv$pending_navigation
    removeModal()
    rv$pending_navigation <- NULL
    perform_navigation(navigation)
  })

  observeEvent(input$save_navigation, {
    req(rv$pending_navigation)
    navigation <- rv$pending_navigation
    saved <- save_changes()
    if (is.null(saved)) {
      showNotification(rv$status, type = "error")
      return()
    }
    removeModal()
    rv$pending_navigation <- NULL
    perform_navigation(navigation)
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
      paste0(current_label, " 변경사항을 저장하시겠습니까?"),
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
    next_label <- if (identical(next_mode, "point")) "포인트로" else "좌표설정으로"
    rv$status <- paste0(
      edit_mode_label(current_mode), " 변경사항을 저장하지 않고 ",
      next_label, " 전환했습니다"
    )
  })

  observeEvent(input$save_mode_switch, {
    req(rv$pending_edit_mode)
    current_mode <- rv$active_edit_mode
    next_mode <- rv$pending_edit_mode
    req(!is.null(save_changes()))
    removeModal()
    rv$pending_edit_mode <- NULL
    activate_edit_mode(next_mode, update_input = TRUE)
    next_label <- if (identical(next_mode, "point")) "포인트로" else "좌표설정으로"
    rv$status <- paste0(
      edit_mode_label(current_mode), " 변경사항을 저장하고 ",
      next_label, " 전환했습니다"
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
      remember_point_change()
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
      remember_point_change()
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
    remember_point_change()
    rv$series <- rv$series[-row, , drop = FALSE]
    selected_id <- if (nrow(rv$series)) {
      as.character(rv$series$id[min(row, nrow(rv$series))])
    } else {
      NULL
    }
    mark_mode_changed("point", paste0(removed_name, " 그룹이 제거되었습니다"))
    refresh_series_choices(selected_id)
  }, ignoreInit = TRUE)

  output$point_series_change_impact <- renderUI({
    pending <- rv$pending_point_series_change
    req(pending, input$point_series_target)
    target_id <- suppressWarnings(as.integer(input$point_series_target))
    target_row <- series_row(target_id)
    req(!is.na(target_row), target_id != pending$source_series_id)
    source_count <- sum(rv$data$series_id == pending$source_series_id)
    target_count <- sum(rv$data$series_id == target_id)
    tags$div(
      class = "well well-sm",
      tags$div(sprintf(
        "%s: %d개 → %d개",
        pending$source_name, source_count, source_count - 1L
      )),
      tags$div(sprintf(
        "%s: %d개 → %d개",
        rv$series$name[target_row], target_count, target_count + 1L
      ))
    )
  })

  observeEvent(input$change_point_series, {
    if (rv$add_mode) {
      rv$status <- "연속입력을 종료한 후 그룹을 변경하세요"
      return()
    }
    if (is.null(rv$selected) || is.null(rv$data) || !nrow(rv$data)) {
      rv$status <- "그룹을 변경할 포인트를 선택하세요"
      return()
    }
    row <- selected_row()
    source_series_id <- rv$data$series_id[row]
    source_row <- series_row(source_series_id)
    target_choices <- series_choices()
    target_choices <- target_choices[unname(target_choices) != as.character(source_series_id)]
    if (is.na(source_row) || !length(target_choices)) {
      rv$pending_point_series_change <- NULL
      showModal(modalDialog(
        title = "포인트 그룹변경",
        "변경할 다른 그룹이 없습니다. 그룹을 먼저 추가하세요.",
        footer = modalButton("확인"),
        easyClose = TRUE
      ))
      return()
    }
    group_rows <- which(rv$data$series_id == source_series_id)
    group_number <- match(row, group_rows)
    rv$pending_point_series_change <- list(
      point_id = rv$data$point_id[row],
      point_number = sprintf("%d-%d", rv$data$point_id[row], group_number),
      source_series_id = source_series_id,
      source_name = rv$series$name[source_row]
    )
    showModal(modalDialog(
      title = "포인트 그룹변경",
      tags$p(tags$strong("선택 포인트: "), rv$pending_point_series_change$point_number),
      tags$p(tags$strong("현재 그룹: "), rv$pending_point_series_change$source_name),
      selectInput(
        "point_series_target", "변경할 그룹",
        choices = target_choices, selectize = FALSE, width = "100%"
      ),
      uiOutput("point_series_change_impact"),
      tags$div(
        class = "alert alert-warning",
        "그룹을 변경하면 포인트 목록 순서와 그룹별 번호가 다시 계산됩니다."
      ),
      footer = tagList(
        actionButton("cancel_point_series_change", "취소"),
        actionButton(
          "confirm_point_series_change", "그룹 변경", class = "btn-primary"
        )
      ),
      easyClose = FALSE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_point_series_change, {
    removeModal()
    rv$pending_point_series_change <- NULL
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_point_series_change, {
    pending <- rv$pending_point_series_change
    if (is.null(pending)) {
      removeModal()
      return()
    }
    point_row <- match(as.integer(pending$point_id), rv$data$point_id)
    target_id <- suppressWarnings(as.integer(input$point_series_target))
    target_row <- series_row(target_id)
    if (is.na(point_row) || is.na(target_row) ||
        identical(target_id, rv$data$series_id[point_row])) {
      showNotification("변경할 포인트와 그룹을 확인하세요", type = "warning")
      return()
    }
    source_row <- series_row(rv$data$series_id[point_row])
    if (is.na(source_row)) {
      showNotification("현재 포인트의 그룹을 확인할 수 없습니다", type = "warning")
      return()
    }
    source_name <- rv$series$name[source_row]
    target_name <- rv$series$name[target_row]
    remember_point_change()
    rv$data$series_id[point_row] <- target_id
    sort_points(pending$point_id)
    mark_mode_changed(
      "point",
      sprintf(
        "포인트 %s의 그룹을 %s → %s로 변경했습니다",
        pending$point_number, source_name, target_name
      )
    )
    refresh_controls(rv$selected)
    refresh_series_choices(as.character(target_id))
    removeModal()
    rv$pending_point_series_change <- NULL
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

  change_axis_scale <- function(axis, new_scale) {
    req(rv$calibration, rv$calibration$type == "projective")
    if (rv$updating_calibration_inputs) return()
    new_scale <- as.character(new_scale)
    if (length(new_scale) != 1L || !new_scale %in% c("linear", "log10")) return()

    current_scale <- rv$calibration[[axis]]$scale
    if (identical(new_scale, current_scale)) return()
    axis_points <- paste0(axis, c("1", "2"))
    values <- vapply(
      axis_points,
      function(point_name) as.numeric(rv$calibration$axis_points[[point_name]]$value),
      numeric(1)
    )
    if (identical(new_scale, "log10") && any(values <= 0)) {
      updateRadioButtons(session, paste0(axis, "_axis_scale"), selected = current_scale)
      rv$status <- paste0(toupper(axis), "축 로그 값은 모두 0보다 커야 합니다")
      return()
    }

    calibration <- rv$calibration
    calibration[[axis]]$scale <- new_scale
    calibration <- rebuild_calibration_ranges(calibration)
    if (is.null(calibration)) {
      updateRadioButtons(session, paste0(axis, "_axis_scale"), selected = current_scale)
      rv$status <- "축 값과 픽셀 위치를 확인하세요"
      return()
    }
    apply_calibration_change(
      calibration,
      paste0(toupper(axis), "축 눈금이 ", if (new_scale == "log10") "로그" else "선형", "으로 변경되었습니다")
    )
    update_calibration_inputs()
  }

  change_axis_position <- function(axis, new_position) {
    req(rv$calibration, rv$calibration$type == "projective")
    if (rv$updating_calibration_inputs) return()
    allowed <- if (axis == "x") c("bottom", "top") else c("left", "right")
    new_position <- as.character(new_position)
    if (length(new_position) != 1L || !new_position %in% allowed) return()

    current_position <- rv$calibration[[axis]]$position
    if (identical(new_position, current_position)) return()
    calibration <- rv$calibration
    calibration[[axis]]$position <- new_position
    calibration <- rebuild_calibration_ranges(calibration)
    if (is.null(calibration)) {
      updateRadioButtons(session, paste0(axis, "_axis_position"), selected = current_position)
      rv$status <- "축 값과 픽셀 위치를 확인하세요"
      return()
    }
    position_label <- if (axis == "x") {
      if (new_position == "top") "상단" else "하단"
    } else {
      if (new_position == "right") "우측" else "좌측"
    }
    apply_calibration_change(
      calibration,
      paste0(toupper(axis), "축 위치가 ", position_label, "으로 변경되었습니다")
    )
    update_calibration_inputs()
  }

  observeEvent(input$x_axis_scale, {
    change_axis_scale("x", input$x_axis_scale)
  }, ignoreInit = TRUE)

  observeEvent(input$y_axis_scale, {
    change_axis_scale("y", input$y_axis_scale)
  }, ignoreInit = TRUE)

  observeEvent(input$x_axis_position, {
    change_axis_position("x", input$x_axis_position)
  }, ignoreInit = TRUE)

  observeEvent(input$y_axis_position, {
    change_axis_position("y", input$y_axis_position)
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
      rv$status <- paste0(toupper(point_name), " 픽셀 위치에 숫자를 입력하세요")
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
      rv$status <- "축 설정점은 박스 설정 범위 안에 있어야 합니다"
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
    apply_calibration_change(calibration, "축 설정점의 픽셀 위치가 변경되었습니다")
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
    if (identical(calibration[[axis_name]]$scale, "log10") && value <= 0) {
      updateNumericInput(session, value_input, value = current_value)
      rv$status <- paste0(toupper(axis_name), "축 로그 값은 0보다 커야 합니다")
      return()
    }
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
    apply_calibration_change(calibration, "축 설정점의 값이 변경되었습니다")
    update_calibration_inputs()
  }, ignoreInit = TRUE)

  selected_row <- reactive({
    req(rv$data, rv$selected)
    validate(need(nrow(rv$data) > 0, "포인트를 선택하세요"))
    row <- as.integer(rv$selected)
    validate(need(row >= 1 && row <= nrow(rv$data), "포인트를 선택하세요"))
    row
  })

  select_point <- function(row, rebuild_choices = FALSE) {
    row <- max(1L, min(as.integer(row), nrow(rv$data)))
    rv$selected <- row
    if (rebuild_choices) update_point_choices() else update_point_selection()
    group_id <- as.character(rv$data$series_id[row])
    if (!identical(as.character(input$setting_series), group_id)) {
      refresh_series_choices(group_id)
    }
    rv$status <- unsaved_status()
  }

  select_point_id <- function(point_id, finalize_order = TRUE) {
    point_id <- as.integer(point_id)
    rebuild_choices <- finalize_order && ensure_point_order(point_id)
    row <- match(point_id, rv$data$point_id)
    if (!is.na(row)) select_point(row, rebuild_choices = rebuild_choices)
  }

  navigate_point <- function(direction) {
    current_id <- selected_point_id()
    rebuild_choices <- ensure_point_order(current_id)
    row <- selected_row()
    next_row <- if (direction == "previous") max(1, row - 1) else min(nrow(rv$data), row + 1)
    select_point(next_row, rebuild_choices = rebuild_choices)
  }

  navigate_series <- function(direction) {
    if (is.null(rv$selected) || is.null(rv$data) || !nrow(rv$data)) {
      rv$status <- "이동할 포인트를 선택하세요"
      return(invisible(FALSE))
    }
    current_id <- selected_point_id()
    rebuild_choices <- ensure_point_order(current_id)
    row <- selected_row()
    current_series_position <- match(rv$data$series_id[row], rv$series$id)
    if (is.na(current_series_position)) {
      if (rebuild_choices) update_point_choices()
      rv$status <- "현재 포인트의 그룹을 확인할 수 없습니다"
      return(invisible(FALSE))
    }
    candidate_positions <- if (identical(direction, "previous")) {
      if (current_series_position > 1L) {
        seq.int(current_series_position - 1L, 1L)
      } else {
        integer()
      }
    } else {
      if (current_series_position < nrow(rv$series)) {
        seq.int(current_series_position + 1L, nrow(rv$series))
      } else {
        integer()
      }
    }
    for (position in candidate_positions) {
      target_rows <- which(rv$data$series_id == rv$series$id[position])
      if (length(target_rows)) {
        select_point(target_rows[1], rebuild_choices = rebuild_choices)
        return(invisible(TRUE))
      }
    }
    if (rebuild_choices) update_point_choices()
    direction_label <- if (identical(direction, "previous")) "이전" else "다음"
    rv$status <- paste0(direction_label, " 그룹에 포인트가 없습니다")
    invisible(FALSE)
  }

  observeEvent(input$previous_series, navigate_series("previous"), ignoreInit = TRUE)
  observeEvent(input$previous_point, navigate_point("previous"), ignoreInit = TRUE)
  observeEvent(input$next_point, navigate_point("next"), ignoreInit = TRUE)
  observeEvent(input$next_series, navigate_series("next"), ignoreInit = TRUE)
  observeEvent(input$key_point_nav, {
    if (!active_mode_is("point")) return()
    navigate_point(input$key_point_nav)
  })

  observeEvent(input$point_user_selection, {
    req(rv$data, input$point_user_selection$value)
    point_id <- as.integer(input$point_user_selection$value)
    if (identical(point_id, selected_point_id())) return()
    select_point_id(point_id)
  }, ignoreInit = TRUE)

  move_selected <- function(direction) {
    row <- selected_row()
    data <- rv$data
    width <- rv$image_width
    height <- rv$image_height
    step <- as.numeric(input$move_step)

    if (direction == "left") data$pixel_x[row] <- max(0, data$pixel_x[row] - step)
    if (direction == "right") data$pixel_x[row] <- min(width, data$pixel_x[row] + step)
    if (direction == "up") data$pixel_y[row] <- max(0, data$pixel_y[row] - step)
    if (direction == "down") data$pixel_y[row] <- min(height, data$pixel_y[row] + step)

    if (identical(data$pixel_x[row], rv$data$pixel_x[row]) &&
        identical(data$pixel_y[row], rv$data$pixel_y[row])) return(invisible(FALSE))
    remember_point_change()
    rv$data <- data
    rv$point_order_dirty <- TRUE
    mark_mode_changed("point")
    update_point_label(row)
    invisible(TRUE)
  }

  set_selected_point_position <- function(
    pixel_x, pixel_y, status = "포인트 위치가 변경되었습니다"
  ) {
    row <- selected_row()
    pixel_x <- round_pixel_coordinate(max(0, min(rv$image_width, pixel_x)))
    pixel_y <- round_pixel_coordinate(max(0, min(rv$image_height, pixel_y)))
    if (identical(pixel_x, rv$data$pixel_x[row]) &&
        identical(pixel_y, rv$data$pixel_y[row])) return(invisible(FALSE))
    remember_point_change()
    rv$data$pixel_x[row] <- pixel_x
    rv$data$pixel_y[row] <- pixel_y
    rv$point_order_dirty <- TRUE
    mark_mode_changed("point", status)
    update_point_label(row)
    invisible(TRUE)
  }

  apply_calibration_change <- function(calibration, status) {
    if (isTRUE(all.equal(calibration, rv$calibration, check.attributes = FALSE))) {
      return(FALSE)
    }
    remember_calibration_change()
    rv$calibration <- calibration
    rv$point_order_dirty <- TRUE
    mark_mode_changed("calibration", status)
    update_point_choices()
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
      rv$status <- "네 모서리가 교차하지 않도록 박스를 설정하세요"
      return(FALSE)
    }
    calibration <- rebuild_calibration_ranges(calibration)
    if (is.null(calibration)) {
      rv$status <- "축 설정점의 위치와 값을 확인하세요"
      return(FALSE)
    }
    changed <- apply_calibration_change(calibration, "박스 설정이 변경되었습니다")
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
    changed <- apply_calibration_change(calibration, "축 설정점이 변경되었습니다")
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
    step <- as.numeric(input$move_step)
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
    step <- as.numeric(input$move_step)
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

  set_selected_calibration_position <- function(pixel_x, pixel_y) {
    req(rv$calibration$type == "projective")
    pixel_x <- round_pixel_coordinate(max(0, min(rv$image_width, pixel_x)))
    pixel_y <- round_pixel_coordinate(max(0, min(rv$image_height, pixel_y)))
    if (identical(rv$calibration_target, "axis")) {
      req(rv$calibration_point)
      fraction <- project_to_axis_edge(
        pixel_x, pixel_y,
        axis_edge(rv$calibration, rv$calibration_point)
      )
      return(set_calibration_axis_fraction(rv$calibration_point, fraction))
    }
    req(identical(rv$calibration_target, "box"), rv$calibration_point)
    set_calibration_box_point(rv$calibration_point, pixel_x, pixel_y)
  }

  observeEvent(input$left, {
    end_movement_history()
    move_target("left")
  })
  observeEvent(input$right, {
    end_movement_history()
    move_target("right")
  })
  observeEvent(input$up, {
    end_movement_history()
    move_target("up")
  })
  observeEvent(input$down, {
    end_movement_history()
    move_target("down")
  })

  observeEvent(input$key_move, {
    direction <- as.character(input$key_move$direction)
    req(direction %in% c("left", "right", "up", "down"))
    if (isTRUE(input$key_move$start)) begin_movement_history()
    move_target(direction)
  })

  observeEvent(input$key_move_end, end_movement_history())

  undo_target <- function() {
    end_movement_history()
    if (active_mode_is("calibration")) {
      if (!length(rv$calibration_history)) {
        rv$status <- "되돌릴 좌표설정 변경이 없습니다"
        return(invisible(FALSE))
      }
      index <- length(rv$calibration_history)
      snapshot <- rv$calibration_history[[index]]
      rv$calibration_history <- if (index == 1L) {
        list()
      } else {
        rv$calibration_history[-index]
      }
      rv$calibration_redo <- append_history(
        rv$calibration_redo, calibration_state_snapshot()
      )
      restore_calibration_state(snapshot)
      rv$status <- "좌표설정 변경을 되돌렸습니다"
      return(invisible(TRUE))
    }

    if (!length(rv$point_history)) {
      rv$status <- "되돌릴 포인트 또는 그룹 변경이 없습니다"
      return(invisible(FALSE))
    }
    index <- length(rv$point_history)
    snapshot <- rv$point_history[[index]]
    rv$point_history <- if (index == 1L) list() else rv$point_history[-index]
    rv$point_redo <- append_history(rv$point_redo, point_state_snapshot())
    restore_point_state(snapshot)
    rv$status <- "포인트 또는 그룹 변경을 되돌렸습니다"
    invisible(TRUE)
  }

  redo_target <- function() {
    end_movement_history()
    if (active_mode_is("calibration")) {
      if (!length(rv$calibration_redo)) {
        rv$status <- "다시 실행할 좌표설정 변경이 없습니다"
        return(invisible(FALSE))
      }
      index <- length(rv$calibration_redo)
      snapshot <- rv$calibration_redo[[index]]
      rv$calibration_redo <- if (index == 1L) list() else rv$calibration_redo[-index]
      rv$calibration_history <- append_history(
        rv$calibration_history, calibration_state_snapshot()
      )
      restore_calibration_state(snapshot)
      rv$status <- "좌표설정 변경을 다시 실행했습니다"
      return(invisible(TRUE))
    }

    if (!length(rv$point_redo)) {
      rv$status <- "다시 실행할 포인트 또는 그룹 변경이 없습니다"
      return(invisible(FALSE))
    }
    index <- length(rv$point_redo)
    snapshot <- rv$point_redo[[index]]
    rv$point_redo <- if (index == 1L) list() else rv$point_redo[-index]
    rv$point_history <- append_history(rv$point_history, point_state_snapshot())
    restore_point_state(snapshot)
    rv$status <- "포인트 또는 그룹 변경을 다시 실행했습니다"
    invisible(TRUE)
  }

  observeEvent(input$undo, undo_target())
  observeEvent(input$redo, redo_target())

  restore_file_snapshot <- function(snapshot, status) {
    req(rv$dataset, rv$dataset$load_path, !is.null(snapshot))
    project_path <- rv$dataset$load_path
    project_key <- rv$dataset$key
    restored <- tryCatch(
      {
        atomic_write_bytes(snapshot, project_path)
        load_dataset(project_key, reset_file_snapshots = FALSE)
        TRUE
      },
      error = function(error) {
        rv$status <- paste0("파일 복원 실패: ", conditionMessage(error))
        FALSE
      }
    )
    if (restored) rv$status <- status
    restored
  }

  observeEvent(input$restore_saved, {
    req(rv$dataset)
    if (is.null(rv$dataset$load_path) || is.null(rv$latest_saved_snapshot)) {
      showModal(modalDialog(
        title = "저장본 복귀",
        "아직 파일로 저장된 상태가 없습니다.",
        footer = modalButton("닫기"),
        easyClose = TRUE
      ))
      return()
    }
    showModal(modalDialog(
      title = "저장본 복귀",
      "마지막 저장본으로 복귀하시겠습니까? 저장되지 않은 변경은 모두 사라집니다.",
      footer = tagList(
        modalButton("취소"),
        actionButton("confirm_restore_saved", "복귀", class = "btn-warning")
      ),
      easyClose = FALSE
    ))
  })

  observeEvent(input$confirm_restore_saved, {
    removeModal()
    restore_file_snapshot(
      rv$latest_saved_snapshot,
      "마지막 저장본으로 복귀했습니다"
    )
  })

  observeEvent(input$reload, {
    req(rv$dataset)
    if (is.null(rv$dataset$load_path) || is.null(rv$initial_file_snapshot)) {
      showModal(modalDialog(
        title = "변경 초기화",
        "신규 파일에는 처음 불러온 CSV가 없어 변경 초기화를 사용할 수 없습니다.",
        footer = modalButton("닫기"),
        easyClose = TRUE
      ))
      return()
    }
    showModal(modalDialog(
      title = "변경 초기화",
      paste0(
        "처음 불러온 CSV로 초기화하시겠습니까? ",
        "현재 파일을 최초 상태로 덮어쓰며 이후 저장 내용과 저장되지 않은 변경이 모두 사라집니다."
      ),
      footer = tagList(
        modalButton("취소"),
        actionButton("confirm_reload", "초기화", class = "btn-danger")
      ),
      easyClose = FALSE
    ))
  })

  observeEvent(input$confirm_reload, {
    removeModal()
    restore_file_snapshot(
      rv$initial_file_snapshot,
      "처음 불러온 상태로 초기화했습니다"
    )
  })

  toggle_add_point <- function() {
    req(rv$data)
    if (rv$add_mode) {
      point_id <- selected_point_id()
      set_add_mode(FALSE)
      ensure_point_order(point_id)
      refresh_controls(rv$selected)
      rv$status <- unsaved_status()
    } else {
      if (!length(input$setting_series) || is.na(series_row(input$setting_series))) {
        rv$status <- "포인트를 추가할 그룹을 선택하세요"
        return()
      }
      showModal(modalDialog(
        title = "연속입력 시작",
        "추가할 포인트의 그룹을 확인해 주세요.",
        selectInput(
          "add_point_series", "포인트 그룹",
          choices = series_choices(), selected = input$setting_series,
          selectize = FALSE, width = "100%"
        ),
        footer = tagList(
          modalButton("취소"),
          actionButton("confirm_add_point", "연속입력 시작", class = "btn-primary")
        ),
        easyClose = FALSE
      ))
    }
  }

  observeEvent(input$add_point, toggle_add_point(), ignoreInit = TRUE)

  observeEvent(input$confirm_add_point, {
    series_id <- suppressWarnings(as.integer(input$add_point_series))
    if (is.na(series_row(series_id))) {
      removeModal()
      rv$status <- "포인트를 추가할 그룹을 선택하세요"
      return()
    }
    updateSelectInput(session, "setting_series", selected = as.character(series_id))
    set_add_mode(TRUE, series_id)
    removeModal()
    rv$status <- "원본 이미지를 클릭하여 포인트를 연속으로 입력하세요"
  }, ignoreInit = TRUE)

  observeEvent(input$delete_point, {
    if (is.null(rv$selected) || !nrow(rv$data)) {
      rv$status <- "제거할 포인트를 선택하세요"
      return()
    }
    row <- selected_row()
    point_series_row <- series_row(rv$data$series_id[row])
    group_name <- if (is.na(point_series_row)) {
      paste("그룹", rv$data$series_id[row])
    } else {
      rv$series$name[point_series_row]
    }
    group_number <- match(row, which(rv$data$series_id == rv$data$series_id[row]))
    values <- axis_values(rv$data[row, , drop = FALSE], rv$calibration)
    rv$pending_point_delete <- list(
      point_id = rv$data$point_id[row],
      point_number = sprintf("%d-%d", rv$data$point_id[row], group_number),
      group_name = group_name,
      x_name = rv$calibration$x$column,
      x_value = format(round(values$x[1], 3), scientific = FALSE, trim = TRUE),
      y_name = rv$calibration$y$column,
      y_value = format(round(values$y[1], 3), scientific = FALSE, trim = TRUE)
    )
    showModal(modalDialog(
      title = "포인트 제거",
      tags$div(id = "point_delete_modal_marker"),
      tags$p(tags$strong("포인트: "), rv$pending_point_delete$point_number),
      tags$p(tags$strong("그룹: "), rv$pending_point_delete$group_name),
      tags$p(
        tags$strong(paste0(rv$pending_point_delete$x_name, ": ")),
        rv$pending_point_delete$x_value
      ),
      tags$p(
        tags$strong(paste0(rv$pending_point_delete$y_name, ": ")),
        rv$pending_point_delete$y_value
      ),
      tags$div(
        class = "alert alert-warning",
        "이 포인트를 제거하시겠습니까? 이 작업은 실행 취소할 수 있습니다."
      ),
      footer = tagList(
        actionButton("cancel_point_delete", "N 취소"),
        actionButton("confirm_point_delete", "Y 제거", class = "btn-danger")
      ),
      easyClose = FALSE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_point_delete, {
    removeModal()
    rv$pending_point_delete <- NULL
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_point_delete, {
    pending <- rv$pending_point_delete
    if (is.null(pending)) {
      removeModal()
      return()
    }
    row <- match(as.integer(pending$point_id), rv$data$point_id)
    if (is.na(row)) {
      removeModal()
      rv$pending_point_delete <- NULL
      rv$status <- "제거할 포인트를 찾을 수 없습니다"
      return()
    }
    remember_point_change()
    rv$data <- rv$data[-row, , drop = FALSE]
    next_point_id <- if (nrow(rv$data)) {
      rv$data$point_id[min(row, nrow(rv$data))]
    } else {
      NULL
    }
    sort_points(next_point_id)
    mark_mode_changed(
      "point", paste0("포인트 ", pending$point_number, "이(가) 제거되었습니다")
    )
    set_add_mode(FALSE)
    refresh_controls(rv$selected)
    removeModal()
    rv$pending_point_delete <- NULL
  }, ignoreInit = TRUE)

  observeEvent(input$overview_click, {
    req(rv$data)
    if (active_mode_is("calibration")) {
      set_selected_calibration_position(input$overview_click$x, input$overview_click$y)
      return()
    }

    if (rv$add_mode) {
      series_id <- as.integer(rv$add_series)
      req(!is.na(series_row(series_id)))
      x <- round_pixel_coordinate(max(0, min(rv$image_width, input$overview_click$x)))
      y <- round_pixel_coordinate(max(0, min(rv$image_height, input$overview_click$y)))
      remember_point_change()
      point_id <- if (nrow(rv$data)) max(rv$data$point_id) + 1L else 1L
      new_row <- data.frame(
        point_id = point_id, series_id = series_id,
        pixel_x = x, pixel_y = y
      )
      rv$data <- rbind(rv$data, new_row)
      rv$selected <- nrow(rv$data)
      rv$point_order_dirty <- TRUE
      mark_mode_changed(
        "point", "새 포인트가 추가되었습니다. 계속 추가하거나 연속입력 종료를 누르세요"
      )
      refresh_controls(rv$selected)
      return()
    }

    if (!nrow(rv$data)) {
      rv$status <- "연속입력 버튼을 눌러 포인트를 추가하세요"
      return()
    }
    distance <- (rv$data$pixel_x - input$overview_click$x)^2 +
      (rv$data$pixel_y - input$overview_click$y)^2
    row <- which.min(distance)
    select_point_id(rv$data$point_id[row])
  })

  observeEvent(input$zoom_click, {
    req(rv$data)
    if (active_mode_is("calibration")) {
      set_selected_calibration_position(input$zoom_click$x, input$zoom_click$y)
      return()
    }
    if (rv$add_mode) {
      rv$status <- "연속입력은 원본 이미지에서 진행하세요"
      return()
    }
    set_selected_point_position(
      input$zoom_click$x, input$zoom_click$y,
      "확대 화면에서 포인트 위치를 변경했습니다"
    )
  }, ignoreInit = TRUE)

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
    crop <- crop_raster(
      rv$raster_matrix,
      (y_top + 1L):y_bottom,
      (x_left + 1L):x_right
    )
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
    row <- selected_row()
    calibration <- rv$calibration
    values <- axis_values(rv$data[row, , drop = FALSE], calibration)
    point_series_row <- match(rv$data$series_id[row], rv$series$id)
    group_name <- if (is.na(point_series_row)) "알 수 없음" else rv$series$name[point_series_row]
    group_number <- match(row, which(rv$data$series_id == rv$data$series_id[row]))
    sprintf(
      "번호: %d-%d\n그룹: %s\n%s: %s\n%s: %s\npixel: (%s, %s)",
      rv$data$point_id[row], group_number,
      group_name,
      calibration$x$column, format(values$x[1], digits = 7),
      calibration$y$column, format(values$y[1], digits = 7),
      format_pixel_coordinate(rv$data$pixel_x[row]),
      format_pixel_coordinate(rv$data$pixel_y[row])
    )
  })

  output$detail_title <- renderText({
    if (!active_mode_is("calibration")) {
      if (is.null(rv$selected)) return("포인트 미선택")
      return("선택한 포인트")
    }
    if (is.null(rv$calibration_target)) return("설정점 미선택")
    if (identical(rv$calibration_target, "box")) "선택한 박스 설정점" else "선택한 축 설정점"
  })

  output$save_name_resolved_box <- renderUI({
    req(rv$dataset, input$save_name_mode_modal)
    req(!identical(input$save_name_mode_modal, "custom"))
    suffix <- if (length(input$save_name_suffix_modal)) {
      input$save_name_suffix_modal
    } else {
      rv$save_name_suffix
    }
    filename <- tryCatch(
      basename(save_path_for_values(
        rv$dataset, input$save_name_mode_modal,
        suffix, rv$save_name_custom
      )),
      error = function(error) ""
    )
    tags$input(
      type = "text", class = "form-control",
      value = filename, disabled = "disabled"
    )
  })

  show_save_as_modal <- function() {
    req(rv$dataset)
    current_file <- if (is.null(rv$dataset$load_path)) {
      "없음"
    } else {
      basename(rv$dataset$load_path)
    }
    current_name_label <- if (is.null(rv$dataset$load_path)) {
      "기본 이름으로 저장"
    } else {
      "현재 이름으로 저장"
    }
    direct_file <- if (is.null(rv$dataset$load_path)) {
      paste0(
        tools::file_path_sans_ext(basename(rv$dataset$source_path)),
        ".csv"
      )
    } else {
      basename(rv$dataset$load_path)
    }
    save_mode_radio <- function(value, label) {
      div(
        class = "radio",
        tags$label(
          tags$input(
            type = "radio", name = "save_name_mode_modal", value = value,
            checked = if (identical(rv$save_name_mode, value)) "checked" else NULL
          ),
          tags$span(label)
        )
      )
    }
    showModal(modalDialog(
      title = "다른 이름으로 저장",
      div(
        class = "save-option-modal",
        div(
          class = "save-option-file-info",
          div(
            span(class = "save-option-label", "원본 이미지:"),
            span(basename(rv$dataset$source_path))
          ),
          div(
            span(class = "save-option-label", "현재 파일:"),
            span(current_file)
          )
        ),
        div(
          id = "save_name_mode_modal",
          class = paste(
            "form-group shiny-input-radiogroup shiny-input-container",
            "save-name-mode-options"
          ),
          div(
            class = "shiny-options-group",
            div(
              class = "save-name-option-row",
              save_mode_radio("current", current_name_label)
            ),
            div(
              class = "save-name-option-row",
              save_mode_radio("suffix", "파일이름에 접미사 추가"),
              div(
                class = "save-option-inline-input save-option-suffix-input",
                textInput(
                  "save_name_suffix_modal", NULL,
                  value = rv$save_name_suffix, width = "100%"
                )
              )
            ),
            div(
              class = "save-name-option-row",
              save_mode_radio("custom", "파일이름 직접 입력")
            )
          )
        ),
        div(
          class = "save-option-filename-box",
          conditionalPanel(
            condition = "input.save_name_mode_modal === 'custom'",
            textInput(
              "save_name_custom_modal", NULL,
              value = direct_file, width = "100%"
            )
          ),
          conditionalPanel(
            condition = "input.save_name_mode_modal !== 'custom'",
            uiOutput("save_name_resolved_box")
          )
        )
      ),
      footer = tagList(
        modalButton("취소"),
        actionButton("save_as", "저장", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  }

  observeEvent(input$save_options, {
    show_save_as_modal()
  })

  observeEvent(input$save_as, {
    req(input$save_name_mode_modal %in% c("current", "suffix", "custom"))
    suffix <- input$save_name_suffix_modal
    custom <- input$save_name_custom_modal
    save_path <- tryCatch(
      save_path_for_values(
        rv$dataset, input$save_name_mode_modal, suffix, custom
      ),
      error = function(error) {
        showNotification(conditionMessage(error), type = "error")
        NULL
      }
    )
    if (is.null(save_path)) return()
    mode <- input$save_name_mode_modal
    if (identical(mode, "suffix")) {
      rv$save_name_suffix <- csv_name_stem(suffix, "접미사")
    }
    if (identical(mode, "custom")) {
      rv$save_name_custom <- csv_name_stem(custom, "파일이름")
    }
    saved <- save_changes(target_path = save_path, force = TRUE)
    if (is.null(saved)) {
      showNotification(rv$status, type = "error")
      return()
    }
    rv$save_name_mode <- mode
    removeModal()
  })

  observeEvent(input$save, {
    req(rv$dataset)
    if (is.null(rv$dataset$load_path)) {
      show_save_as_modal()
      return()
    }
    req(!is.null(save_changes()))
  })

  output$status <- renderText(rv$status)
}

shinyApp(ui, server)
