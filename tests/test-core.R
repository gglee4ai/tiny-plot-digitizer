script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_argument)) {
  stop("테스트 파일의 위치를 확인할 수 없습니다", call. = FALSE)
}
script_path <- sub("^--file=", "", script_argument[1])
project_dir <- dirname(dirname(normalizePath(script_path, mustWork = TRUE)))

app <- new.env(parent = globalenv())
sys.source(file.path(project_dir, "app.R"), envir = app)

expect_equal <- function(actual, expected, label, tolerance = 1e-8) {
  equal <- isTRUE(all.equal(
    unname(actual), unname(expected),
    tolerance = tolerance, check.attributes = FALSE
  ))
  if (!equal) {
    stop(
      sprintf(
        "%s 실패\n실제: %s\n기대: %s",
        label, paste(actual, collapse = ", "), paste(expected, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

calibration <- app$new_project_calibration(100, 100)
expect_equal(
  app$axis_point_marker(calibration, "x1"), "triangle_up", "하단 X축 마커 방향"
)
expect_equal(
  app$axis_point_marker(calibration, "y1"), "triangle_right", "좌측 Y축 마커 방향"
)
opposite_axes <- calibration
opposite_axes$x$position <- "top"
opposite_axes$y$position <- "right"
expect_equal(
  app$axis_point_marker(opposite_axes, "x1"), "triangle_down", "상단 X축 마커 방향"
)
expect_equal(
  app$axis_point_marker(opposite_axes, "y1"), "triangle_left", "우측 Y축 마커 방향"
)

boundary_points <- data.frame(
  pixel_x = c(-20, 50, 120),
  pixel_y = c(120, 50, -20)
)
linear_values <- app$axis_values(boundary_points, calibration)
expect_equal(linear_values$x, c(0, 0.5, 1), "선형 X축 박스 경계 제한")
expect_equal(linear_values$y, c(0, 0.5, 1), "선형 Y축 박스 경계 제한")

log_calibration <- calibration
log_calibration$x$scale <- "log10"
log_calibration$x$minimum <- 1
log_calibration$x$maximum <- 100
log_values <- app$axis_values(boundary_points, log_calibration)
expect_equal(log_values$x, c(1, 10, 100), "로그 X축 박스 경계 제한")

projective_calibration <- calibration
projective_calibration$box$origin <- list(pixel_x = 10, pixel_y = 90)
projective_calibration$box$x_axis_end <- list(pixel_x = 90, pixel_y = 80)
projective_calibration$box$xy_axis_end <- list(pixel_x = 80, pixel_y = 10)
projective_calibration$box$y_axis_end <- list(pixel_x = 20, pixel_y = 20)
unit_points <- data.frame(
  x_fraction = c(0, 0.2, 0.5, 0.8, 1),
  y_fraction = c(1, 0.7, 0.5, 0.3, 0)
)
pixel_points <- app$project_unit_to_pixels(
  unit_points$x_fraction, unit_points$y_fraction, projective_calibration
)
round_trip <- app$project_pixels_to_unit(
  pixel_points$pixel_x, pixel_points$pixel_y, projective_calibration$box
)
expect_equal(
  round_trip$x_fraction, unit_points$x_fraction,
  "투영변환 X좌표 왕복"
)
expect_equal(
  round_trip$y_fraction, unit_points$y_fraction,
  "투영변환 Y좌표 왕복"
)

second_style <- app$group_style_defaults(2L)
series <- rbind(
  app$default_groups(),
  data.frame(
    id = 2L, name = "empty_group", marker = second_style$marker,
    color = second_style$color, size = second_style$size,
    alpha = second_style$alpha, stringsAsFactors = FALSE
  )
)
data <- data.frame(
  point_id = c(7L, 12L), series_id = c(1L, 1L),
  pixel_x = c(-20, 120), pixel_y = c(120, -20)
)
project_lines <- app$serialize_project_csv(
  "source.png", 100, 100, data, calibration, series
)
project_path <- tempfile(fileext = ".csv")
on.exit(unlink(project_path), add = TRUE)
app$atomic_write_lines(project_lines, project_path)

saved_data <- utils::read.csv(
  project_path, comment.char = "#", check.names = FALSE
)
expect_equal(saved_data$point_id, c(7L, 12L), "point_id CSV 왕복")
expect_equal(saved_data$x, c(0, 1), "저장된 X좌표 경계 제한")
expect_equal(saved_data$y, c(0, 1), "저장된 Y좌표 경계 제한")

metadata <- app$read_csv_metadata(project_path)
saved_series <- app$series_from_metadata(metadata$display_styles)
if (!identical(saved_series$name, c("group01", "empty_group"))) {
  stop("포인트가 없는 그룹의 메타데이터 왕복 실패", call. = FALSE)
}
saved_calibration <- app$parse_projective_calibration(metadata, names(saved_data))
if (is.null(saved_calibration) || !app$valid_projective_calibration(saved_calibration)) {
  stop("좌표 설정 메타데이터 왕복 실패", call. = FALSE)
}

cat("Tiny Plot Digitizer 핵심 회귀 테스트 통과\n")
