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

expect_error <- function(expression, label) {
  result <- try(force(expression), silent = TRUE)
  if (!inherits(result, "try-error")) stop(label, call. = FALSE)
}

calibration <- app$new_project_calibration(100, 100)
history <- list()
for (value in seq_len(55)) {
  history <- app$append_history(history, value)
}
expect_equal(length(history), 50L, "편집 이력 최대 개수")
expect_equal(unlist(history[c(1, 50)]), c(6, 55), "편집 이력 오래된 항목 제거")

folder_fixture <- tempfile("tiny-plot-digitizer-folder-")
dir.create(folder_fixture)
on.exit(unlink(folder_fixture, recursive = TRUE), add = TRUE)
development_folder <- file.path(folder_fixture, "development")
expect_equal(
  app$default_working_folder(folder_fixture, development_folder),
  folder_fixture, "개발 폴더가 없을 때 홈 폴더 기본값"
)
dir.create(development_folder)
expect_equal(
  app$default_working_folder(folder_fixture, development_folder),
  normalizePath(development_folder), "개발 폴더 기본값"
)

special_header_value <- '그룹: "A", C:\\plots'
expect_equal(
  app$decode_project_header_string(
    app$quote_project_header(special_header_value)
  ),
  special_header_value, "CSV 헤더 특수 문자열 왕복"
)

legacy_project_path <- file.path(
  project_dir, "tests", "fixtures", "legacy-project.csv"
)
legacy_header <- app$read_project_header_lines(legacy_project_path)
if (any(startsWith(legacy_header, '"group"'))) {
  stop("CSV 헤더 전용 읽기가 본문까지 읽었습니다", call. = FALSE)
}
legacy_metadata <- app$read_csv_metadata(legacy_project_path)
expect_equal(
  legacy_metadata$source_image$filename, "legacy-source.png",
  "구버전 CSV 원본 이미지 헤더"
)
legacy_series <- app$series_from_metadata(legacy_metadata$display_styles)
expect_equal(
  legacy_series$name, 'legacy: "series"', "구버전 CSV 그룹 헤더"
)
expect_equal(
  legacy_series$color, "#007AFF", "영문 그룹 색상 이름 읽기"
)
expect_error(
  app$group_color_value("#d62728"),
  "구버전 그룹 색상 코드를 허용했습니다"
)
expect_equal(
  vapply(
    unname(app$group_color_palette), app$group_color_name, character(1)
  ),
  names(app$group_color_palette), "그룹 색상 코드의 영문 이름 변환"
)
legacy_data <- utils::read.csv(
  legacy_project_path, comment.char = "#", check.names = FALSE
)
legacy_calibration <- app$parse_projective_calibration(
  legacy_metadata, names(legacy_data)
)
if (is.null(legacy_calibration) ||
    !app$valid_projective_calibration(legacy_calibration)) {
  stop("구버전 CSV 좌표 설정 헤더를 읽지 못했습니다", call. = FALSE)
}

other_metadata_path <- file.path(folder_fixture, "other-metadata.csv")
writeLines(c(
  "# ---", '# title: "다른 형식"', "# nested:", "#   value: 1",
  "# ---", "value", "1"
), other_metadata_path)
expect_equal(
  length(app$read_csv_metadata(other_metadata_path)), 0L,
  "다른 형식의 메타데이터 헤더 무시"
)

picker_root <- file.path(folder_fixture, "picker-root")
visible_folder <- file.path(picker_root, "Visible")
hidden_folder <- file.path(picker_root, ".hidden")
outside_folder <- file.path(folder_fixture, "outside")
dir.create(visible_folder, recursive = TRUE)
dir.create(hidden_folder)
dir.create(outside_folder)
writeLines("not a folder", file.path(picker_root, "file.txt"))
picker_entries <- app$list_child_folders(picker_root, picker_root)
expect_equal(picker_entries$name, "Visible", "폴더 선택 목록 필터링")

writeLines("csv", file.path(picker_root, "data.csv"))
writeLines("png", file.path(picker_root, "plot.PNG"))
writeLines("hidden", file.path(picker_root, ".hidden.csv"))
dir.create(file.path(picker_root, "folder.csv"))
picker_files <- app$list_folder_files(picker_root, picker_root)
expect_equal(
  picker_files$name, c("data.csv", "plot.PNG"),
  "폴더 선택 PNG/CSV 파일 목록"
)

nested_folder <- file.path(visible_folder, "nested")
dir.create(nested_folder)
expect_equal(
  app$parent_folder_in_root(nested_folder, picker_root),
  normalizePath(visible_folder), "폴더 선택 상위 이동"
)
expect_equal(
  app$parent_folder_in_root(picker_root, picker_root),
  normalizePath(picker_root), "폴더 선택 루트 경계"
)
expect_error(
  app$normalize_folder_in_root(outside_folder, picker_root),
  "폴더 선택 홈 밖 경로 차단"
)
outside_link <- file.path(picker_root, "outside-link")
if (isTRUE(file.symlink(outside_folder, outside_link))) {
  picker_entries <- app$list_child_folders(picker_root, picker_root)
  if ("outside-link" %in% picker_entries$name) {
    stop("폴더 선택 홈 밖 심볼릭 링크 차단 실패", call. = FALSE)
  }
}

draft_path <- file.path(folder_fixture, "recovery-draft.rds")
draft <- list(
  version = app$recovery_draft_version,
  saved_at = as.numeric(Sys.time()),
  dataset = list(
    key = "new::source.png", source_path = "source.png",
    load_path = NULL, label = "[복구] source.png"
  ),
  image_width = 100, image_height = 100,
  data = app$empty_points(), series = app$default_groups(),
  calibration = calibration,
  point_baseline_data = app$empty_points(),
  series_baseline = app$default_groups(),
  calibration_baseline = calibration,
  point_dirty = TRUE, calibration_dirty = FALSE,
  selected_point_id = NULL, active_edit_mode = "point",
  calibration_target = NULL, calibration_point = NULL,
  save_name_mode = "current", save_name_suffix = "-digitized",
  save_name_custom = "source",
  initial_file_snapshot = NULL, latest_saved_snapshot = NULL,
  disk_file_snapshot = NULL
)
app$atomic_write_recovery_draft(draft, draft_path)
recovered_draft <- app$read_recovery_draft(draft_path)
expect_equal(recovered_draft$version, app$recovery_draft_version, "복구 draft 왕복")

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

first_transforms <- app$projective_transforms(projective_calibration$box)
second_transforms <- app$projective_transforms(projective_calibration$box)
if (!identical(first_transforms, second_transforms)) {
  stop("동일한 박스의 투영변환 계수를 재사용하지 못했습니다", call. = FALSE)
}
changed_calibration <- projective_calibration
changed_calibration$box$origin$pixel_x <- 11
changed_transforms <- app$projective_transforms(changed_calibration$box)
if (identical(first_transforms, changed_transforms)) {
  stop("변경된 박스의 투영변환 계수를 다시 계산하지 못했습니다", call. = FALSE)
}

native_image_path <- system.file("img", "Rlogo.png", package = "png")
native_image <- png::readPNG(native_image_path, native = TRUE)
native_crop <- app$crop_raster(native_image, 1:10, 1:12)
if (!inherits(native_crop, "nativeRaster") ||
    !identical(attr(native_crop, "channels"), attr(native_image, "channels"))) {
  stop("PNG 잘라내기에서 nativeRaster 형식을 유지하지 못했습니다", call. = FALSE)
}
expect_equal(dim(native_crop), c(10L, 12L), "nativeRaster 잘라내기 크기")
expected_crop <- as.raster(png::readPNG(native_image_path))[1:10, 1:12, drop = FALSE]
render_raster <- function(image, path) {
  grDevices::png(path, width = 120, height = 100)
  tryCatch({
    par(mar = c(0, 0, 0, 0))
    plot.new()
    plot.window(c(0, 1), c(0, 1), xaxs = "i", yaxs = "i")
    rasterImage(image, 0, 0, 1, 1, interpolate = FALSE)
  }, finally = grDevices::dev.off())
}
native_render_path <- tempfile(fileext = ".png")
expected_render_path <- tempfile(fileext = ".png")
on.exit(unlink(c(native_render_path, expected_render_path)), add = TRUE)
render_raster(native_crop, native_render_path)
render_raster(expected_crop, expected_render_path)
expect_equal(
  png::readPNG(native_render_path), png::readPNG(expected_render_path),
  "nativeRaster 잘라내기 화면 결과", tolerance = 0
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
expect_equal(
  vapply(
    metadata$display_styles,
    function(style) as.character(style$color), character(1)
  ),
  c("red", "blue"), "저장된 그룹 색상의 영문 이름"
)
saved_series <- app$series_from_metadata(metadata$display_styles)
if (!identical(saved_series$name, c("group01", "empty_group"))) {
  stop("포인트가 없는 그룹의 메타데이터 왕복 실패", call. = FALSE)
}
saved_calibration <- app$parse_projective_calibration(metadata, names(saved_data))
if (is.null(saved_calibration) || !app$valid_projective_calibration(saved_calibration)) {
  stop("좌표 설정 메타데이터 왕복 실패", call. = FALSE)
}

cat("Tiny Plot Digitizer 핵심 회귀 테스트 통과\n")
