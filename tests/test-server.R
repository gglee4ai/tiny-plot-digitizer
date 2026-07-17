script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_argument)) {
  stop("테스트 파일의 위치를 확인할 수 없습니다", call. = FALSE)
}
script_path <- sub("^--file=", "", script_argument[1])
project_dir <- dirname(dirname(normalizePath(script_path, mustWork = TRUE)))

app <- new.env(parent = globalenv())
sys.source(file.path(project_dir, "app.R"), envir = app)

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

run_server_tests <- function() {
fixture_dir <- tempfile("tiny-plot-digitizer-test-", tmpdir = project_dir)
dir.create(fixture_dir)
on.exit({
  unlink(fixture_dir, recursive = TRUE)
  Sys.unsetenv(c(
    "DIGITIZER_FOLDER", "DIGITIZER_DIRTY_STATE_FILE", "DIGITIZER_DRAFT_FILE"
  ))
}, add = TRUE)

calibration <- app$new_project_calibration(10, 10)
series <- app$default_groups()
second_style <- app$group_style_defaults(2L)
series <- rbind(
  series,
  data.frame(
    id = 2L, name = "group02", marker = second_style$marker,
    color = second_style$color, size = second_style$size,
    alpha = second_style$alpha, stringsAsFactors = FALSE
  )
)
data <- data.frame(
  point_id = 7L, series_id = 1L, pixel_x = 4, pixel_y = 6
)
project_paths <- character(2)
for (index in seq_len(2)) {
  image_path <- file.path(fixture_dir, paste0("source", index, ".png"))
  png::writePNG(array(1, dim = c(10, 10, 4)), image_path)
  project_paths[index] <- file.path(fixture_dir, paste0("project", index, ".csv"))
  app$atomic_write_lines(
    app$serialize_project_csv(
      image_path, 10, 10, data, calibration, series
    ),
    project_paths[index]
  )
  project_paths[index] <- normalizePath(project_paths[index], mustWork = TRUE)
}

draft_path <- file.path(fixture_dir, "recovery-draft.rds")
Sys.setenv(
  DIGITIZER_FOLDER = fixture_dir,
  DIGITIZER_DRAFT_FILE = draft_path
)

picker_child <- file.path(fixture_dir, "폴더 선택 테스트")
picker_nested <- file.path(picker_child, "하위")
dir.create(picker_nested, recursive = TRUE)

shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(folder = 1)
  session$flushReact()
  expect_true(
    identical(folder_picker_path(), normalizePath(fixture_dir)),
    "폴더 선택 모달 시작 경로"
  )
  entries <- folder_picker_entries()
  child_index <- match("폴더 선택 테스트", entries$name)
  expect_true(!is.na(child_index), "폴더 선택 하위 목록")

  session$setInputs(
    folder_picker_open = list(index = child_index, nonce = 1)
  )
  session$flushReact()
  expect_true(
    identical(folder_picker_path(), normalizePath(picker_child)),
    "폴더 선택 하위 이동"
  )
  session$setInputs(folder_picker_up = 1)
  session$flushReact()
  expect_true(
    identical(folder_picker_path(), normalizePath(fixture_dir)),
    "폴더 선택 상위 이동"
  )

  session$setInputs(
    folder_picker_open = list(index = child_index, nonce = 2)
  )
  session$flushReact()
  session$setInputs(confirm_folder_picker = 1)
  session$flushReact()
  expect_true(
    identical(selected_folder(), normalizePath(picker_child)),
    "현재 작업 폴더 선택"
  )
})

shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(dataset = project_paths[1])
  session$flushReact()
  stopifnot(identical(rv$dataset$key, project_paths[1]))
  expect_true(inherits(rv$raster_matrix, "nativeRaster"), "PNG nativeRaster 로딩")

  rv$point_dirty <- TRUE
  session$setInputs(dataset = project_paths[2])
  session$flushReact()
  stopifnot(
    identical(rv$dataset$key, project_paths[1]),
    identical(rv$pending_navigation$kind, "dataset")
  )

  session$setInputs(cancel_navigation = 1)
  session$flushReact()
  stopifnot(
    identical(rv$dataset$key, project_paths[1]),
    is.null(rv$pending_navigation)
  )
})

shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(dataset = project_paths[1])
  session$flushReact()
  rv$data$pixel_x[1] <- 8
  rv$point_dirty <- TRUE

  session$setInputs(dataset = project_paths[2])
  session$flushReact()
  session$setInputs(discard_navigation = 1)
  session$flushReact()
  stopifnot(
    identical(rv$dataset$key, project_paths[2]),
    is.null(rv$pending_navigation)
  )
  unchanged <- utils::read.csv(
    project_paths[1], comment.char = "#", check.names = FALSE
  )
  stopifnot(identical(unchanged$pixel_x, 4L))
})

shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(dataset = project_paths[1])
  session$flushReact()
  rv$data$pixel_x[1] <- 8
  rv$point_dirty <- TRUE

  session$setInputs(dataset = project_paths[2])
  session$flushReact()
  session$setInputs(save_navigation = 1)
  session$flushReact()
  stopifnot(
    identical(rv$dataset$key, project_paths[2]),
    is.null(rv$pending_navigation)
  )
  saved <- utils::read.csv(
    project_paths[1], comment.char = "#", check.names = FALSE
  )
  stopifnot(identical(saved$pixel_x, 8L))
})

project1_snapshot <- app$read_file_bytes(project_paths[1])
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(dataset = project_paths[1])
  session$flushReact()

  external_snapshot <- c(project1_snapshot, charToRaw("\n# external change\n"))
  app$atomic_write_bytes(external_snapshot, project_paths[1])
  rv$data$pixel_x[1] <- 9
  rv$point_dirty <- TRUE

  saved <- save_changes()
  stopifnot(
    is.null(saved),
    isTRUE(rv$point_dirty),
    grepl("앱 밖에서 변경", rv$status, fixed = TRUE),
    identical(app$read_file_bytes(project_paths[1]), external_snapshot)
  )
})
app$atomic_write_bytes(project1_snapshot, project_paths[1])

project2_snapshot <- app$read_file_bytes(project_paths[2])
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(dataset = project_paths[1])
  session$flushReact()
  app$atomic_write_lines("broken", project_paths[2])

  loaded <- perform_navigation(list(
    kind = "dataset", target = project_paths[2], label = basename(project_paths[2])
  ))
  stopifnot(
    identical(loaded, FALSE),
    identical(rv$dataset$key, project_paths[1]),
    grepl("파일을 불러오지 못했습니다", rv$status, fixed = TRUE)
  )

  app$atomic_write_bytes(project2_snapshot, project_paths[2])
  loaded <- perform_navigation(list(
    kind = "dataset", target = project_paths[2], label = basename(project_paths[2])
  ))
  stopifnot(
    identical(loaded, TRUE),
    identical(rv$dataset$key, project_paths[2])
  )
})

unlink(draft_path)
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(dataset = project_paths[1], move_step = 1)
  session$flushReact()

  original_x <- rv$data$pixel_x[rv$selected]
  session$setInputs(key_move = list(direction = "left", start = TRUE, nonce = 1))
  session$flushReact()
  session$setInputs(key_move = list(direction = "left", start = FALSE, nonce = 2))
  session$flushReact()
  expect_true(
    identical(length(rv$point_history), 1L),
    "방향키 반복 이동 실행취소 기록 병합"
  )
  expect_true(
    isTRUE(all.equal(rv$data$pixel_x[rv$selected], original_x - 2)),
    "방향키 반복 이동 좌표 반영"
  )

  session$setInputs(key_move_end = 1)
  session$flushReact()
  session$setInputs(key_move = list(direction = "left", start = TRUE, nonce = 3))
  session$flushReact()
  expect_true(
    identical(length(rv$point_history), 2L),
    "다음 방향키 이동 실행취소 기록 분리"
  )

  undo_target()
  session$flushReact()
  expect_true(
    isTRUE(all.equal(rv$data$pixel_x[rv$selected], original_x - 2)),
    "두 번째 방향키 이동 실행취소"
  )
  undo_target()
  session$flushReact()
  expect_true(
    isTRUE(all.equal(rv$data$pixel_x[rv$selected], original_x)),
    "병합된 방향키 반복 이동 실행취소"
  )
})

unlink(draft_path)
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(dataset = project_paths[1])
  session$flushReact()

  first_group_point <- selected_point_id()
  rv$data <- rbind(
    rv$data,
    data.frame(
      point_id = c(8L, 9L), series_id = c(2L, 2L),
      pixel_x = c(8, 2), pixel_y = c(5, 5)
    )
  )
  sort_points(first_group_point)
  refresh_controls(rv$selected)

  session$setInputs(next_series = 1)
  session$flushReact()
  expect_true(identical(selected_point_id(), 9L), "다음 그룹 첫 포인트 이동")
  session$setInputs(previous_series = 1)
  session$flushReact()
  expect_true(
    identical(selected_point_id(), first_group_point),
    "이전 그룹 첫 포인트 이동"
  )
})

unlink(draft_path)
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(dataset = project_paths[1])
  session$flushReact()

  original_point_id <- selected_point_id()
  original_count <- nrow(rv$data)
  session$setInputs(delete_point = 1)
  session$flushReact()
  expect_true(
    identical(rv$pending_point_delete$point_id, original_point_id),
    "삭제 확인 대상 포인트"
  )
  expect_true(identical(nrow(rv$data), original_count), "확인 전 포인트 유지")

  session$setInputs(cancel_point_delete = 1)
  session$flushReact()
  expect_true(is.null(rv$pending_point_delete), "삭제 취소 모달 상태 정리")
  expect_true(identical(nrow(rv$data), original_count), "삭제 취소 포인트 유지")

  session$setInputs(delete_point = 2)
  session$flushReact()
  session$setInputs(confirm_point_delete = 1)
  session$flushReact()
  expect_true(identical(nrow(rv$data), original_count - 1L), "포인트 삭제 확인")
  expect_true(is.null(rv$pending_point_delete), "삭제 확인 모달 상태 정리")
  expect_true(undo_target(), "포인트 삭제 Undo")
  expect_true(original_point_id %in% rv$data$point_id, "포인트 삭제 Undo 상태")
})

unlink(draft_path)
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(dataset = project_paths[1])
  session$flushReact()

  original_point_id <- selected_point_id()
  expect_true(identical(rv$data$series_id[rv$selected], 1L), "변경 전 포인트 그룹")
  session$setInputs(change_point_series = 1)
  session$flushReact()
  expect_true(
    identical(rv$pending_point_series_change$point_id, original_point_id),
    "그룹변경 대상 포인트"
  )
  session$setInputs(point_series_target = "2", confirm_point_series_change = 1)
  session$flushReact()
  changed_row <- match(original_point_id, rv$data$point_id)
  expect_true(identical(rv$data$series_id[changed_row], 2L), "포인트 그룹변경")
  expect_true(identical(selected_point_id(), original_point_id), "그룹변경 후 선택 유지")
  expect_true(is.null(rv$pending_point_series_change), "그룹변경 모달 상태 정리")
  expect_true(undo_target(), "포인트 그룹변경 Undo")
  restored_row <- match(original_point_id, rv$data$point_id)
  expect_true(identical(rv$data$series_id[restored_row], 1L), "포인트 그룹변경 Undo 상태")
})

unlink(draft_path)
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(dataset = project_paths[1])
  session$flushReact()

  start_x <- rv$data$pixel_x[1]
  start_y <- rv$data$pixel_y[1]
  expect_true(set_selected_point_position(start_x + 1, start_y), "첫 포인트 이동")
  expect_true(set_selected_point_position(start_x + 2, start_y), "둘째 포인트 이동")
  expect_true(length(rv$point_history) == 2L, "포인트 이력 2단계")
  expect_true(undo_target(), "첫 포인트 Undo")
  expect_true(identical(rv$data$pixel_x[1], start_x + 1), "첫 포인트 Undo 좌표")
  session$flushReact()
  expect_true(undo_target(), "둘째 포인트 Undo")
  expect_true(identical(rv$data$pixel_x[1], start_x), "둘째 포인트 Undo 좌표")
  session$flushReact()
  expect_true(redo_target(), "첫 포인트 Redo")
  expect_true(identical(rv$data$pixel_x[1], start_x + 1), "첫 포인트 Redo 좌표")
  session$flushReact()
  expect_true(redo_target(), "둘째 포인트 Redo")
  expect_true(identical(rv$data$pixel_x[1], start_x + 2), "둘째 포인트 Redo 좌표")
  session$flushReact()

  session$setInputs(zoom_click = list(x = 7.4, y = 3.6))
  session$flushReact()
  expect_true(identical(rv$data$pixel_x[1], 7.5), "확대 화면 X좌표")
  expect_true(identical(rv$data$pixel_y[1], 3.5), "확대 화면 Y좌표")

  original_name <- rv$series$name[1]
  remember_point_change()
  rv$series$name[1] <- "renamed"
  mark_mode_changed("point")
  expect_true(undo_target(), "그룹 Undo")
  expect_true(identical(rv$series$name[1], original_name), "그룹 Undo 상태")
  session$flushReact()
  expect_true(redo_target(), "그룹 Redo")
  expect_true(identical(rv$series$name[1], "renamed"), "그룹 Redo 상태")
  session$flushReact()

  rv$active_edit_mode <- "calibration"
  rv$calibration_target <- "box"
  rv$calibration_point <- "origin"
  origin <- rv$calibration$box$origin
  expect_true(set_calibration_box_point("origin", 1, origin$pixel_y), "첫 좌표설정 이동")
  expect_true(set_calibration_box_point("origin", 2, origin$pixel_y), "둘째 좌표설정 이동")
  expect_true(undo_target(), "첫 좌표설정 Undo")
  expect_true(identical(rv$calibration$box$origin$pixel_x, 1), "첫 좌표설정 Undo 좌표")
  session$flushReact()
  expect_true(undo_target(), "둘째 좌표설정 Undo")
  expect_true(
    identical(rv$calibration$box$origin$pixel_x, origin$pixel_x),
    "둘째 좌표설정 Undo 좌표"
  )
  session$flushReact()
  expect_true(redo_target(), "좌표설정 Redo")
  expect_true(identical(rv$calibration$box$origin$pixel_x, 1), "좌표설정 Redo 좌표")
  session$flushReact()

  save_recovery_draft()
  expect_true(file.exists(draft_path), "복구 draft 저장")
  draft <- app$read_recovery_draft(draft_path)
  rv$data$pixel_x[1] <- 0
  expect_true(restore_recovery_draft(draft), "복구 draft 적용")
  expect_true(identical(rv$data$pixel_x[1], draft$data$pixel_x[1]), "복구 draft 좌표")
  capture_all_baselines()
  save_recovery_draft()
  expect_true(!file.exists(draft_path), "정상 상태의 복구 draft 제거")
})

dirty_state_path <- file.path(fixture_dir, "dirty-state")
Sys.setenv(DIGITIZER_DIRTY_STATE_FILE = dirty_state_path)
shiny::testServer(app$server, {
  session$flushReact()
  stopifnot(identical(readLines(dirty_state_path), "clean"))
  rv$point_dirty <- TRUE
  session$flushReact()
  stopifnot(identical(readLines(dirty_state_path), "dirty"))
  rv$point_dirty <- FALSE
  session$flushReact()
  stopifnot(identical(readLines(dirty_state_path), "clean"))
})

}

run_server_tests()
cat("Tiny Plot Digitizer 서버 전환 테스트 통과\n")
