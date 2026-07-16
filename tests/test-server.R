script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_argument)) {
  stop("테스트 파일의 위치를 확인할 수 없습니다", call. = FALSE)
}
script_path <- sub("^--file=", "", script_argument[1])
project_dir <- dirname(dirname(normalizePath(script_path, mustWork = TRUE)))

app <- new.env(parent = globalenv())
sys.source(file.path(project_dir, "app.R"), envir = app)

fixture_dir <- tempfile("tiny-plot-digitizer-test-", tmpdir = project_dir)
dir.create(fixture_dir)

calibration <- app$new_project_calibration(10, 10)
series <- app$default_groups()
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

Sys.setenv(DIGITIZER_FOLDER = fixture_dir)

shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(dataset = project_paths[1])
  session$flushReact()
  stopifnot(identical(rv$dataset$key, project_paths[1]))

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

unlink(fixture_dir, recursive = TRUE)
Sys.unsetenv(c("DIGITIZER_FOLDER", "DIGITIZER_DIRTY_STATE_FILE"))
cat("Tiny Plot Digitizer 서버 전환 테스트 통과\n")
