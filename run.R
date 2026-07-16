script_arguments <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_arguments)) {
  stop("run.R 파일의 위치를 확인할 수 없습니다", call. = FALSE)
}
app_dir <- dirname(normalizePath(
  sub("^--file=", "", script_arguments[1]), mustWork = TRUE
))

required_packages <- c("shiny", "shinyFiles", "png", "yaml")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages)) {
  stop(
    paste0(
      "필요한 R 패키지가 없습니다: ", paste(missing_packages, collapse = ", "),
      "\n다음 명령으로 설치하세요:\ninstall.packages(c(",
      paste(encodeString(missing_packages, quote = '"'), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

port_text <- trimws(Sys.getenv("DIGITIZER_PORT", "8766"))
port <- suppressWarnings(as.integer(port_text))
if (length(port) != 1L || is.na(port) || port < 1L || port > 65535L) {
  stop("DIGITIZER_PORT는 1~65535 범위의 정수여야 합니다", call. = FALSE)
}

browser_setting <- tolower(trimws(Sys.getenv("DIGITIZER_BROWSER", "true")))
launch_browser <- !browser_setting %in% c("0", "false", "no", "off")

options(digitization.point.editor.app_dir = app_dir)
shiny::runApp(
  app_dir,
  host = "127.0.0.1",
  port = port,
  launch.browser = launch_browser
)
