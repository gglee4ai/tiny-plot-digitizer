app_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
port <- as.integer(Sys.getenv("DIGITIZER_PORT", "8766"))

options(digitization.point.editor.app_dir = app_dir)
shiny::runApp(app_dir, host = "127.0.0.1", port = port, launch.browser = interactive())
