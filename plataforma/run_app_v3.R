local_lib <- file.path(getwd(), "r-lib")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = FALSE), .libPaths()))
}

required_packages <- c("shiny", "DT", "openxlsx", "readxl")
missing <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing) > 0) {
  stop(
    "Missing R packages: ",
    paste(missing, collapse = ", "),
    ". Run install_packages.R first.",
    call. = FALSE
  )
}

host <- Sys.getenv("SHINY_HOST", "127.0.0.1")
port <- as.integer(Sys.getenv("SHINY_PORT", "3838"))

shiny::runApp("plataforma/app_v3.R", host = host, port = port, launch.browser = interactive())
