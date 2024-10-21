# Global variable for the cookie
cookie_file <- file.path(tempdir(), "sp_cookie.txt")

#' Read a SharePoint item based on a file URL or file name and site details
#' (Updated to handle cookies)
#'
#' @description
#' [read_sharepoint()] is designed to download a SharePoint item to a temporary
#' folder and read the file based on the file extension, while handling cookies
#' to avoid repeated authentication.
#'
#' @name read_sharepoint
#' @param file Required. A SharePoint shared file URL, document URL, or file name.
#' @param .f Optional function to use to read the file downloaded from SharePoint.
#' @param ... Additional parameters passed to one of the reading functions or supplied to `.f`
#' @param new_path The path where the downloaded file will be saved.
#' @param overwrite Logical. Whether to overwrite an existing file.
#' @param drive_name, drive_id, site_url, site_name, site_id SharePoint site details.
#' @param site SharePoint site object.
#' @export
read_sharepoint <- function(file,
                            ...,
                            .f = NULL,
                            new_path = tempdir(),
                            overwrite = TRUE,
                            drive_name = NULL,
                            drive_id = NULL,
                            drive = NULL,
                            site_url = NULL,
                            site_name = NULL,
                            site_id = NULL,
                            site = NULL) {
  
  # Function to load cookie (if exists)
  load_cookie <- function() {
    if (file.exists(cookie_file)) {
      return(readLines(cookie_file))
    }
    return(NULL)
  }
  
  # Function to save the cookie
  save_cookie <- function(cookie) {
    writeLines(cookie, cookie_file)
  }
  
  # Check if we already have a saved cookie
  sp_cookie <- load_cookie()
  
  if (is.null(sp_cookie)) {
    # Cookie not found, perform authentication and save cookie
    sp_cookie <- authenticate_and_get_cookie()  # Replace with actual auth logic
    save_cookie(sp_cookie)
    message("New cookie generated and saved.")
  } else {
    message("Using saved cookie.")
  }
  
  # Use the saved cookie for the download request
  dest <- download_sp_file(
    file = file,
    new_path = new_path,
    drive_name = drive_name,
    drive_id = drive_id,
    drive = drive,
    site_url = site_url,
    site_name = site_name,
    site_id = site_id,
    site = site,
    overwrite = overwrite,
    cookie = sp_cookie  # Use the saved cookie here
  )

  message <- "Reading item with "

  if (!is.null(.f)) {
    fn_label <- expr_text(substitute(.f))
    .f <- as_function(.f)
    cli_progress_step("{message}{.fn {fn_label}}")
    return(.f(dest, ...))
  }

  if (is_fileext_path(dest, c("csv", "csv2", "tsv"))) {
    check_installed("readr")
    cli_progress_step("{message}{.fn read_csv}")
    return(read_csv(dest, ...))
  }

  if (is_fileext_path(dest, c("xlsx", "xls"))) {
    check_installed("readxl")
    cli_progress_step("{message}{.fn readxl::read_excel}")
    return(readxl::read_excel(dest, ...))
  }

  if (is_fileext_path(dest, "rds")) {
    check_installed("readr")
    cli_progress_step("{message}{.fn readr::read_rds}")
    return(readr::read_rds(dest, ...))
  }

  if (is_fileext_path(dest, "docx")) {
    check_installed("officer")
    cli_progress_step("{message}{.fn officer::read_docx}")
    return(officer::read_docx(dest))
  }

  if (is_fileext_path(dest, "pptx")) {
    check_installed("officer")
    cli_progress_step("{message}{.fn officer::read_pptx}")
    return(officer::read_pptx(dest))
  }

  if (is_fileext_path(dest, c("gpkg", "geojson", "kml", "gdb", "zip"))) {
    check_installed("sf")
    cli_progress_step("{message}{.fn sf::read_sf}")
    return(sf::read_sf(dest, ...))
  }

  check_installed("readr")
  cli_progress_step("{message}{.fn readr::read_lines}")
  readr::read_lines(dest, ...)
}

