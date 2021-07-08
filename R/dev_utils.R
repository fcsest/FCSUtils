#' @title Flexible package installation function
#'
#' @description A flexible installation function for packages from CRAN or from Github repositories.
#'
#' @param pkgs A list of packages, only names or github's repository name;
#'
#' @param force Logical which force install or not; \strong{
#' ```
#' Default: TRUE
#' ```
#' }
#'
#' @return A list of packages that have been installed and packages that have not been installed...
#'
#' @examples
#' \dontrun{
#' # Install a list of packages from CRAN or Github Repo
#' c("ThinkR-open/fcuk",
#'   "ThinkR-open/attachment",
#'   "here",
#'   "jbkunst/highcharter",
#'   "tidyverse") |>
#'   install_flex(force = FALSE)}
#'
#' @author Fernando Souza
#'
#' @export
#' @importFrom remotes install_github install_cran
#' @importFrom purrr map keep map_chr when
#' @importFrom stringr str_detect str_extract
install_flex <- function(pkgs, force = T) {
  ui_start("FCSUtils::install_flex",
                     "Install Packages",
                     "Installing Github or CRAN packages from list:")
  ui_info(r"(Installing packages below directly from CRAN \/)")

  pkgs |>
    keep(str_detect,
         pattern = "/",
         negate = T) |>
    map(install_flex_cran,
        force = force)

  pkgs |>
    keep(str_detect,
         pattern = "/") |>
    map(install_flex_github,
        force = force)

  ui_end("FCSUtils::install_flex")

  pkgs |>
    map_chr(~when(.x,
                  str_detect(., "/") ~ str_extract(., "(?<=/).*"),
                  ~ paste0(.))) |>
    check_installed()
}

#' @title Check installed packages
#'
#' @description Check if package has installed or not.
#'
#' @param pkgs A list of packages to check if has installed;
#'
#' @return A list with installed packages and non installed packages...
#'
#' @examples
#' \dontrun{
#' c("fcuk",
#'   "attachment",
#'   "here",
#'   "highcharter",
#'   "tidyverse") |>
#'   check_installed()
#' }
#'
#' @export
#' @importFrom purrr keep when
check_installed <- function(pkgs) {
  inst_pkgs <- pkgs |>
    keep(~.x %in% installed.packages()) |>
    when(length(.) >= 1 ~ .,
         ~ NULL)

  non_inst_pkgs <- pkgs |>
    keep(~.x %not_in% installed.packages()) |>
    when(length(.) >= 1 ~ .,
         ~ NULL)

  if (not_null(inst_pkgs)) {
    half_drule() |>
      ui_color_success() |>
      cat()

    inst_pkgs |>
      ui_success_list(header = "Installed packages: ")
  }
  if (not_null(inst_pkgs) & not_null(non_inst_pkgs)) {
    half_srule() |>
      ui_color_success() |>
      cat("\n")

    half_srule() |>
      ui_color_error() |>
      cat()
  } else if (not_null(inst_pkgs) | not_null(non_inst_pkgs)) {
    if (not_null(inst_pkgs)) {
      half_drule() |>
        ui_color_success() |>
        cat()
    } else if (not_null(non_inst_pkgs)) {
      half_drule() |>
        ui_color_error() |>
        cat()
    }
  }
  if (not_null(non_inst_pkgs)) {
    non_inst_pkgs |>
      ui_error_list(header = "Non installed packages: ")

    half_drule() |>
      ui_color_error() |>
      cat()
  }

  list("installed" = inst_pkgs,
       "non_installed" = non_inst_pkgs) |>
    invisible()
}

#' @importFrom remotes install_cran
#' @importFrom cli col_yellow style_no_color
install_flex_cran <- function(pkg, force = T) {
  ui_step("Installing the ",
          ui_color_start("{"),
          col_yellow(pkg),
          ui_color_start("}"),
          " package directly from ",
          style_no_color("CRAN"),
          " right now...")

  pkg |>
    install_cran(force = force)
}

#' @importFrom remotes install_github
#' @importFrom purrr chuck
#' @importFrom stringr str_split
#' @importFrom cli style_no_color col_yellow style_hyperlink
install_flex_github <- function(pkg, force = T) {
  pkg_author <- pkg |>
    str_split("/") |>
    unlist() |>
    chuck(1)

  pkg_name <- pkg |>
    str_split("/") |>
    unlist() |>
    chuck(2)

  ui_step("Installing the ",
          ui_color_start("{"),
          col_yellow(pkg_name),
          ui_color_start("}"),
          " package directly from ",
          paste0(pkg_author,
                 "'s Github Repository") |>
            style_no_color() |>
            style_hyperlink(url = paste0("https://github.com/",
                                         pkg)),
          " right now...")

  pkg |>
    install_github(force = force)
}

#' @export
#' @importFrom purrr map
#' @importFrom usethis use_package
preset_dev <- function(force = FALSE) {
  c("devtools",
    "here",
    "tictoc",
    "magrittr",
    "cli",
    "usethis",
    "vroom",
    "purrr",
    "stringr",
    "dplyr",
    "dtplyr",
    "data.table") |>
    install_flex(force = force)

  c("devtools",
    "here",
    "tictoc",
    "magrittr",
    "cli",
    "usethis",
    "vroom",
    "purrr",
    "stringr",
    "dplyr",
    "dtplyr",
    "data.table") |>
    map(use_package)
}

#' @export
#' @importFrom here here
#' @importFrom dplyr tibble pull filter
#' @importFrom purrr discard pluck walk
#' @importFrom stringr str_remove_all str_squish str_split str_subset
load_dev <- function() {
  pkgs <- tibble("pkg_call" = here("DESCRIPTION") |>
                   read.dcf(fields = c("Imports", "Remotes")) |>
                   str_remove_all("(\n)|.*::") |>
                   str_split(",") |>
                   unlist() |>
                   str_subset(r"{\)$}",
                              negate = TRUE) |>
                   str_squish() |>
                   append(values = c("devtools",
                                     "usethis",
                                     "here"),
                          after = 0) |>
                   unique(),
                 "pkg_name" = pkg_call |>
                   str_remove_all(".*/"))

  non_installed <- pull(pkgs,
                        "pkg_name") |>
    check_installed() |>
    pluck("non_installed")

  if (not_null(non_installed)) {
    pkgs |>
      filter(pkg_name %in% non_installed) |>
      pull("pkg_call") |>
      install_flex()
  }

  if (pull(pkgs,
           "pkg_name") |>
      check_installed() |>
      pluck("non_installed") |>
      is.null()) {
  pkgs |>
    pull("pkg_name") |>
    walk(require,
         character.only = TRUE)
  }

  pull(pkgs,
       "pkg_name") |>
    check_installed()
}

#' @export
#' @importFrom here here
#' @importFrom usethis use_git_config edit_r_environ
first_dev <- function(name, email, preset = FALSE){
  use_git_config(scope = "user",
                 user.name = name,
                 user.email = email)

  if (preset) {

    preset_dev()

  } else if (!preset) {

    load_dev()

  }

  edit_r_environ(scope = "user")
}

#' @export
#' @importFrom devtools load_all
run_dev <- function(){
  # Clear workspace
  rm(list = ls(all.names = TRUE))

  # Load package
  load_all()
}

#' @export
#' @importFrom stringr str_detect
uses_dtplyr <- function(imports){
  if (any(str_detect(imports, "data.table")) &
      any(str_detect(imports, "dtplyr")) &
      any(str_detect(imports, "dplyr"))) {

    TRUE

  } else {

    FALSE

  }
}

#' @export
#' @importFrom here here
#' @importFrom devtools document check
#' @importFrom purrr discard detect_index when
check_dev <- function(){
  # Removing man folder with the old documentation
  here("man") |>
  unlink(recursive = TRUE)

  # Removing NAMESPACE file
  here("NAMESPACE") |>
  file.remove()

  # Clear workspace
  rm(list = ls(all.names = TRUE))

  # Update documentation
  document()

  # Checking package
  check()

  # Reorder import of data.table
  if (here("NAMESPACE") |>
    readLines() |>
    uses_dtplyr()) {

    here("NAMESPACE") |>
      readLines() |>
      discard(str_detect,
        pattern = r"{import\(data.table\)}"
      ) |>
      when(~append(.,
                   values = "import(data.table)",
                   after = detect_index(.,
                                        str_detect,
                                        pattern = r"{import\(dtplyr\)}"
                                        )
                   )) |>
      writeLines(here("NAMESPACE"))
  }
}

#' @export
#' @importFrom devtools build
build_dev <- function(){
  # Building a compressed file of package
  build()
}
