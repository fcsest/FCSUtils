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
#' @importFrom purrr map keep
#' @importFrom stringr str_detect str_extract
install_flex <- function(pkgs, force = T){
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

  pkgs %>%
    {ifelse(str_detect(.,
                       "/"),
            str_extract(., "(?<=/).*"),
            .)} |>
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
#' c("fcuk",
#'   "attachment",
#'   "here",
#'   "highcharter",
#'   "tidyverse") |>
#'   check_installed()
#'
#' @export
#' @importFrom purrr keep
check_installed <- function(pkgs) {
  list("installed" = pkgs |> keep(~.x %in% installed.packages()),
       "non_installed" = pkgs |> keep(~.x %not_in% installed.packages()))
}

#' @importFrom remotes install_cran
#' @importFrom cli col_yellow style_no_color
install_flex_cran <- function(pkg, force = T){
    FCSUtils::ui_step("Installing the ",
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
install_flex_github <- function(pkg, force = T){
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
#' @importFrom pacman p_load
#' @importFrom purrr map
#' @importFrom usethis use_package
preset_dev <- function(){
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
    "data.table") %>%
    {p_load(char = .)}

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
    "data.table") %>%
    map(use_package)
}

#' @export
#' @importFrom here here
#' @importFrom purrr discard
#' @importFrom stringr str_remove str_squish
#' @importFrom pacman p_load
load_dev <- function(){
  here("DESCRIPTION") %>%
    readLines() %>%
    discard(str_detect,
            pattern = "(\\:)|(\\=)|(pacman)") %>%
    str_remove(",") %>%
    str_squish() %>%
    append(values = c("devtools",
                      "usethis",
                      "here"),
           after = 0) %>%
    unique() %>%
    {p_load(char = .)}
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
#' @importFrom purrr discard detect_index
check_dev <- function(){
  # Removing man folder with the old documentation
  here("man") %>%
  unlink(recursive = TRUE)

  # Removing NAMESPACE file
  here("NAMESPACE") %>%
  file.remove()

  # Clear workspace
  rm(list = ls(all.names = TRUE))

  # Update documentation
  document()

  # Checking package
  check()

  # Reorder import of data.table
  if (here("NAMESPACE") %>%
    readLines() %>%
    uses_dtplyr()) {

    here("NAMESPACE") %>%
      readLines() %>%
      discard(str_detect,
        pattern = "import\\(data.table\\)"
      ) %>%
      append(
        values = "import(data.table)",
        after = detect_index(.,
          str_detect,
          pattern = "import\\(dtplyr\\)"
        )
      ) %>%
      writeLines(here("NAMESPACE"))

  }
}

#' @export
#' @importFrom devtools build
build_dev <- function(){
  # Building a compressed file of package
  build()
}
