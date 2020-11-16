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
    p_load()

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

#' @importFrom here here
#' @importFrom purrr discard
#' @importFrom stringr str_remove str_squish
#' @importFrom pacman p_load
load_dev <- function(){
  here("DESCRIPTION") %>%
    readLines() %>%
    discard(str_detect,
            pattern = "(\\:)|(\\=)") %>%
    str_remove(",") %>%
    str_squish() %>%
    append(values = c("devtools",
                      "pacman",
                      "usethis",
                      "here"),
           after = 0) %>%
    unique() %>%
    p_load()
}

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

#' @importFrom devtools load_all
run_dev <- function(){
  # Clear workspace
  rm(list = ls(all.names = TRUE))

  # Load package
  load_all()
}

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

#' @importFrom devtools build
build_dev <- function(){
  # Building a compressed file of package
  build()
}
