#' @export
#' @importFrom purrr when
#' @importFrom emo ji
startup_message <- function() {
  cat("\n")
  full_drule() |>
    cat()

  Sys.time() |>
    format("%H") |>
    as.numeric() |>
    when(. %in% 5:11 ~ paste0("\nBom dia ",
                              ji("sunrise_over_mountains")),
         . %in% 12:18 ~ paste0("\nBoa tarde ",
                               ji("sun_behind_cloud")),
         . %in% 19:23 ~ paste0("\nBoa noite ",
                               ji("crescent_moon")),
         ~ paste0("\nBoa madrugada ",
                  ji("new_moon"))) |>
    paste0("\nBem vindo ao R das ruas ", ji("sunglasses"),
           "\nHoje \u00e9 ",
           format(Sys.time(),
                  "%A-feira, dia %d de %B de %Y e s\u00e3o %H horas e %M minutos "),
           ji("poop")) |>
    cat("\n")

  full_drule() |>
    cat("\n\n")
}

#' @export
#' @importFrom cli combine_ansi_styles
ui_color_start <- function(...){
  combine_ansi_styles("#10aff2",
                      "bold")(...)
}

#' @export
#' @importFrom cli boxx rule
ui_start <- function(function_name, title, description, length = 100){
  cat("\n\n\n",
      rule(left = ui_color_start(function_name,
                                 "()"),
           line = 2,
           col = "bold",
           width = length),
      "\n")
  cat(boxx(ui_color_start(title),
           float = "center",
           border_style = "round",
           col = "bold",
           width = length),
      "\n")
  cat(rule(width = length,
           line = 2),
      "\n")
  cat(ui_color_start("\\/",
                     " ",
                     description),
      "\n")
  cat(rule(width = length),
      "\n\n")
}

#' @export
#' @importFrom cli rule combine_ansi_styles
ui_end <- function(function_name, length = 100) {
  cat("\n\n")
  cat(rule(right = paste0(combine_ansi_styles("#10aff2")("End of function: "),
                          ui_color_start(function_name,
                                         "()")),
           line = 2,
           width = length),
      "\n\n")
}

#' @export
#' @importFrom cli combine_ansi_styles
ui_color_error <- function(...){
  combine_ansi_styles("red",
                      "bold")(...)
}

#' @importFrom cli cat_bullet
ui_error_item <- function(...){
  ui_color_error("\n => ",
                 ...)
}

#' @export
#' @importFrom cli combine_ansi_styles symbol
ui_error_list <- function(..., header = NULL){
  if (not_null(header)) {
    cat("\n",
        ui_color_error("\n",
                       symbol$cross,
                       " ",
                       header),
        ui_error_item(...),
        "\n\n")
  } else {
    cat("\n",
        ui_error_item(...),
        "\n\n")
  }
}

#' @export
#' @importFrom cli combine_ansi_styles symbol
ui_error <- function(...){
  cat("\n",
      ui_color_error("\n",
                     symbol$cross,
                     " ",
                     ...),
      "\n\n")
}

#' @export
#' @importFrom cli combine_ansi_styles
ui_color_info <- function(...){
  combine_ansi_styles("#4A80A3",
                      "bold")(...)
}

#' @importFrom cli cat_bullet
ui_info_item <- function(...){
  ui_color_info("\n -> ",
                ...)
}

#' @export
#' @importFrom cli combine_ansi_styles symbol
ui_info_list <- function(..., header = NULL){
  if (not_null(header)) {
    cat("\n",
        ui_color_info("\n",
                      symbol$info,
                      " ",
                      header),
        ui_info_item(...),
        "\n\n")
  } else {
    cat("\n",
        ui_info_item(...),
        "\n\n")
  }
}

#' @export
#' @importFrom cli combine_ansi_styles symbol
ui_info <- function(...){
  cat("\n",
      ui_color_info("\n",
                    symbol$info,
                    " ",
                    ...),
      "\n\n")
}

#' @export
#' @importFrom cli combine_ansi_styles
ui_color_warning <- function(...){
  combine_ansi_styles("#9897c7",
                      "bold")(...)
}

#' @importFrom cli cat_bullet
ui_warning_item <- function(...){
  ui_color_warning("\n -> ",
                   ...)
}

#' @export
#' @importFrom cli combine_ansi_styles symbol
ui_warning_list <- function(..., header = NULL){
  if (not_null(header)) {
    cat("\n",
        ui_color_warning("\n",
                         symbol$warning,
                         " ",
                         header),
        ui_warning_item(...),
        "\n\n")
  } else {
    cat("\n",
        ui_warning_item(...),
        "\n\n")
  }
}

#' @export
#' @importFrom cli combine_ansi_styles symbol
ui_warning <- function(...){
  cat("\n",
      ui_color_warning("\n",
                       symbol$warning,
                       " ",
                       ...),
      "\n\n")
}

#' @export
#' @importFrom cli combine_ansi_styles
ui_color_step <- function(...){
  combine_ansi_styles("#005387",
                      "bold")(...)
}

#' @importFrom cli cat_bullet
ui_step_item <- function(...){
  ui_color_step("\n -> ",
                ...)
}

#' @export
#' @importFrom cli combine_ansi_styles symbol
ui_step_list <- function(..., header = NULL){
  if (not_null(header)) {
    cat("\n",
        ui_color_step("\n",
                      symbol$arrow_right,
                      " ",
                      header),
        ui_step_item(...),
        "\n\n")
  } else {
    cat("\n",
        ui_step_item(...),
        "\n\n")
  }
}

#' @export
#' @importFrom cli combine_ansi_styles symbol
ui_step <- function(...){
  cat("\n",
      ui_color_step("\n",
                    symbol$arrow_right,
                    " ",
                    ...),
      "\n\n")
}

#' @export
#' @importFrom cli combine_ansi_styles
ui_color_success <- function(...){
  combine_ansi_styles("#09e2c5",
                      "bold")(...)
}

#' @importFrom cli cat_bullet
ui_success_item <- function(...){
  ui_color_success("\n -> ",
                   ...)
}

#' @export
#' @importFrom cli combine_ansi_styles symbol
ui_success_list <- function(..., header = NULL){
  if (not_null(header)) {
    cat("\n",
        ui_color_success("\n",
                         symbol$tick,
                         " ",
                         header),
        ui_success_item(...),
        "\n\n")
  } else {
    cat("\n",
        ui_success_item(...),
        "\n\n")
  }
}

#' @export
#' @importFrom cli combine_ansi_styles symbol
ui_success <- function(...){
  cat("\n",
      ui_color_success("\n",
                       symbol$tick,
                       " ",
                       ...),
      "\n\n")
}
