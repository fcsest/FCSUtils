#' @importFrom cli combine_ansi_styles
ui_color_start <- function(...){
  combine_ansi_styles("#10aff2",
                      "bold")(...)
}

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

#' @importFrom cli combine_ansi_styles
ui_step <- function(...){
  cat()
  cat("\n",
      combine_ansi_styles("#005387",
                          "bold")("=>",
                                  " ",
                                  ...),
      "\n")
}

#' @importFrom cli combine_ansi_styles symbol
ui_warning <- function(...){
  cat("\n\n")
  cat(combine_ansi_styles("#9897c7",
                          "bold")(symbol$warning,
                                  " ",
                                  ...,
                                  "\n\n"))
}

#' @importFrom cli combine_ansi_styles symbol
ui_info <- function(...){
  cat("\n\n")
  cat(combine_ansi_styles("#4A80A3",
                          "bold")(symbol$info,
                                  " ",
                                  ...),
      "\n\n")
}

#' @importFrom cli combine_ansi_styles
ui_error <- function(...){
  cat("\n\n")
  cat(combine_ansi_styles("red",
                          "bold")(symbol$cross,
                                  " ",
                                  ...,
                                  "\n\n"))
}

#' @importFrom cli combine_ansi_styles
ui_error_list <- function(head, ...){
  cat("\n\n")
  cat(combine_ansi_styles("red",
                          "bold")(symbol$cross,
                                  " ",
                                  head),
      ...)
}

#' @importFrom cli cat_bullet
ui_error_item <- function(...){
  combine_ansi_styles("red",
                      "bold")("\n -> ",
                              ...)
}

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

#' @importFrom cli combine_ansi_styles style_bold symbol
ui_success <- function(...){
  cat("\n\n")
  cat(combine_ansi_styles("#09e2c5")(style_bold(symbol$tick),
                                     " ",
                                     ...,
                                     "\n\n"))
}
