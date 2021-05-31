#----------------------------------------------#
# Importing globalVariables from utils package #
#----------------------------------------------#
#' @importFrom utils globalVariables
#---------------------------------------------#
# Setting global variables of other functions #
#---------------------------------------------#
globalVariables(c("."))

#' @importFrom magrittr %>%
NULL

#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)
