#' @importFrom clipr write_clip
#' @importFrom littleboxes dd
boxes <- function(texto, level = 1, length = 100) {
  if (level == 1) {
    c("#", rep("=", length - 2), "#\n",
      "##", rep(" ", length - 4), "##\n",
      "###", rep(" ", length - 6), "###\n",
      dd(texto, length), "\n",
      "###", rep(" ", length - 6), "###\n",
      "##", rep(" ", length - 4), "##\n",
      "#", rep("=", length - 2), "#\n") %>%
      paste(collapse = "") %>%
      write_clip()
  } else if (level == 2) {
    c("#", rep("=", length - 2), "#\n",
      "##", rep(" ", length - 4), "##\n",
      dd(paste0("*", texto), length), "\n",
      "##", rep(" ", length - 4), "##\n",
      "#", rep("=", length - 2), "#\n") %>%
      paste(collapse = "") %>%
      write_clip()
  } else if (level == 3) {
    c("#", rep("=", length - 2), "#\n",
      dd(paste0("**", texto), length), "\n",
      "#", rep("=", length - 2), "#\n") %>%
      paste(collapse = "") %>%
      write_clip()
  } else if (level == 4) {
    c("#", rep("=", round((length * 3)/5) - 2), "#\n",
      dd(paste0("***", texto), round((length * 3)/5)), "\n",
      "#", rep("=", round((length * 3)/5) - 2), "#\n") %>%
      paste(collapse = "") %>%
      write_clip()
  } else {
    return("ERROR: Invalid level!")
  }
}

#' @importFrom clipr write_clip
hrule <- function(length = 100, clip = FALSE) {
  paste(c("#",
          rep("=",
              length - 2),
          "#"),
        collapse = "") %>%
    {if (clip)
      {write_clip(.)}
      else if (!clip)
        {.}
    }
}

#' @importFrom clipr write_clip
hhrule <- function(length = 100, clip = FALSE) {
  paste(c("#",
          rep("=",
              round((length * 3)/5) - 2),
          "#"),
        collapse = "") %>%
    {if (clip)
    {write_clip(.)}
      else if (!clip)
      {.}
    }
}
