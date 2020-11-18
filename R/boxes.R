#' @export
dd <- function(x, l = 60) {
  if (nchar(x) <= (l - 10)) {
    ((l - nchar(x) - 2) / 2) %>%
      {c("####",
         rep(" ",
             floor(.) - 3),
         x,
         rep(" ",
             ceiling(.) - 3),
         "####")} %>%
      paste(collapse = "")
  } else if (nchar(x) > (l - 10)) {
    s1 <- strsplit(x, "\\s") %>% unlist()

    # si s1 trop long on découpe arbitraitement au milieu
    if (length(s1) == 1) {
      coupe <- nchar(s1) / 2
      s1 <- c(substr(s1,
                     1,
                     coupe),
              substr(s1,
                     1 + coupe,
                     nchar(s1)))
    }

    c(Recall(paste(s1[1:floor(length(s1)/2)],
                   collapse = " "),
             l = l),
      Recall(paste(s1[(1+floor(length(s1)/2)):length(s1)],
                   collapse = " "),
             l = l))
  }
}

#' @export
#' @importFrom clipr write_clip
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

#' @export
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

#' @export
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
