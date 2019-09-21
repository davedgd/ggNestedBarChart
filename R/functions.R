library(ggplot2)
library(grid)
library(gtable)
library(ggpubr)
library(dplyr)
library(purrr)

ggNestedBarChart <- function (p) {

  gp <- ggplotGrob(p)

  # adjust bar sizes (thanks to Z.Lin @ https://stackoverflow.com/questions/52341385/how-to-automatically-adjust-the-width-of-each-facet-for-facet-wrap/52341665)
  panels <- gp$layout$l[grep("panel", gp$layout$name)]
  xVars <- sapply(ggplot_build(p)$layout$panel_scales_x, function(x) length(x$range$range))
  gp$widths[panels] <- gp$widths[panels] * xVars

  # collect strip texts
  stripTexts <- grep("strip", gp$layout$name)

  # build list of strip labels
  labels <- lapply(1:3, function (x) map_chr(gp$grobs[stripTexts], ~ .$grobs[[x]]$children[[2]]$children[[1]]$label))

  # find strip label ranges
  nestingRanges <- lapply(1:(length(labels) - 1), function (x) {

    labelLevels <- apply(data.frame(labels[1:x], stringsAsFactors = FALSE), 1, paste, collapse = " ")

    calcRuns <- rle(labelLevels)

    values <- calcRuns$values
    lengths <- calcRuns$lengths
    ends <- cumsum(lengths)
    x <- data.frame(min = ends - lengths + 1,
                    max = ends,
                    label = values,
                    level = x,
                    stringsAsFactors = FALSE)

  }) %>% bind_rows()

  # substitute strips
  numLevels <- n_distinct(nestingRanges$level)

  for (j in numLevels:1) {

    labelRanges <- subset(nestingRanges, level == j)

    for (i in 1:nrow(labelRanges)) {

      currentFacet <- labelRanges[i,]

      tempbuildReplacementFacet <- gp$grobs[stripTexts][[currentFacet$min]]
      currentGrob <- tempbuildReplacementFacet[j,] # the label of interest

      # this is a bit hacky...
      buildReplacementFacet <- tempbuildReplacementFacet[1,]

      while (nrow(buildReplacementFacet) + 1 < j * 2)
        buildReplacementFacet <- rbind(buildReplacementFacet, tempbuildReplacementFacet[1,])

      buildReplacementFacet <- rbind(buildReplacementFacet, currentGrob)

      for (k in (length(buildReplacementFacet) - 1):1)
        buildReplacementFacet$grobs[k] <- list(rectGrob(gp = gpar(col = NA)))

      l <- gp$layout[stripTexts[currentFacet$min],]
      r <- gp$layout[stripTexts[currentFacet$max],]

      gp <- gtable_add_grob(gp, buildReplacementFacet, t = l$t - numLevels, l = l$l, r = r$r)

    }

  }

  gp$layout

  return(as_ggplot(gp))

}
