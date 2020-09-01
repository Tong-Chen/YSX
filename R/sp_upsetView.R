#' Generating upsetView plot
#'
#' Input file is a matrix:

#' vennFormat 0
#'
#' (First row would be treated as header line. First column is just a normal column (but needed). 0 represents the sample does not contain the genes in row. 1 represents the containing relationship)

#' ID	Samp1	Samp2	Samp3	Samp4	Samp5
#'
#' G1	1	0	1	0	1
#'
#' G2	0	0	1	1	1
#'
#' G3	1	1	1	0	1
#'
#' G4	1	1	1	0	0
#'
#' G5	0	1	0	1	1
#'
#' G6	1	0	1	0	0

#' vennFormat 1 or 2
#'
#' The output contains two barplots, horizontal bar represents the number of genes in each sample, which is the sum of all 1 in sample column. Vertical bar represents the number of sample specific and common genes as indicated by linking vertical lines and points (just as the overlapping regions of venndiagram).


#'
#' @param data Data file. Receive long and wide table forms.
#' @param vennFormat Venn diagram format without header line. Default 0 represents normal data. Accept 1,2.
#' 0: represents wide data listed above.
#' 1: represents venn diagram format without header line.
#' 2: represents venn diagram format with header line.
#' @param pointsize Point size. Default 8.
#' @param keep_empty Keep empty intersections. Default FALSE. Accept TRUE to remove empty intersections.
#' @inheritParams base_plot_save
#' @param ...
#'
#' @return A pdf file.
#' @export
#'
#' @examples
#'
#' upsetview_data <- data.frame(elements=c("1","2","2","2","3"), sets=c("A","A","B","C","C"))
#' sp_upsetview(data = upsetview_data, vennFormat=2, saveplot = "upsetView_long.pdf")
#'
#'
#' ## Not run:
#' upsetview_data = "upsetview.data"
#' sp_upsetview(data = upsetview_data, saveplot = "upsetView_wide.pdf")
#' ## End(Not run)
#'
sp_upsetview <- function (data,
                          vennFormat = 0,
                          pointsize = 8,
                          keep_empty = FALSE,
                          saveplot=NULL,
                          debug = FALSE,
                          ...) {

  if (debug) {
    argg <- c(as.list(environment()), list(...))
    print(argg)
  }


  if (vennFormat == 0) {
    if (class(data) == "character") {
      data <- sp_readTable(data, row.names = NULL)
    } else if (class(data) != "data.frame") {
      stop("Unknown input format for `data` parameter.")
    }
    data[, -1][data[, -1] != 0] <- 1
    data[, -1][data[, -1] == 0] <- 0
  } else {
    header = ifelse(vennFormat == 1, F, T)
    if (class(data) == "character") {
      data <- sp_readTable(data, row.names = NULL, header = header)
    } else if (class(data) != "data.frame") {
      stop("Unknown input format for `data` parameter.")
    }
    data <- unique(data[, 1:2])
    colnames(data) <- c("Item", "Grp")
    data = as.data.frame(reshape2::acast(data, Item ~ Grp, length))
    data = cbind(ID = rownames(data), data)
  }

  nsets = dim(data)[2] - 1

  if (keep_empty) {
    keep_empty = 'on'
  } else {
    keep_empty = NULL
  }

  a = UpSetR::upset(
    data,
    nsets = nsets,
    nintersects = NA,
    sets.bar.color = "#56B4E9",
    order.by = "freq",
    empty.intersections = keep_empty
  )


  if (!sp.is.null(saveplot)) {
    base_plot_save(saveplot, pointsize=point_size, ...)
    print(a)
    dev.off()
  }
  a
}

