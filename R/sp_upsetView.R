#' Generating upsetView plot
#'
#' @param data Data file. Receive long and wide table forms.
#' @param vennFormat Venn diagram format without header line. Default 0 represents normal data. Accept 1,2.
#' 0: represents wide data listed above.
#' 1: represents venn diagram format without header line.
#' 2: represents venn diagram format with header line.
#' @param pointsize Point size. Default 8.
#' @param keep_empty Keep empty intersections. Default TRUE. Accept FALSE to remove empty intersections.
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
                          keep_empty = TRUE,
                          saveplot="1.pdf",
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
    data = as.data.frame(acast(data, Item ~ Grp, length))
    data = cbind(ID = rownames(data), data)
  }

  nsets = dim(data)[2] - 1

  if (keep_empty) {
    keep_empty = 'on'
  } else {
    keep_empty = NULL
  }

    # pdf(
    #   file = "upset.pdf",
    #   onefile = FALSE,
    #   paper = "special",
    #   bg = "white",
    #   pointsize = 8
    # )
#
  if (!is.null(saveplot)) {
    print(saveplot)
    if(dev.cur()!=1){
      dev.off()
    }
    base_plot_save(saveplot,...)

  }
  print(data)
  print(nsets)

  # pdf(saveplot,...)


  # return(list(data=data, nsets=nsets))
  upset(
    data,
    nsets = nsets,
    nintersects = NA,
    sets.bar.color = "#56B4E9",
    order.by = "freq",
    empty.intersections = keep_empty
  )


  if (!is.null(saveplot)) {
    dev.off()
  }
}

#sp_upsetview(data="vignettes/upsetview.data", saveplot="2.pdf")
