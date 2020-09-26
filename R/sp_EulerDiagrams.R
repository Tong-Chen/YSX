sp_EulerDiagrams <- function (data,
                              intersection_variable = "Intersection",
                              count_variable = "Count",
                              type = c("percent"),
                              shape = "ellipse",
                              color = NULL,
                              legend.position = "right",
                              font_quantities = 1,
                              lty = c(2),
                              labels_font = c(1:3),
                              saveplot = NULL,
                              ...) {

  # library(eulerr)
  # 最基本的输入格式判断不能落下
  if (class(data) == "character") {
    data <- sp_readTable(data, row.names = NULL, header=T)
  } else if (class(data) != "data.frame") {
    stop("Unknown input format for `data` parameter.")
  }

  setName = unique(unlist(strsplit(data[[intersection_variable]],'&')))

  setnumber <- length(setName)

  intersection_count <- data[[count_variable]]

  names(intersection_count) = data[[intersection_variable]]

  Euler <- euler(c(setnumber), shape = shape)

  if (!sp.is.null(color)) {
    fill_color = color
  } else {
    fill_color = list()
  }

  # fill_color <-  generate_color_list(manual_color_vector, number_fill_color,
  #                     alpha = alpha)

  a <- plot(
    Euler,
    quantities = list(type = type, font = font_quantities),
    fill = fill_color,
    legend = list(side = legend.position),
    edges = list(lty = lty),
    labels = list(font = labels_font)
  )



  if (!is.null(saveplot)) {
    base_plot_save(saveplot, ...)
    print(a)
    dev.off()
  }

}
