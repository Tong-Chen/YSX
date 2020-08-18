



#' Generating bar plot
#'
#' @param data Data frame or data file (with header line, the first column will
#' not be treated as row names, tab seperated).
#' @param melted `TRUE` for dealinig with long format matrix, the program will skip melt preprocess. If input is wide format matrix, this parameter should be set to `FALSE`.
#' @param xvariable Name for x-axis variable (one of colum names, should be specified
#' when inputinh long format matrix).
#' @param color_variable Name for specifying bars colors (one of colum names, should be specified
#' when inputinh long format matrix).
#' @param color_variable_order Set orders of color variable (this can also used to extract specific rows).
#' @param yvariable Name for value column (one of colum names, should be specified
#' when inputinh long format matrix).
#' @param xvariable_order Levels for x-axis variable, suitable when x-axis is not used as a number.
#' @param stat The ways to show the height of bars.
#' The height of bars represent the numerical values in each group by default (normally in `yvariable` column of melted data).
#' One can also give `count` to let the program count the number of
#' items in each group (Normally the `color_variable` column is used to group
#' 'xvariable' colum after melt).
#' Or one can give `weight` which will sum values of each group.
#' Default `identity`, accept `count` when categorical data are given.
#' @param bar_mode The ways to place multiple bars for one group.
#' Multiple bars in same place will be stacked together by default.
#' Giving `fill` to get stacked percent barplot.
#' Giving `dodge` to arrange multiple bars side-by-side.
#' Default `stack`, accept `dodge`, `fill`.
#' @inheritParams sp_ggplot_facet
#' @inheritParams sp_transfer_one_column
#' @param error_bar_variable Error-bar column (one of column names). Specify the column containing error bars.
#' @param base_font_size Font-size. Default 11.
#' @param extra_ggplot2_cmd Other legal R codes for ggplot2 will be given here.
#' @param xtics Display xtics. Default TRUE.
#' @param ytics Display ytics. Default FALSE.
#' @param add_text 	Add text to bar. Default FALSE.
#' @inheritParams sp_load_font
#' @inheritParams sp_ggplot_layout
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @param ...
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#'
#' bar_test_data <- data.frame(ID = letters[1:4], Gene = letters[c(8,8,9,9,10,10,11,11)], Expr = runif(16))
#' sp_barplot(data = bar_test_data, xvariable = "ID", yvariable = "Expr", color_variable = "Gene")
#'
#' ## Not run:
#' bar_data = "bar.data"
#'
#' sp_barplot(data = bar_data, xvariable = "ID", yvariable = "Expr", color_variable = "Gene")
#' ## End(Not run)
#'
sp_barplot <- function (data,
                        color_variable = NULL,
                        yvariable = NULL,
                        xvariable = NULL,
                        melted = TRUE,
                        title = NULL,
                        x_label = NULL,
                        y_label = NULL,
                        color_variable_order = NULL,
                        xvariable_order = NULL,
                        y_add = 0,
                        yaxis_scale_mode = NULL,
                        facet_variable = NULL,
                        stat = 'identity',
                        bar_mode = 'stack',
                        facet_variable_order = NULL,
                        facet_nrow = NULL,
                        facet_ncol = NULL,
                        error_bar_variable = NULL,
                        base_font_size = 11,
                        legend.position = 'right',
                        xtics = TRUE,
                        xtics_angle = 0,
                        ytics = TRUE,
                        manual_color_vector = "Set2",
                        facet_scales = 'fixed',
                        extra_ggplot2_cmd = NULL,
                        coordinate_flip = FALSE,
                        add_text = FALSE,
                        font_path = NULL,
                        debug = FALSE,
                        ...) {
  options(scipen = 999)

  if (debug) {
    argg <- c(as.list(environment()), list(...))
    print(argg)
  }

  fontname = sp_load_font(font_path = font_path)


  if (melted) {
    if (sp.is.null(xvariable) || sp.is.null(yvariable)) {
      stop("For melted matrix, <xvariable> and <yvariable> should be supplied.")
    }
  } else {
    xvariable = 'xvariable'
    yvariable = 'value'
    color_variable = 'variable'
  }

  data <- sp_read_in_long_wide_matrix(data, xvariable, melted)

  #print(data)

  wide_rownames <- data$wide_rownames
  wide_colnames <- data$wide_colnames
  data <- data$data
  data_colnames <- colnames(data)

  if (!(xvariable %in% data_colnames &&
        yvariable %in% data_colnames)) {
    stop(paste(xvariable, 'or', yvariable, 'must be one of column names of data!'))
  }

  if (!sp.is.null(yaxis_scale_mode)) {
    data <-
      sp_transfer_one_column(
        data,
        variable = yvariable,
        yaxis_scale_mode = yaxis_scale_mode,
        y_add = y_add
      )
  }

  #print(data)

  xvariable_en = sym(xvariable)
  color_variable_en = sym(color_variable)
  yvariable_en = sym(yvariable)

  if (bar_mode  == "fill" && add_text) {
    data <-
      data %>% group_by(!!xvariable_en) %>%
      mutate(count = sum(!!yvariable_en)) %>%
      mutate(freq = round(100 * !!yvariable_en / count, 2))
  }

  if (!melted){
    xvariable_order = wide_rownames
    color_variable_order = wide_colnames
  }

  data = sp_set_factor_order(data, xvariable, xvariable_order)

  #print(data)

  if (!sp.is.null(color_variable) && color_variable != xvariable) {
    if (!(color_variable %in% data_colnames)) {
      stop(paste(color_variable,'must be one of column names of data!'))
    }
    data = sp_set_factor_order(data, color_variable, color_variable_order)
  } else {
    color_variable = variable
  }

  #print(data)

  if (!sp.is.null(facet_variable)) {
    if (!(facet_variable %in% data_colnames)) {
      stop(paste(facet_variable,'must be one of column names of data!'))
    }
    data = sp_set_factor_order(data, facet_variable, facet_variable_order)
  }



  xvariable_en = sym(xvariable)
  color_variable_en = sym(color_variable)
  yvariable_en = sym(yvariable)

  #print(data)

  if (stat == "count") {
    p <- ggplot(data, aes(x = !!xvariable_en, group = !!yvariable_en))
  } else {
    p <-
      ggplot(data,
             aes(
               x = !!xvariable_en,
               y = !!yvariable_en,
               group = !!color_variable_en
             ))
  }


  p <-
    p + geom_bar(
      stat = stat ,
      position = bar_mode ,
      aes(fill = !!color_variable_en),
      width = 0.75
    )

  if (!sp.is.null(error_bar_variable)) {
    if (!(error_bar_variable %in% data_colnames)) {
      stop(paste(error_bar_variable,'must be column names of data!'))
    }

    error_bar_variable_en = sym(error_bar_variable)
    p <-
      p + geom_errorbar(
        aes(
          ymin = !!yvariable_en - !!error_bar_variable_en,
          ymax = !!yvariable_en + !!error_bar_variable_en
        ),
        colour = "black",
        width = 0.2,
        position = position_dodge(width = .75)
      )
  }


  if (bar_mode  == "fill") {
    p <- p + scale_y_continuous(labels = scales::percent)
  }

  if(add_text){
    text_size =  base_font_size / 3.2
    geom_text_parameter <- list()

    if (bar_mode  == "dodge") {
      width_dodge = 0.75
      geom_text_parameter$position = position_dodge(width = width_dodge)
    }else if (bar_mode  == "stack") {
      geom_text_parameter$position = position_stack(vjust = 0.5)
    }else if (bar_mode  == "fill") {
      geom_text_parameter$position = position_fill(vjust = 0.5)
    }

    if(!sp.is.null(fontname)){
      geom_text_parameter$famliy = fontname
    }

    geom_text_parameter$size = text_size
    geom_text_parameter$show.legend = F

    if(sp.is.null(error_bar_variable)){
      sp_geom_text <- function(...){
        ggplot2::geom_text(mapping=aes(label = !!yvariable_en), ...)
      }
      p <-
        p + do.call(sp_geom_text, c(geom_text_parameter))
    } else {
      sp_geom_text1 <- function(...){
        geom_text(mapping=aes(
          label = sprintf("%.2f", !!yvariable_en - !!error_bar_variable_en),
          y = !!yvariable_en - !!error_bar_variable_en
        ),
        vjust = 1.5, ...)
      }
      sp_geom_text2 <- function(...){
        geom_text(mapping=aes(
          label = sprintf("%.2f", !!yvariable_en + !!error_bar_variable_en),
          y = !!yvariable_en + !!error_bar_variable_en
        ),
        vjust = .5, ...)
      }
      p <-
        p + do.call(sp_geom_text1, c(geom_text_parameter)) +
        do.call(sp_geom_text2, c(geom_text_parameter))
    }
  }


  if (!sp.is.null(facet_variable)) {
    p <-
      sp_ggplot_facet(p, facet_variable, facet_ncol, facet_nrow, facet_scales)
  }

  if (!sp.is.null(yaxis_scale_mode) &&
      (yaxis_scale_mode  != "log2") &&
      (yaxis_scale_mode  != "log10")) {
    p <- p +  eval(parse(text = yaxis_scale_mode))
  }


  p <-
    sp_manual_fill_ggplot2(p, data, color_variable, manual_color_vector)

  additional_theme <- list()

  if (!xtics) {
    additional_theme$axis.text.x = element_blank()
  }
  if (!ytics) {
    additional_theme$axis.text.y = element_blank()
  }

  additional_theme$axis.ticks.x = element_blank()
  additional_theme$legend.key  = element_blank()

  p <- sp_ggplot_layout(
      p,
      xtics_angle = 0,
      legend.position = legend.position,
      extra_ggplot2_cmd = NULL,
      file_name = NULL,
      title = title,
      x_label = x_label,
      y_label = y_label,
      coordinate_flip = FALSE,
      additional_theme = additional_theme,
      fontname = fontname,
      base_font_size = base_font_size,
      ...
    )

  p


}
