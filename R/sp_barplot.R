


#' Generating bar plot
#'
#' @param data Data frame or data file (with header line, the first column will not be treated as
#' row names, tab seperated)
#' @param color_variable Name for specify bars colors. Default 'variable'.
#' @param yvariable Name for value column. Default 'value'.
#' @param xvariable Name for x-axis variable.
#' @param melted `TRUE` for dealinig with long format matrix, the program will skip melt preprocess.
#' Default `FALSE` for dealing with wide format matrix.
#' @param x_label Xlab title Default NULL.
#' @param y_label Ylab title. Default NULL.
#' @param xvariable_order Levels for x-axis variable, suitable when x-axis is not used as a number.
#' @param stat The ways to show the height of bars.
#' The height of bars represent the numerical values in each group by default (normally in `yvariable`
#' column of melted data).
#' One can also give `count` to let the program count the number of
#' items in each group (Normally the `color_variable` column is used to group
#' 'xvariable' colum after melt).
#' Or one can give `weight` which will sum values of each group.
#' Default `identity`, accept `count` when categorical data are given.
#' @param position The ways to place multiple bars for one group if there are.
#' Multiple bars in same place will be stacked together by default.
#' Giving `fill` to get stacked percent barplot.
#' Giving `dodge` to arrange multiple bars side-by-side.
#' Default `stack`, accept `dodge`, `fill`.
#' @inheritParams sp_ggplot_facet
#' @inheritParams sp_transfer_one_column
#' @param error_bar Error-bar column. Specify the column containing error bars.
#' @param base_font_size Font-size. Default 11.
#' @param par Other legal R codes for ggplot2 will be given here.
#' @param xtics Display xtics. Default TRUE.
#' @param ytics Display ytics. Default FALSE.
#' @param color Manually set colors for each bar. Default FALSE, meaning using ggplot2 default.
#' @param color_v Color for each bar.
#' @param add_text 	Add text to bar. Default FALSE.
#' @param font_type Specify font type. Give a path for one font type file like '/etc/fonts/Arial.ttf'
#' or 'HeiArial.ttc'(if in current directory), Default system default.
#' @inheritParams sp_ggplot_layout
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
                        color_variable = 'Gene',
                        yvariable = 'Expr',
                        xvariable = 'ID',
                        melted = TRUE,
                        title = NULL,
                        x_label = NULL,
                        y_label = NULL,
                        level_i = c(),
                        xvariable_order = c(),
                        y_add = 0,
                        yaxis_scale_mode = NULL,
                        facet_variable = NULL,
                        stat = 'identity',
                        position = 'stack',
                        facet_variable_order = NULL,
                        facet_nrow = NULL,
                        facet_ncol = NULL,
                        error_bar = 'ctCTct',
                        base_font_size = 11,
                        par = NULL,
                        legend.position = 'right',
                        xtics = TRUE,
                        xtics_angle = 0,
                        ytics = FALSE,
                        color = FALSE,
                        color_v = c(),
                        facet_scales = 'fixed',
                        coordinate_flip = FALSE,
                        add_text = FALSE,
                        font_type = NULL,
                        debug = FALSE,
                        ...) {
  options(scipen = 999)

  if (!sp.is.null(font_type)) {
    library(showtext)
    showtext.auto(enable = TRUE)
    font_name = tools::file_path_sans_ext(basename(font_type))
    font.add(font_name, font_type)
  }

  if (class(data) == "character") {
    if (!melted) {
      data <- sp_readTable(data, row.names = NULL)
      rownames_data <- make.unique(as.character(data[, 1]))
      data <- data[, -1, drop = F]
      rownames(data) <- rownames_data
      if (all(apply(data, 2, numCheck))) {
        rownames_data <- rownames(data)
        data <- as.data.frame(apply(data, 2, mixedToFloat))
        data <- as.data.frame(data)
        rownames(data) <- rownames_data
      } else {
        stop(
          "For wide format data matrix, all elements except the first row and column must be numbers unless long format is used."
        )
      }
      data_rownames <- rownames(data)
      data_colnames <- colnames(data)
      data[[xvariable]] <- data_rownames
      data <- melt(data, id.vars = xvariable)
    } else {
      data <- sp_readTable(data, row.names = NULL)
    }
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

  if (y_add != 0) {
    data[[yvariable]] <- data[[yvariable]] + y_add
    if (scaleY_x  == "log2") {
      if (is.numeric(data[[yvariable]])) {
        data[[yvariable]] <- log2(data[[yvariable]])
        scaleY = FALSE
      } else {
        stop(
          "Only numbers are allowed in `yvariable` column in given data matrix. Comma separated numbers are also not allowed."
        )
      }
    }
  }


  if (position  == "fill") {
    data <-
      data %>% group_by(xvariable) %>% mutate(count = sum(yvariable)) %>% mutate(freq =
                                                                               round(100 * yvariable / count, 2))
  }

  if (length(level_i) > 1) {
    data[[color_variable]] <- factor(data[[color_variable]], levels = level_i)
  } else {
    if (!melted) {
      data[[color_variable]] <-
        factor(data[[color_variable]], levels = data_colnames,
               ordered = T)
    }
  }

  if (x_type) {
    if (length(xvariable_order) > 1) {
      data[[xvariable]] <- factor(data[[xvariable]], levels = xvariable_order)
    } else{
      if (!melted) {
        data[[xvariable]] <- factor(data[[xvariable]],
                                    levels = data_rownames, ordered = TRUE)
      }
    }
  }


  if (!is.null(facet_variable_order)) {
    data[[facet_variable]] <- factor(data[[facet_variable]],
                            levels = facet_variable_order, ordered = T)
  }

  xvariable_en = sym(xvariable)
  variable_en = sym(color_variable)
  value_en = sym(yvariable)

  if (stat == "count") {
    p <- ggplot(data, aes(x = !!xvariable_en, group = !!variable_en))
  } else {
    p <-
      ggplot(data,
             aes(
               x = !!xvariable_en,
               y = !!value_en,
               group = !!variable_en
             ))
  }

  if (!is.null(font_type)) {
    p <-
      p + theme_bw(base_family = font_name, base_size = base_font_size)
  } else {
    p <- p + theme_bw(base_size = base_font_size)
  }

  p <-
    p + theme(axis.ticks.x = element_blank(), legend.key = element_blank())

  p <-
    p + geom_bar(
      stat = stat ,
      position = position ,
      aes(fill = !!variable_en),
      width = 0.75
    )

  if (error_bar  != "ctCTct") {
    p <-
      p + geom_errorbar(
        aes(
          ymin = !!value_en - error_bar,
          ymax = !!value_en + error_bar
        ),
        colour = "black",
        width = 0.2,
        position = position_dodge(width = .75)
      )
  }


  if (position  == "fill") {
    p <- p + scale_y_continuous(labels = scales::percent)
  }

  if (add_text) {
    text_size =  base_font_size / 3.2
    if (position  == "dodge") {
      width_dodge = 0.75
      if (font_type != "FALSE") {
        if (error_bar  != "ctCTct") {
          p <-
            p + geom_text(
              aes(
                label = sprintf("%.2f", !!value_en - error_bar),
                y = !!value_en - error_bar
              ),
              vjust = 1.5,
              position = position_dodge(width = width_dodge),
              size = text_size,
              show.legend = F
            ) + geom_text(
              aes(
                label = sprintf("%.2f", !!value_en + error_bar),
                y = !!value_en + error_bar
              ),
              vjust = -.5,
              position = position_dodge(width = width_dodge),
              size = text_size,
              family = font_name,
              show.legend = F
            )
        } else {
          p <-
            p + geom_text(
              aes(label = !!value_en),
              position = position_dodge(width = width_dodge),
              size = text_size,
              family = font_name,
              show.legend = F
            )
        }
      } else {
        if (error_bar  != "ctCTct") {
          p <-
            p + geom_text(
              aes(
                label = sprintf("%.2f", !!value_en - error_bar),
                y = !!value_en - error_bar
              ),
              vjust = 1.5,
              position = position_dodge(width = width_dodge),
              size = text_size,
              show.legend = F
            ) + geom_text(
              aes(
                label = sprintf("%.2f", !!value_en + error_bar),
                y = !!value_en + error_bar
              ),
              vjust = -.5,
              position = position_dodge(width = width_dodge),
              size = text_size,
              show.legend = F
            )
        } else {
          p <-
            p + geom_text(
              aes(label = !!value_en),
              position = position_dodge(width = width_dodge),
              size = text_size,
              show.legend = F
            )
        }
      }
    } else if (position  == "stack") {
      if (font_type  != "FALSE") {
        p <-
          p + geom_text(
            aes(label = !!value_en),
            position = position_stack(vjust = 0.5),
            size = text_size,
            family = font_name,
            show.legend = F
          )
      } else {
        p <-
          p + geom_text(
            aes(label = !!value_en),
            position = position_stack(vjust = 0.5),
            size = text_size,
            show.legend = F
          )
      }
    } else if (position == "fill") {
      if (font_type  != "FALSE") {
        p <-
          p + geom_text(
            aes(label = freq),
            position = position_fill(vjust = 0.5),
            size = text_size,
            family = font_name,
            show.legend = F
          )
      } else {
        p <-
          p + geom_text(
            aes(label = freq),
            position = position_fill(vjust = 0.5),
            size = text_size,
            show.legend = F
          )
      }
    }
  }

  if (facet_variable  != "NoMeAnInGTh_I_n_G_s") {
    p + facet_wrap( ~  .data[[facet_variable]] ,
                    nrow = nrow ,
                    ncol = ncol ,
                    scale = scales)
    # as.formula(paste0( "~",facet_variable))
  }

  if (scaleY) {
    p <- p + scaleY_x
  }

  colorCount = length(unique(data[[color_variable]]))
  if (color) {
    p <- p + scale_fill_manual(values = color_v)
  } else {
    p <-
      p + scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(colorCount))
  }

  if (!xtics) {
    p <-
      p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  } else{
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
      ...
    )
  }

  if (ytics) {
    p <- p + theme(axis.text.y = element_blank())
  }

  p <- p + par

  if (debug) {
    argg <- c(as.list(environment()), list(...))
    print(argg)
  }
  p
}
