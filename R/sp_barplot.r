

#' Generating bar plot
#'
#' @param data Data frame or data file (with header line, the first column will not be treated as the rowname, tab seperated)
#' @param variable Name for legend column. Default 'variable'.
#' @param value Name for value column. Default 'value'.
#' @param xvariable Name for x-axis variable.
#' @param melted When true, it will skip melt preprocesses. But the format must be When true, it will skip melt preprocesses. But the format must be. Default FALSE, accept TRUE.
#' @param x_label Xlab label.Default NULL.
#' @param y_label Ylab label.Default NULL.
#' @param x_level Levels for x-axis variable, suitable when x-axis is not used as a number.
#' @param x_type The attribute of x-axis variable. Default TRUE, means X-axis label is text. FALSE means X-axis label is number.
#' @param scaleY Scale y axis. Default FALSE. Accept TRUE. If the supplied number after `y_add` is not 0, this parameter will be automatically TRUE.
#' @param y_add A number to add if scale is used. Default 0. If a non-zero number is given, `scaleY` is TRUE
#' @param scaleY_x If scale is TRUE, give the following scale_y_log10(), coord_trans(y="log10"), or other legal command for ggplot2.
#' @param facet Wrap plots by given column. This is used to put multiple plot in one picture. Used when `melted` is TRUE, normally a string set should be suitable for this parameter.
#' @param stat The ways to show the height of bars.
#' @param position The ways to place multiple bars for one group if there are. Multiple bars in same place will be stacked together by default.
#' @param facet_level The levels of wrapping to set the order of each group.
#' @param nrow 	The number of rows one want when `melted` is used. Default NULL.
#' @param ncol The number of columns one want when `melted` is used. Default NULL.
#' @param error_bar Error-bar column. Specify the column containing error bars.
#' @param base_font_size Font-size. Default 11.
#' @param par Other legal R codes for gggplot2 will be given here.
#' @param xtics Display xtics. Default TRUE.
#' @param xtics_angle Rotation angle for x-axis labels (anti clockwise), Default 0.
#' @param ytics Display ytics. Default FALSE.
#' @param color Manually set colors for each bar. Default FALSE, meaning using ggplot2 default.
#' @param color_v Color for each bar.
#' @param scales Paramter for scales for facet. Optional, only used when `melted` is given. Default each inner graph use same scale (x,y range). 'free','free_x','free_y' is accepted.
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
#' sp_barplot(data = bar_test_data, xvariable = "ID", value = "Expr", variable = "Gene")
#'
#' ## Not run:
#' bar_data = "bar.data"
#'
#' sp_barplot(data = bar_data, xvariable = "ID", value = "Expr", variable = "Gene")
#' ## End(Not run)
#'
sp_barplot <- function (data,
                        variable = 'Gene',
                        value = 'Expr',
                        xvariable = 'ID',
                        melted = TRUE,
                        title = NULL,
                        x_label = NULL,
                        y_label = NULL,
                        level_i = c(),
                        x_level = c(),
                        x_type = TRUE,
                        scaleY = FALSE,
                        y_add = 0,
                        scaleY_x = 'scale_y_log10()',
                        facet = 'NoMeAnInGTh_I_n_G_s',
                        stat = 'identity',
                        position = 'stack',
                        facet_level = NULL,
                        nrow = NULL,
                        ncol = NULL,
                        error_bar = 'ctCTct',
                        base_font_size = 11,
                        par = NULL,
                        legend.position = 'right',
                        xtics = TRUE,
                        xtics_angle = 0,
                        ytics = FALSE,
                        color = FALSE,
                        color_v = c(),
                        scales = 'fixed',
                        coordinate_flip = FALSE,
                        add_text = FALSE,
                        font_type = NULL,
                        debug = FALSE,
                        ...) {
  options(scipen = 999)

  if (!is.null(font_type)) {
    library(showtext)
    showtext.auto(enable = TRUE)
    font_path = font_type
    font_name = tools::file_path_sans_ext(basename(font_path))
    font.add(font_name, font_path)
  }

  if (class(data) == "character") {
    if (!melted) {
      data <- sp_readTable(data, row.names = NULL)
      rownames_data <- make.unique(as.character(data[, 1]))
      data <- data[,-1, drop = F]
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

  if (y_add != 0) {
    data[[value]] <- data[[value]] + y_add
    if (scaleY_x  == "log2") {
      if (is.numeric(data[[value]])) {
        data[[value]] <- log2(data[[value]])
        scaleY = FALSE
      } else {
        stop(
          "Only numbers are allowed in `value` column in given data matrix. Comma separated numbers are also not allowed."
        )
      }
    }
  }


  if (position  == "fill") {
    data <-
      data %>% group_by(xvariable) %>% mutate(count = sum(value)) %>% mutate(freq =
                                                                                 round(100 * value / count, 2))
  }

  if (length(level_i) > 1) {
    data[[variable]] <- factor(data[[variable]], levels = level_i)
  } else {
    if (!melted) {
      data[[variable]] <-
        factor(data[[variable]], levels = data_colnames,
               ordered = T)
    }
  }

  if (x_type) {
    if (length(x_level) > 1) {
      data[[xvariable]] <- factor(data[[xvariable]], levels = x_level)
    } else{
      if (!melted) {
        data[[xvariable]] <- factor(data[[xvariable]],
                                      levels = data_rownames, ordered = TRUE)
      }
    }
  }


  if (!is.null(facet_level)) {
    data[[facet]] <- factor(data[[facet]],
                              levels = facet_level, ordered = T)
  }

  xvariable_en = sym(xvariable)
  variable_en = sym(variable)
  value_en = sym(value)

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
    p <- p + theme_bw(base_family = font_name, base_size = base_font_size)
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
                label = sprintf("%.2f",!!value_en - error_bar),
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

  if (facet  != "NoMeAnInGTh_I_n_G_s") {
    p + facet_wrap(~  .data[[facet]] ,
                   nrow = nrow ,
                   ncol = ncol ,
                   scale = scales)
    # as.formula(paste0( "~",facet))
  }

  if (scaleY) {
    p <- p + scaleY_x
  }

  colorCount = length(unique(data[[variable]]))
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
