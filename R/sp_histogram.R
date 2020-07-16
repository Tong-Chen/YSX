
#' Generating histogram plot
#'
#' @param data Data frame or data file (with header line, the first column will not be treated as the rowname, tab seperated)
#' @param melted When FALSE, it will skip melt preprocesses.
#' @param yAxis_variable Specify the column name used as value variable.
#' @param legend_variable Specify the column name used as legend variable.
#' @param type_hist Plot with density or count or frequency in y-axis. Default frequency, accept density, count. When `type_p` is both, frequency will be given here.
#' @param type_p Plot frequency or density or hist or both frequency and histogram. Default line means frequency accept density_line, hist or both.
#' @param scale Scale value. All values will be divided by supplied one. Default 1.
#' @param level_i Levels for legend variable. Accept a string like "'ctcf','h3k27ac'".
#' @param facet Variable name for facet. Optional, the name of one column representing a group should be given if group
#' information is needed. Here 'set' can be given if you want to plot set-1 and set-2 separatelt.
#' @param facet_order_i  Order of s_facet (facet_variable given to `facet`). Optional even -s is given. Only specified if the facet order is what you want. Input format likes
#' "'facet1','facet2',"facet10"".
#' @param j_facet_order_i Order of J_facet (facet variable given to `j_facet`). Optional even -s is given. Only specified if the facet order is what you want. Input format likes
#' "'facet1','facet2',"facet10"".
#' @param largest The largest value allowed. Values other than this will be set as this value. Default Inf.
#' @param adjust The value for adjust (like the width of each bin) in geom_density, and binwidth for geom_histogram.
#' @param pos Position paramter for hist bars. Default identity, accept dodge.
#' @param alpha Alpha value for transparent. Default 0.4, accept a number form 0 to 1,  the smaller, the more transparent.
#' @param fill_area Fill the area if TRUE. Default FALSE.
#' @param line_size line size. Default 1. Accept a number.
#' @param vline Add mean value as vline. Default FALSE,  accept TRUE.
#' @param color_v Color for each line. Str in given
#' format must be supplied, ususlly the number of colors should
#' be equal to the number of lines.
#' "'red','pink','blue','cyan','green','yellow'" or
#' "rgb(255/255,0/255,0/255),rgb(255/255,0/255,255/255),rgb(0/255,0/255,255/255),
#' 		rgb(0/255,255/255,255/255),rgb(0/255,255/255,0/255),rgb(255/255,255/255,0/255)"
#' @param j_facet Another variable for facet. Optional, same meaning as `facet` but different columns, like 'variable'. When this is given,
#' facet_grid would be used in format like facet_grid(s_facet ~ j_facet) pay attention to the order.
#' @param facet_scale Parameter for scales for facet. Necessary if only `facet` is given. Default each inner graph use same scale (x, y range). "free", 'free_x', 'free_y' is accepted.
#' @param facet_ncol Number of columns in one row when do faceting. Necessary if only `facet` is given
#' @param xtics Show the X axis.
#' @param ytics Show the Y axis.
#' @param legend.position Position of legend, accept top, bottom, left, right, none or c(0.8,0.8).
#' @param xtics_angle Rotation angle for a-axis.
#' @param xtics_value Manually set the value of xtics when `xtics_v` is specified. Default the content of `xtics_v` when `xtics_v` is specified,
#'accept a series of numbers in following format "c(1,2,3,4,5)" or other R code that can generate a vector to set the position of xtics.
#' @param xtics_v Manually set xtics. Default FALSE, accept a series of
#' numbers in following format "c(1,2,3,4,5)" or other R code that can generate a vector.
#' @param custom_vline_coord Add vline by given point. Default FALSE, accept a series of numbers in following format "c(1,2,3,4,5)" or other R code that can generate a vector.
#' @param custom_vline_anno Add labels to vline. Default FALSE, accept a series of numbers in following format "c(1,2,3,4,5)" or other R code that can generate a vector.
#' @param x_label Xlab label.
#' @param y_label Ylab label.
#' @param title Title of picture.
#' @param ...
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' 
#' histogram_test_data <- data.frame(Type = letters[1:2], Value = runif(80))
#' sp_histogram(data = histogram_test_data, yAxis_variable = "Value", legend_variable = "Type",
#'  type_p = "both", type_hist = "count/sum(count)")
#' 
#' ## Not run:
#' input <- "histogram.data"
#' sp_histogram(data=input, yAxis_variable = "Value", legend_variable = "Type",
#'  type_p = "both", type_hist = "count/sum(count)")
#' ## End(Not run)

sp_histogram <- function(data ,
                         melted = FALSE,
                         yAxis_variable,
                         legend_variable ,
                         stat = "density",
                         scale = 1,
                         level_i = c(),
                         facet = "haha",
                         facet_order_i = c(),
                         j_facet_order_i = c(),
                         largest = "Inf",
                         type_hist = 'count/sum(count)',
                         type_p = 'line',
                         adjust = NULL,
                         pos = "identity",
                         alpha = 0.4,
                         fill_area = FALSE,
                         line_size = 1,
                         vline = FALSE,
                         color_v = c(),
                         j_facet = "haha",
                         facet_scale = 'fixed',
                         facet_ncol = 1,
                         xtics = TRUE,
                         ytics = TRUE,
                         legend.position = "right",
                         xtics_angle = 90,
                         xtics_value = 0,
                         xtics_v = 0,
                         custom_vline_coord = NULL,
                         custom_vline_anno = NULL,
                         x_label = NULL,
                         y_label = NULL,
                         title = '',
                         ...) {
  if (class(data) == "character") {
    if (melted) {
      #cat("Currently not supported for unmelted files")
      #quit()
      #currently this part is unused
      data <- sp_readTable(data, row.names = NULL)
      #	data_rownames <- rownames(data)
      #	data_colnames <- colnames(data)
      #	data\$${xvariable} <- data_rownames
      data <- melt(data, id.vars = xvariable)
      #data <- melt(data)
    } else {
      data <- sp_readTable(data, row.names = NULL)
    }
  }
  #legend_variable="${legend_variable}"
  #yAxis_variable="${yAxis_variable}"
  
  
  stat = "density"
  
  if (!is.numeric(data[[yAxis_variable]])) {
    stat = "count"
    stop("Value variable column must be numeric.")
  }
  
  if (scale != 1) {
    data[[yAxis_variable]] <- data[[yAxis_variable]] / scale
  }
  
  if (length(level_i) > 1) {
    data[[legend_variable]] <-
      factor(data[[legend_variable]], levels = level_i)
  }
  #else if (! ${melted}){
  #	data\$variable <- factor(data\$variable, levels=data_colnames,
  #	ordered=T)
  #}
  
  if (length(facet_order_i) > 1) {
    data[[facet]] <- factor(data[[facet]],
                            levels = facet_order_i)
  }
  
  
  if (length(j_facet_order_i) > 1) {
    data[[j_facet]] <- factor(data[[j_facet]],
                              levels = j_facet_order_i)
  }
  
  if (largest != "Inf") {
    data[[yAxis_variable]][data[[yAxis_variable]] > largest] <- largest
  }
  
  yAxis_variable_en = sym(yAxis_variable)
  legend_variable_en = sym(legend_variable)
  
  p <- ggplot(data, aes(x = !!yAxis_variable_en))
  
  if (type_hist == "count/sum(count)") {
    if (type_p == "hist" ||  type_p == "both") {
      fill_area = TRUE
      if (!is.null(adjust)) {
        p <- p + geom_histogram(
          aes(
            y = after_stat(count / sum(count)) ,
            fill = !!legend_variable_en
          ),
          binwidth = adjust ,
          alpha = alpha,
          position = pos ,
          stat = stat
        )
      } else {
        p <- p + geom_histogram(
          aes(
            y = after_stat(count / sum(count)) ,
            fill = !!legend_variable_en
          ),
          alpha = alpha ,
          position = pos ,
          stat = stat
        )
      }
    }
    if (is.null(y_label)) {
      y_label = "Frequency"
    }
  }
  
  if (type_hist == "count") {
    if (type_p == "hist" ||  type_p == "both") {
      fill_area = TRUE
      if (!is.null(adjust)) {
        p <- p + geom_histogram(
          aes(
            y = after_stat(count) ,
            fill = !!legend_variable_en
          ),
          binwidth = adjust ,
          alpha = alpha,
          position = pos ,
          stat = stat
        )
      } else {
        p <- p + geom_histogram(
          aes(
            y = after_stat(count) ,
            fill = !!legend_variable_en
          ),
          alpha = alpha ,
          position = pos ,
          stat = stat
        )
      }
    }
    if (is.null(y_label)) {
      y_label = "Count"
    }
  }
  
  if (type_hist  == "density") {
    if (type_p == "hist" ||  type_p == "both") {
      fill_area = TRUE
      if (!is.null(adjust)) {
        p <- p + geom_histogram(
          aes(
            y = after_stat(density) ,
            fill = !!legend_variable_en
          ),
          binwidth = adjust ,
          alpha = alpha,
          position = pos ,
          stat = stat
        )
      } else {
        p <- p + geom_histogram(
          aes(
            y = after_stat(density) ,
            fill = !!legend_variable_en
          ),
          alpha = alpha ,
          position = pos ,
          stat = stat
        )
      }
    }
    if (is.null(y_label)) {
      y_label = "Density"
    }
  }
  
  
  if (type_p == "density_line" ||
      (type_p  == "line" &&
       type_hist  == "density") ||
      (type_p  == "both" &&  type_hist  == "density")) {
    if (fill_area) {
      if (!is.null(adjust)) {
        p <- p + geom_density(
          size = line_size,
          alpha = alpha,
          stat = stat,
          aes(
            fill = !!legend_variable_en,
            color = !!legend_variable_en
          ),
          adjust = adjust
        )
      }
      p <-
        p + geom_density(
          size = line_size ,
          alpha = alpha ,
          stat = stat,
          aes(
            fill = !!legend_variable_en ,
            color = !!legend_variable_en
          )
        )
    } else {
      if (!is.null(adjust)) {
        p <- p + stat_density(
          adjust = adjust ,
          size = line_size ,
          stat = stat,
          aes(
            color = !!legend_variable_en ,
            group = !!legend_variable_en ,
            y = after_stat(density)
          ),
          geom = "line",
          position = "identity"
        )
      } else {
        p <- p + stat_density(
          size = line_size,
          stat = stat,
          aes(
            color = !!legend_variable_en,
            group = !!legend_variable_en,
            y = after_stat(density)
          ),
          geom = "line",
          position = "identity"
        )
      }
    }
    
    if (is.null(y_label)) {
      y_label = "Density"
    }
  }
  
  if ((type_p == "line" &&
       type_hist  != "density") ||
      (type_p == "both" &&  type_hist  != "density")) {
    if (type_hist == "count") {
      if (fill_area) {
        if (!is.null(adjust)) {
          p <- p + geom_area(
            size = line_size,
            alpha = alpha,
            aes(
              color = !!legend_variable_en,
              fill = !!legend_variable_en,
              y = after_stat(count)
            ),
            stat = "bin",
            binwidth = adjust
          )
        } else{
          p <- p + geom_area(
            size = line_size,
            alpha = alpha,
            aes(
              color = !!legend_variable_en,
              fill = !!legend_variable_en,
              y = after_stat(count)
            ),
            stat = "bin"
          )
        }
      } else {
        if (!is.null(adjust)) {
          p <- p + geom_freqpoly(
            size = line_size,
            alpha = alpha,
            aes(
              color = !!legend_variable_en,
              y = after_stat(count)
            ),
            binwidth = adjust
          )
        } else {
          p + geom_freqpoly(
            size = line_size,
            alpha = alpha,
            aes(
              color = !!legend_variable_en,
              y = after_stat(count)
            )
          )
          
        }
      }
    } else {
      if (fill_area) {
        if (!is.null(adjust)) {
          p <- p + geom_area(
            size = line_size,
            alpha = alpha,
            aes(
              color = !!legend_variable_en,
              fill = !!legend_variable_en,
              y =  after_stat(count / sum(count))
            ),
            stat = "bin",
            binwidth = adjust
          )
        } else{
          p <- p + geom_area(
            size = line_size,
            alpha = alpha,
            aes(
              color = !!legend_variable_en,
              fill = !!legend_variable_en,
              y =  after_stat(count / sum(count))
            ),
            stat = "bin"
          )
        }
      } else {
        if (!is.null(adjust)) {
          p <- p + geom_freqpoly(
            size = line_size,
            alpha = alpha,
            aes(
              color = !!legend_variable_en,
              y =  after_stat(count / sum(count))
            ),
            binwidth = adjust
          )
        } else {
          p <- p + geom_freqpoly(
            size = line_size,
            alpha = alpha,
            aes(
              color = !!legend_variable_en,
              y =  after_stat(count / sum(count))
            )
          )
          
        }
      }
    }
  }
  
  if (vline) {
    cdf <-
      ddply(data,
            .(legend_variable),
            summarise,
            rating.mean = mean(yAxis_variable))
    p <- p + geom_vline(
      data = cdf,
      aes(
        xintercept = rating.mean,
        colour = !!legend_variable_en
      ),
      linetype = "dashed",
      size = 0.5,
      show.legend = F
    )
    p <- p + geom_text(
      data = cdf,
      aes(
        x = rating.mean * 1.01,
        y = 0,
        label = prettyNum(rating.mean, digits =
                            2),
        color = !!legend_variable_en
      ),
      show.legend = F,
      hjust = 1,
      angle = 90
    )
  }
  
  
  if (!is.null(color_v)) {
    if (fill_area) {
      p <- p + scale_fill_manual(values = color_v)
    } else {
      p <- p + scale_color_manual(values = color_v)
    }
  }
  
  if (j_facet  != "haha") {
    p <- p + facet_grid(facet ~ j_facet , scales = facet_scale)
  } else {
    if (facet  != "haha") {
      p <- p + facet_wrap(~ .data[[facet]] , ncol = facet_ncol ,
                          scales = facet_scale)
    }
  }
  
  if (is.vector(custom_vline_coord)) {
    p <- p + geom_vline(
      xintercept = custom_vline_coord,
      linetype = "dotted",
      show.legend = F,
      size = 0.5
    )
    if (is.vector(custom_vline_anno)) {
      p <- p + annotate(
        "text",
        x = custom_vline_coord,
        y = 0,
        label = custom_vline_anno,
        hjust = 0
      )
    }
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
      y_label = y_label
    )
  }
  if (!ytics) {
    p <- p + theme(axis.text.y = element_blank())
  }
  
  if (length(xtics_v) > 1) {
    if (length(xtics_value) <= 1) {
      xtics_value <- xtics_v
    }
    p <-
      p + scale_x_continuous(breaks = xtics_v, labels = xtics_value)
  }
  
  p
}
