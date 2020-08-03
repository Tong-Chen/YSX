

# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#   Generate DOC:              'Ctrl + Shift + Alt + r'
#   Generate DOC (Mac):              'Ctrl + Shift + Option + r'

#' Check and install given packages
#'
#' @param package A list containing names and install-names of each package.
#' (instll-names is only required for packages from github.)
#' Like list(package1=c("ggplot2")) or
#' list(packages1=c("ggplot2"), package2=c("YSX", "git_user/YSX"))
#'
#' @return NULL
#' @export
#'
#' @examples
#'
#' checkAndInstallPackages(list(package1=c("ggplot2")))
#'
#' checkAndInstallPackages(list(packages1=c("ggplot2"), package2=c("YSX", "git_user/YSX")))
#'
checkAndInstallPackages <-
  function(packageL, site = "https://mirrors.tuna.tsinghua.edu.cn/CRAN") {
    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager",
                       update = F,
                       site_repository = site)

    all_installed_packages = rownames(installed.packages())
    for (i in packageL) {
      package_name = i[1]
      package_install_name = i[1]
      if (length(i) == 2) {
        package_install_name = i[2]
      }
      if (!package_name %in% all_installed_packages)
        BiocManager::install(package_install_name,
                             update = F,
                             site_repository = site)
    }

    suppressPackageStartupMessages(library(package_name, character.only = TRUE))
  }


#' Get current time in strign format
#'
#' @param delim_left Default `[`.
#' @param delim_right Default `]`.
#'
#' @return A string
#' @export
#'
#' @examples
#'
#' sp_current_time()
#'
sp_current_time <- function(delim_left = '[',
                            delim_right = ']') {
  return(paste0(delim_left, Sys.time(), delim_right))
}

#' Determine the value to add befor log transform.
#'
#' @param data A numerical dataframe or a vector
#' @param ratio Minimum non-zero value would be used as add values. if `ratio` specified,
#' the detected minimum non-zero multiple ratio would be returned.
#'
#' @return A numericalvalue
#' @export
#'
#' @examples
#'
#' sp_determine_log_add(c(1,2,3))
#'
sp_determine_log_add <- function(data, ratio = 1) {
  min_value = min(min(data))
  if (min_value > 0) {
    return(0)
  } else if (min_value == 0) {
    min_value = min(min(data[data != 0]))
    return(min_value * ratio)
  } else{
    stop("Negative value is not allowed for log2 transform!")
  }
}

#' Check Null Object
#'
#' @param x `NULL` object or `'null'` string
#'
#' @return True when x is `NULL` or `"NULL"` (case insensitive for character type)
#' @export
#'
#' @examples
#'
#' sp.is.null('NULL')
#'
sp.is.null <- function(x) {
  if (length(x) > 1) {
    return(FALSE)
  }
  if (is.character(x)) {
    return(toupper(x) == 'NULL')
  } else{
    return(base::is.null(x))
  }
}


#' Transfer color string to vector
#'
#' @param x A string
#' @param pattern delimiter of sub-strings
#'
#' @return A vector
#' @export
#'
#' @examples
#'
#' sp_string2vector('red, blue,white')
#'
sp_string2vector <- function(x, pattern = ",") {
  if(sp.is.null(x)){
    return(x)
  }
  if (requireNamespace("stringr", quietly = TRUE)) {
    return(stringr::str_trim(stringr::str_split(x, pattern, simplify = T)))
  } else {
    return(trimws(unlist(strsplit(x, split = pattern))))
  }
}

#' Read in data
#'
#' @inheritParams  utils::read.table
#' @param renameDuplicateRowNames If TRUE, the function will transfer first column
#' as row names (with duplicates numbered)
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' # Not run
#' sp_readTable("a.txt")
#'
sp_readTable <-
  function(file,
           sep = "\t",
           row.names = NULL,
           header = T,
           quote = "",
           comment = "",
           check.names = F,
           renameDuplicateRowNames = F) {
    if (renameDuplicateRowNames) {
      data <- read.table(
        file,
        sep = sep,
        row.names = NULL,
        header = header,
        quote = quote,
        comment = comment,
        check.names = check.names
      )
      rownames_data <- make.unique(as.vector(data[, 1]))
      data <- data[,-1, drop = F]
      rownames(data) <- rownames_data
    } else {
      data <-
        read.table(
          file,
          sep = sep,
          row.names = row.names,
          header = header,
          quote = quote,
          comment = comment,
          check.names = check.names
        )
    }
    invisible(data)
  }

#' Write dataframe to file with names of first column filled.
#'
#' @param df A dataframe
#' @param file Filename
#' @param keep_rownames Default TRUE meaning output rownames as the first column
#' with column name is \code{ID}. If FALSE, ignore rownames.
#' @inheritParams utils::write.table
#'
#' @return NA
#' @export
#'
#' @examples
#'
#' # Not run
#' sp_writeTable(df, "a.txt")
#'
sp_writeTable <- function(df,
                          file = '',
                          keep_rownames = T,
                          col.names=T) {
  if (keep_rownames) {
    write.table(
      data.frame(ID = rownames(df), df),
      file = file,
      sep = "\t",
      quote = F,
      row.names = F,
      col.names = col.names
    )
  } else {
    write.table(
      df,
      file = file,
      sep = "\t",
      quote = F,
      row.names = F,
      col.names = col.names
    )
  }
}

#' Generate gene expression table or otu abundance table with given samle information for test.
#'
#' @param type Generate gene expression or OTU abundance. Only affect rownames.
#' @param mean Mean value of abundance given to \code{\link{rnorm}}.
#' @param sd Standard deviations given to \code{\link{rnorm}}.
#' @param nGene Number of genes or OTUs.
#' @param nGrp Number of sample groups.
#' @param nSample Number of sample replications for each group.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#'
#' df = generateAbundanceDF()
#'
generateAbundanceDF <-
  function(type = "Gene",
           mean = 20,
           nGene = 15,
           nGrp = 2,
           nSample = 3) {
    df <-
      as.data.frame(matrix(rnorm(nGene * nGrp * nSample, mean = mean), nrow =
                             nGene))
    colnames(df) <-
      paste("Samp", paste(rep(LETTERS[1:nGrp], each = nSample), rep(1:nSample, nGrp), sep =
                            "_"), sep = "_")
    rownames(df) <- paste(type, letters[1:nGene], sep = "_")
    return(df)
  }



#' Get ordered column correlation matrix from input dataframe. Normally used
#' to do sample corealtion of gene expression or OTU abundance matrix.
#'
#' @param mat A dataframe.
#' @param method Type of correlation coefficient given to \code{\link{cor}}.
#' Default "pearson".
#' @param digits Number of decimial digits (given to \code{\link{round}}) to keep (default 4).
#' @param cor_file Save ordered correlation matrix to given file name.
#'
#' @return A list containing ordered column correlation matrix and hcluster result.
#' @export
#'
#' @examples
#'
#' df = generateAbundanceDF()
#' Matrix2colCorrelation(df)
#'
Matrix2colCorrelation <-
  function(mat,
           method = "pearson",
           digits = 4,
           cor_file = NULL) {
    pearson_cor <-
      round(as.matrix(cor(mat, method = method)), digits = digits)
    hc <- amap::hcluster(t(mat), method = method)
    pearson_cor <- pearson_cor[hc$order, hc$order]
    if (!is.null(file)) {
      pearson_cor_output = data.frame(id = rownames(pearson_cor), pearson_cor)
      write.table(
        pearson_cor_output,
        file = cor_file,
        quote = F,
        sep = "\t",
        row.names = F,
        col.names = T
      )
    }
    return(list(pearson_cor = pearson_cor, hc = hc))
  }



#' Get lower triangle of the correlation matrix (from web)
#'
#' @param cormat A data frame
#'
#' @return A data frame
#' @export
#'
#' @examples
#'
#' df = generateAbundanceDF()
#' df_cor = Matrix2colCorrelation(df)
#' get_lower_tri(df_cor)
#'
get_lower_tri <- function(cormat) {
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}


#' Get upper triangle of the correlation matrix (from web)
#'
#' @param cormat A data frame
#'
#' @return A data fram
#' @export
#'
#'
#' @examples
#'
#' df = generateAbundanceDF()
#' df_cor = Matrix2colCorrelation(df)
#' get_upper_tri(df_cor)
#'
get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}



# options(scipen=999)

#' Check if given string or vector is all numeric
#'
#' @param x A string or a vector
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#'
#' numCheck(3)
#'
#' numCheck("-1/3")
#'
#' numCheck(c("1","0.2","1/3","-1"))
#'
numCheck <- function(x) {
  # Function get from https://stackoverflow.com/questions/10674992/convert-a-character-vector-of-mixed-numbers-fractions-and-integers-to-numeric?rq=1
  # With little modifications
  is.numeric2 <- is.numeric(x)
  x <- sapply(x, as.character)
  is.integer  <- grepl("^-?\\d+$", x)
  is.fraction <- grepl("^-?\\d+\\/\\d+$", x)
  is.float <- grepl("^-?\\d+\\.\\d+$", x)
  is.percent <- grepl("[0-9.]+%$", x)
  is.mixed    <- grepl("^-?\\d+ \\d+\\/\\d+$", x)
  return(all(
    is.numeric2 |
      is.integer | is.fraction | is.float | is.mixed | is.percent
  ))
}

#' Transfer numeric string to numeric.
#'
#' @param x A string or a vector
#'
#' @return A number or a numeric vector
#' @export
#'
#' @examples
#'
#' mixedToFloat(3)
#'
#' mixedToFloat("-1/3")
#'
#' mixedToFloat(c("1","0.2","1/3","-1"))
#'
mixedToFloat <- function(x) {
  x <- sapply(x, as.character)
  is.integer  <- grepl("^-?\\d+$", x)
  is.fraction <- grepl("^-?\\d+\\/\\d+$", x)
  is.float <- grepl("^-?\\d+\\.\\d+$", x)
  is.mixed    <- grepl("^-?\\d+ \\d+\\/\\d+$", x)
  is.percent <- grepl("[0-9.]+%$", x)
  stopifnot(all(is.integer |
                  is.fraction | is.float | is.mixed | is.percent))

  numbers <- strsplit(x, "[ /%]")

  ifelse(is.integer,
         as.numeric(sapply(numbers, `[`, 1)),
         ifelse(
           is.percent,
           as.numeric(sapply(numbers, `[`, 1)) / 100,
           ifelse(
             is.float,
             as.numeric(sapply(numbers, `[`, 1)),
             ifelse(
               is.fraction,
               as.numeric(sapply(numbers, `[`, 1)) /
                 as.numeric(sapply(numbers, `[`, 2)),
               as.numeric(sapply(numbers, `[`, 1)) +
                 as.numeric(sapply(numbers, `[`, 2)) /
                 as.numeric(sapply(numbers, `[`, 3))
             )
           )
         ))

}

#mixedToFloat(c('1 1/2', '2 3/4', '2/3', '11 1/4', '1'))


#' Generate color code
#'
#' @param color Colors like c('red', 'blue', '#6181BD') or
#' a RColorBrewer color set like  "BrBG"     "PiYG"     "PRGn"     "PuOr"
#' "RdBu"     "RdGy"     "RdYlBu"   "RdYlGn"  "Spectral" "Accent"
#' "Dark2"    "Paired"   "Pastel1"  "Pastel2"  "Set1"
#' "Set2"    "Set3"     "Blues"    "BuGn"     "BuPu"
#' "GnBu"     "Greens"   "Greys"    "Oranges" "OrRd"     "PuBu"
#' "PuBuGn"   "PuRd"     "Purples"  "RdPu"     "Reds"
#' "YlGn"    "YlGnBu"   "YlOrBr"   "YlOrRd"
#' (check <http://www.sthda.com/english/wiki/colors-in-r> for more).
#'
#' @param number Number of colors to return.
#'
#' @param alpha Generate an alpha transparency values for return colors. 0 means fully transparent and 1 means opaque. Default 1.
#'
#' @return A color vector
#' @export
#'
#' @examples
#'
#' generate_color_list('red', 5)
#' generate_color_list(c('green', 'red'), 5)
#' generate_color_list(c('green', 'red'), 5, alpha=0.5)
#' generate_color_list("Set3", 5)
#'

generate_color_list <- function(color, number, alpha = 1) {
  color_len = length(color)
  if (color_len == 1) {
    brewer = rownames(RColorBrewer::brewer.pal.info)
    if (color %in% brewer) {
      if (number <= RColorBrewer::brewer.pal.info[color, ]$maxcolors) {
        colorL <- RColorBrewer::brewer.pal(number, color)
      } else {
        colorL <-
          colorRampPalette(RColorBrewer::brewer.pal(3, color))(number)
      }
    } else{
      colorL <- rep(color, number)
    }
  } else if (color_len == number) {
    colorL = color
  } else{
    colorL = colorRampPalette(color)(number)
  }
  return(rgb(
    t(col2rgb(colorL)),
    alpha = alpha * 255,
    maxColorValue = 255
  ))
}

#' Set factor order of given variable. If `variable_order` is supplied, only
#' factors in `variable_order` will be kept and re-factored. Other variables
#' would be depleted.
#'
#' @param data A data matrix
#' @param variable One column name of data matrix
#' @param variable_order Expected order of `data[[variable]]`.
#'
#' @return A data frame
#' @export
#'
#' @examples
#'
#' data <- data.frame(A=letters[1:4], B=letters[1:4])
#' data
#' data = sp_set_factor_order(data,'A')
#' data$A
#' data = sp_set_factor_order(data,'B',c('c','d','b','a'))
#' data$B
#' data = sp_set_factor_order(data,'B',c('c','d','a'))
#' data$B
#'
sp_set_factor_order <-
  function(data, variable, variable_order = NULL) {
    if (!sp.is.null(variable_order)) {
      data = data[data[[variable]] %in% variable_order, , drop = F]
      data[[variable]] <-
        factor(data[[variable]], levels = variable_order, ordered = T)
    } else {
      data[[variable]] <- factor(data[[variable]],
                                 levels = unique(data[[variable]]), ordered = T)
    }
    invisible(data)
  }

#' Add manual color assignment for both categorical and numerical variable
#'
#' @param p A ggplot2 object
#' @param data Data matrix used for the ggplot2 object `p`
#' @param color_variable Name of columns for color assignment
#' @param manual_color_vector Manually set colors for each geom.
#' Default NULL, meaning using ggplot2 default.
#' Colors like c('red', 'blue', '#6181BD') (number of colors not matter) or
#' a RColorBrewer color set like  "BrBG"     "PiYG"     "PRGn"     "PuOr"
#' "RdBu"     "RdGy"     "RdYlBu"   "RdYlGn"  "Spectral" "Accent"
#' "Dark2"    "Paired"   "Pastel1"  "Pastel2"  "Set1"
#' "Set2"    "Set3"     "Blues"    "BuGn"     "BuPu"
#' "GnBu"     "Greens"   "Greys"    "Oranges" "OrRd"     "PuBu"
#' "PuBuGn"   "PuRd"     "Purples"  "RdPu"     "Reds"
#' "YlGn"    "YlGnBu"   "YlOrBr"   "YlOrRd"
#' (check <http://www.sthda.com/english/wiki/colors-in-r> for more).
#' @param alpha Color transparency (0-1). 0: opaque; 1: transparent.
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#'
#' ## Not run:
#' p <- sp_manual_color_ggplot2(p, data, color_variable, manual_color_vector)
#'
#' ## End(Not run)
#'
sp_manual_color_ggplot2 <-
  function (p,
            data,
            color_variable,
            manual_color_vector = NULL,
            alpha = 1) {
    if (!sp.is.null(manual_color_vector)) {
      if (is.numeric(data[[color_variable]])) {
        color_v <- generate_color_list(manual_color_vector, 10, alpha = alpha)
        p <-
          p + scale_color_gradientn(colors = color_v)
      } else {
        color_v <-
          generate_color_list(manual_color_vector, length(unique(data[[color_variable]])),
                              alpha = alpha)
        p <- p + scale_color_manual(values = color_v)
      }
    }
    p
  }

#' Add manual fill-color assignment for both categorical and numerical variable
#'
#' @param p A ggplot2 object
#' @param data Data matrix used for the ggplot2 object `p`
#' @param color_variable Name of columns for color assignment
#' @param manual_color_vector Manually set colors for each geom.
#' Default NULL, meaning using ggplot2 default.
#' Colors like c('red', 'blue', '#6181BD') (number of colors not matter) or
#' a RColorBrewer color set like  "BrBG"     "PiYG"     "PRGn"     "PuOr"
#' "RdBu"     "RdGy"     "RdYlBu"   "RdYlGn"  "Spectral" "Accent"
#' "Dark2"    "Paired"   "Pastel1"  "Pastel2"  "Set1"
#' "Set2"    "Set3"     "Blues"    "BuGn"     "BuPu"
#' "GnBu"     "Greens"   "Greys"    "Oranges" "OrRd"     "PuBu"
#' "PuBuGn"   "PuRd"     "Purples"  "RdPu"     "Reds"
#' "YlGn"    "YlGnBu"   "YlOrBr"   "YlOrRd"
#' (check <http://www.sthda.com/english/wiki/colors-in-r> for more).
#' @param alpha Transparency
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#'
#' ## Not run:
#' p <- sp_manual_fill_ggplot2(p, data, color_variable, manual_color_vector)
#'
#' ## End(Not run)
#'
sp_manual_fill_ggplot2 <-
  function (p,
            data,
            color_variable,
            manual_color_vector = NULL,
            alpha = 1) {
    if (!sp.is.null(manual_color_vector)) {
      if (is.numeric(data[[color_variable]])) {
        color_v <- generate_color_list(manual_color_vector, 10, alpha = alpha)
        p <-
          p + scale_fill_gradientn(colors = color_v)
      } else {
        color_v <-
          generate_color_list(manual_color_vector, length(unique(data[[color_variable]])),
                              alpha = alpha)
        p <- p + scale_fill_manual(values = color_v)
      }
    }
    p
  }

#' Add hline or vline for ggplot2 object
#'
#' @param p A ggplot2 pbject
#' @param xintercept A vector of coordinates for vertical lines.
#' @param custom_vline_anno Annotation text for each vertical line.
#' @param yintercept A vector of coordinates for horizontal lines.
#' @param custom_hline_anno Annotation text for each horizontal line.
#' @inheritParams ggplot2::geom_vline
#' @param ... Extra parameters given to `geom_vline` and `geom_hline`
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#'
#' ## Not run:
#' sp_ggplot_add_vline_hline(p)
#'
#' ## End(Not run)
#'
#'
sp_ggplot_add_vline_hline <- function(p,
                                      xintercept = NULL,
                                      custom_vline_anno = NULL,
                                      custom_vline_anno_y_pos = NULL,
                                      yintercept = NULL,
                                      custom_hline_anno = NULL,
                                      custom_hline_anno_x_pos = NULL,
                                      linetype = "dotted",
                                      size = 0.5,
                                      ...) {
  gb = ggplot_build(p)

  if (!is.null(xintercept)) {
    p <- p + geom_vline(xintercept = xintercept,
                        linetype = linetype,
                        size = size,
                        ...)
    if (!is.null(custom_vline_anno)) {
      if (is.null(custom_vline_anno_y_pos)) {
        custom_vline_anno_y_pos = gb$layout$panel_params[[1]]$y.range[2]
      }
      p <-
        p + annotate(
          "text",
          x = xintercept,
          y = custom_vline_anno_y_pos,
          label = custom_vline_anno,
          hjust = 0
        )
    }
  }


  if (!is.null(yintercept)) {
    p <- p + geom_hline(yintercept = yintercept,
                        linetype = linetype,
                        size = size,
                        ...)
    if (!is.null(custom_hline_anno)) {
      if (is.null(custom_hline_anno_x_pos)) {
        custom_hline_anno_x_pos = 0
      }
      #xmax = gb$layout$panel_params[[1]]$x.range[2]
      p <-
        p + annotate(
          "text",
          y = yintercept,
          x = custom_hline_anno_x_pos,
          label = custom_hline_anno,
          vjust = 0,
          hjust = 0
        )
    }
  }
  return(p)
}

#' Change common layout of ggplot2 object
#'
#' @param p A ggplot2 object
#' @param xtics_angle Rotation angle for a-axis
#' @param legend.position Position of legend, accept top, bottom, left, right, none or c(0.8,0.8).
#' @param extra_ggplot2_cmd Extra ggplot2 commands (currently unsupported)
#' @param filename Output picture to given file.
#' @param title Title of picture.
#' @param x_label Xlab label.
#' @param y_label Ylab label.
#' @param coordinate_flip Flip cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal. This is primarily useful for converting geoms and statistics which display y conditional on x, to x conditional on y.
#' @param width Picture width (units: cm)
#' @param height Picture height (units: cm)
#' @param ... Extra parameters to \code{\link[ggplot2]{ggsave}}.
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#'
#' ## Not run:
#' sp_ggplot_layout(p)
#'
#' ## End(Not run)
#'
sp_ggplot_layout <-
  function(p,
           xtics_angle = 0,
           legend.position = "right",
           extra_ggplot2_cmd = NULL,
           filename = NULL,
           x_label = NULL,
           y_label = NULL,
           title = NULL,
           coordinate_flip = FALSE,
           ...) {
    p <-
      p + theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        axis.line.x = element_line(
          size = 0.4,
          colour = "black",
          linetype = 'solid'
        ),
        axis.line.y = element_line(
          size = 0.4,
          colour = "black",
          linetype = 'solid'
        ),
        axis.ticks = element_line(size = 0.4,
                                  colour = "black")
      )

    if (xtics_angle != 0) {
      if (xtics_angle == 90) {
        p <- p + theme(axis.text.x =
                         element_text(
                           angle = xtics_angle,
                           hjust = 1,
                           vjust = 0.5
                         ))
      } else if (xtics_angle == 45) {
        p <- p + theme(axis.text.x =
                         element_text(
                           angle = xtics_angle,
                           hjust = 0.5,
                           vjust = 0.5
                         ))
      } else {
        p <- p + theme(axis.text.x =
                         element_text(
                           angle = xtics_angle,
                           hjust = 0.5,
                           vjust = 0.5
                         ))
      }
    }

    if (!sp.is.null(x_label)) {
      p <- p + xlab(x_label)
    }

    if (!sp.is.null(y_label)) {
      p <- p + ylab(y_label)
    }

    if (!sp.is.null(title)) {
      p <- p + labs(title = title)
    }

    p <- p + theme(legend.position = legend.position)

    #add additional ggplot2 supported commands

    if (!sp.is.null(extra_ggplot2_cmd)) {
      p <- p + eval(parse(text = extra_ggplot2_cmd))
    }

    if (coordinate_flip) {
      p <- p + coord_flip()
    }
    # output pictures
    if (sp.is.null(filename)) {
      return(p)
    } else{
      ggsave(p,
             filename = filename,
             units = c("cm"),
             ...)
    }
  }

#' Get the x, y limits of a ggplot2 plot
#'
#' @param p A ggplot2 object
#'
#' @return A list list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
#' @export
#'
#' @examples
#' ## Not run:
#' sp_get_ggplot_limits(p)
#'
#' ## End(Not run)
sp_get_ggplot_limits <- function(p) {
  # https://stackoverflow.com/questions/7705345/how-can-i-extract-plot-axes-ranges-for-a-ggplot2-object#
  gb = ggplot_build(plot)
  xmin = gb$layout$panel_params[[1]]$x.range[1]
  xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  list(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax
  )
}

#' Return if unique values of two vectors are the same (order does not matter)
#'
#' @param x A vector
#' @param y A vector
#'
#' @return Logial value T or F
#' @export
#'
#' @examples
#'
#' value.identical(c('a','a','b','d'), c('d','d','a','b'))
#'
#' # TRUE
#'
value.identical <-
  function(x, y, treat_fully_contain_as_identical = F) {
    x_unique = sort(unique(as.character(x)))
    y_unique = sort(unique(as.character(y)))
    all_ident = identical(x_unique, y_unique)
    if (all_ident) {
      return(all_ident)
    }
    if (treat_fully_contain_as_identical) {
      xy_intersect = intersect(x_unique, y_unique)
      return(identical(xy_intersect, x_unique) ||
               identical(xy_intersect, y_unique))
    } else {
      return(all_ident)
    }
  }

#' Detect pairs of columns with same unique values (order does not matter) in two dataframes.
#'
#' @param df1 Dataframe1
#' @param df2 Dataframe2
#' @param only_allow_one_match Default FALSE. This parameters is designed to get only one pair of matched columns
#' between two dataframes to supply as parameters for \link{merge} function (when TRUE).
#'
#' @return A dataframe containing names of matched columns. Or a vetor containing names of matched columns
#' when `only_allow_one_match` is `TRUE` and there do have one match.
#' @inheritParams value.identical
#' @export
#'
#' @examples
#'
#' vec1 <- data.frame(col1=c('a','a','b','d'), a=c(1,2,3,4))
#' vec2 <- data.frame(col2=c('d','d','a','b'), b=c(1,2,4,5),a=c(1,2,3,4))
#' get_matched_columns_based_on_value(vec1, vec2)
#'
#' #     match_1 match_2
#' # DF1    col1       a
#' # DF2    col2       a
#'
#' vec2 <- data.frame(col2=c('d','d','a','b'))
#' get_matched_columns_based_on_value(vec1, vec2)
#'
#' #     match_1
#' # DF1    col1
#' # DF2    col2
#'
#' get_matched_columns_based_on_value(vec1, vec2, only_allow_one_match = T)
#'
#' # "col1" "col2"
#'
#'
get_matched_columns_based_on_value <-
  function(df1,
           df2,
           only_allow_one_match = F,
           treat_fully_contain_as_identical = T) {
    if (length(df1) == 1) {
      df1['__extra_s_p_column__'] = '__extra_s_p_column__'
    }
    if (length(df2) == 1) {
      df2['__extra_s_p_column__'] = '__extra_s_p_column2__'
    }
    df1_rownames = rownames(df1)
    df2_rownames = rownames(df2)

    if(value.identical(df1_rownames, df2_rownames, treat_fully_contain_as_identical)){
      return(c(0,0))
    }

    matches <-
      sapply(df2, function(x)
        sapply(
          df1,
          value.identical,
          x,
          treat_fully_contain_as_identical = treat_fully_contain_as_identical,
          simplify = T
        ),
        simplify = T)
    # print(matches)
    matches_index <-
      as.data.frame(which(matches == T, arr.ind = TRUE))
    # print(matches_index)
    df1_colnames <- colnames(df1)
    df2_colnames <- colnames(df2)
    matches_names <- apply(matches_index, 1,
                           function(x)
                             c(df1_colnames[x[1]], df2_colnames[x[2]]))
    matches_names <- as.data.frame(matches_names)
    # print(matches_names)
    if (nrow(matches_names) == 0) {
      stop(
        "No columns matched each other between given two data.frames. The program does not know which to return. Please check."
      )
    }
    matches_names_count <- length(matches_names)
    # print(matches_names_count)
    colnames(matches_names) <-
      paste0("match_", 1:matches_names_count)
    rownames(matches_names) <- c("DF1", "DF2")
    # print(matches_names)
    if (only_allow_one_match)
      if (matches_names_count > 1) {
        stop(
          "Multiple pairs of columns matched each other between given two data.frames. The program does not know which to return. Please check."
        )
      } else {
        return(matches_names[, 1])
      }

    matches_names
  }
