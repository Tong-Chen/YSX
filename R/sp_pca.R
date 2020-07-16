

#' Generating pca plot
#'
#' @param data Data file. With header line, the first column is the rowname, tab seperated. Each row represents variable (normally genes), each column represents samples.
#' @param grp_file Sample group file with first column as sample names, other columns as sample attributes. Below, color, size, shape variable should be existed in this file.
#' @param title Title of picture. Default empty title
#' @param scale Scale data for PCA analysis. In defaut, prcomp will centralized data by minus mean value and
#' normalize data by column standard deviation dividing.
#' Often, we would normalize data. Only when we care about the real number changes other than the trends,
#' we do not need scale. When this happens, we expect the changin ranges of data is small for example log-transformed data.
#' @param minimum_mad Minimum mad to keep. Larger mad, larger variance. Default 0.5.
#' @param top_n Use top n most changed variables for PCA computation. Default 5000. Giving 0 to use all variables.
#' @param color The variable for point color. Optional, such as color.
#' @param color_v Manually specified colors. Default system default. Accept string in format like <'"green", "red"'> (both types of quotes needed).
#' @param color_order The order for color variable. Default alphabetical order, accept a string like "'K562','hESC','GM12878','HUVEC','NHEK','IMR90','HMEC'".
#' @param log Log-transform data before principle component analysis.
#' @param log_add Add a value before log-transform to avoid log(0). Default 1.
#' @param size The variable for point size. Optional, such as a number or a variable like count, normally should be number column.
#' @param shape The variable for point shape. Optional, such as shape.
#' @param shape_order The order for shape variable. Default alphabetical order, accept a string like "'K562','hESC','GM12878','HUVEC','NHEK','IMR90','HMEC'".
#' @param dimensions Dimensions to plot. Default 2.	Accept 3 (both color and shape variable needed and should be same variable).
#' @param alpha Transparency value for points. Optional, such as a number or a variable indicating one data column, normally should be number column.
#' @param label Label points (using geom_text_repel). Default no-label (FALSE), accept TRUE.
#' @param label_font_size Label font size. Default system default. Accept numbers less than 5 to shrink fonts.
#' @param facet The formula for facets. Default no facets,
#' facet_grid(level ~ .) means divide by levels of 'level' vertcally.
#' facet_grid(. ~ level) means divide by levels of 'level' horizontally.
#' facet_grid(lev1 ~ lev2) means divide by lev1 vertically and lev2 horizontally.
#' facet_wrap(~level, ncol=2) means wrap horizontally with 2 columns.
#' Example: facet_wrap(~Size,ncol=6,scale='free')
#' @param ...
#'
#' @return pdf and xls files.
#' @export
#'
#' @examples
#' pca_test_data <- matrix(runif(3000,0,100000),ncol=6)
#' colnames(pca_test_data) <- c(paste("wt",1:3,sep = ""),paste("ko",1:3,sep = ""))
#' rownames(pca_test_data) <- c(ID = paste0("ENSG",c(1:500)))
#' pca_data <- as.data.frame(pca_test_data)
#' sp_pca(data = pca_data, grp_file = NULL)
#'
#'
#' ## Not run:
#' data = "pca.data"
#' group_data = "pca_group.data"
#' sp_pca(data = data, grp_file = group_data, color="Conditions", size = "Diameters", shape = "Batch", label = TRUE)
#' ## End(Not run)
#'
#'
sp_pca <- function(data,
                   grp_file = NULL,
                   title = '',
                   scale = TRUE,
                   color = 'c_t_c_t0304',
                   color_v = c(),
                   log = 'nolog',
                   facet = NULL,
                   size = NULL,
                   shape = 'c_t_c_t0304',
                   color_order = c(),
                   top_n = 5000,
                   shape_order = c(),
                   dimensions = 2,
                   alpha = 1,
                   mid = ".pca",
                   label = FALSE,
                   log_add = 1,
                   label_font_size = NULL,
                   minimum_mad = 0.5,
                   debug = FALSE,
                   saveplot = NULL,
                   filename = "data",
                   ...) {
  if (scale) {
    mid = ".pca.scale"
  }
  if (class(data) == "character") {
    filename <- data
    data <- sp_readTable(data, row.names = NULL)
    rownames_data <- make.unique(as.vector(data[, 1]))
    data <- data[, -1, drop = F]
    rownames(data) <- rownames_data
  }

  data <- data[rowSums(abs(data)) != 0, ]

  data$mad <- apply(data, 1, mad)

  data <- data[data$mad > minimum_mad , ]

  data <-
    data[order(data$mad, decreasing = T), 1:(dim(data)[2] - 1)]

  dim_data <- dim(data)

  data_row_num <- dim_data[1]

  if (top_n != 0 & top_n < data_row_num) {
    data <- data[1:top_n, ]
  }

  data <- as.data.frame(t(data))

  sampleL = rownames(data)


  if (is.null(grp_file)) {
    data_t_label <- data
    data_t_label$group = sampleL
    data_t_label$Row.names = sampleL
  } else {
    if (class(grp_file) == "character") {
      grp_data <- sp_readTable(grp_file, row.names = NULL)
      rownames(grp_data) <- grp_data[, 1]
      data_t_label <- merge(data, grp_data, by = 0, all.x = T)
      rownames(data_t_label) <- data_t_label$Row.names
      data_t_label <-
        data_t_label[match(sampleL, data_t_label$Row.names), ]
    }
  }

  if (shape != "c_t_c_t0304") {
    if (length(shape_order) > 1) {
      data_t_label[[shape]] <-
        factor(data_t_label[[shape]], levels = shape_order, ordered = T)
    } else {
      data_t_label[[shape]] <- factor(data_t_label[[shape]])
    }
  }

  if (length(color_order) > 1) {
    data_t_label[[color]] <-
      factor(data_t_label[[color]], levels = color_order, ordered = T)
  }


  if (log != "nolog") {
    data <- log(data + log_add)
  }


  if (shape  != "c_t_c_t0304") {
    shape_level <- length(unique(data_t_label[[shape]]))
    shapes = (1:shape_level) %% 30
  }

  pca <- prcomp(data, scale = scale)

  rotation = pca$rotation

  write.table(
    data.frame(ID = rownames(rotation), rotation),
    file = paste0(filename, mid, ".weights.xls"),
    sep = "\t",
    quote = F,
    row.names = F,
    col.names = T
  )


  x = pca$x

  write.table(
    data.frame(ID = rownames(x), x),
    file = paste0(filename, mid, ".pcs.xls"),
    sep = "\t",
    quote = F,
    row.names = F,
    col.names = T
  )

  percentVar <- pca$sdev ^ 2 / sum(pca$sdev ^ 2)

  percentVar2 <- as.data.frame(percentVar)
  rownames(percentVar2) <- colnames(x)

  write.table(
    percentVar2,
    file = paste0(filename, mid, ".pc_variance.xls"),
    sep = "\t",
    quote = F,
    row.names = T
  )


  if (dimensions == 2) {
    p <-
      autoplot(pca, data = data_t_label, alpha = alpha) + ggtitle(title) + coord_fixed()

    if (!is.null(size)) {
      size_en = sym(size)
      p <- p + aes(size = !!size_en)
    }
    if (color != "c_t_c_t0304") {
      color_en = sym(color)
      p <- p + aes(colour = !!color_en)
      if (length(color_v) >= 2) {
        if (is.numeric(data_t_label[[color]])) {
          p <-
            p + scale_colour_gradient(low = color_v[1],
                                      high = color_v[2],
                                      name = color)
        } else {
          p <- p + scale_color_manual(values = color_v)
        }
      }
    }

    if (shape  != "c_t_c_t0304") {
      shape_en = sym(shape)
      p <- p + aes(shape = !!shape_en)
      if (shape_level > 6) {
        p <- p + scale_shape_manual(values = shapes)
      }
    }

    if (label) {
      if (!is.null(label_font_size)) {
        p <-
          p + geom_text_repel(aes(label = Row.names),
                              show.legend = F,
                              size = label_font_size)
      } else {
        p <- p + geom_text_repel(aes(label = Row.names), show.legend = F)
      }
    }

    x_label = paste0("PC1 (", round(percentVar[1] * 100), "% variance)")
    y_label = paste0("PC2 (", round(percentVar[2] * 100), "% variance)")

    p <- p + xlab(x_label) + ylab(y_label)

    p <- p + facet

    if (is.null(saveplot)) {
      p
    } else {
      ggsave(p,
             filename = saveplot,
             units = c("cm"))
    }

  } else {
    library(scatterplot3d)
    if (color != "c_t_c_t0304") {
      # 根据分组数目确定颜色变量
      group = data_t_label[[color]]
      colorA <- rainbow(length(unique(group)))

      # 根据每个样品的分组信息获取对应的颜色变量
      colors <- colorA[as.factor(group)]

      # 根据样品分组信息获得legend的颜色
      colorl <- colorA[as.factor(unique(group))]
    }

    if (shape != "c_t_c_t0304") {
      # 获得PCH symbol列表
      group <- data_t_label[[shape]]
      pch_l <- as.numeric(as.factor(unique(group)))
      # 产生每个样品的pch symbol
      pch <- pch_l[as.factor(group)]
    }

    pc <- as.data.frame(pca$x)

    saveplot = paste0(filename, mid, ".pdf")

    if (!is.null(saveplot)) {
      base_plot_save(saveplot, ...)
    }

    # pdf(paste0(filename,mid,"sds.pdf"))
    scatterplot3d(
      x = pc$PC1,
      y = pc$PC2,
      z = pc$PC3,
      pch = pch,
      color = colors,
      xlab = paste0("PC1 (", round(percentVar[1] * 100), "% variance)"),
      ylab = paste0("PC2 (", round(percentVar[2] * 100), "% variance)"),
      zlab = paste0("PC3 (", round(percentVar[3] * 100), "% variance)")
    )

    legend(
      -3,
      8,
      legend = levels(as.factor(color)),
      col = colorl,
      pch = pch_l,
      xpd = T,
      horiz = F,
      ncol = 6
    )

    if (!is.null(saveplot)) {
      dev.off()
    }
  }

  if (debug) {
    argg <- c(as.list(environment()), list(...))
    print(argg)
  }
}
