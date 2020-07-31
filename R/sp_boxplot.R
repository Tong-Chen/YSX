#' Generating box plot
#'
#' `metadata`
#'
#' @param data Data file (with header line, the first row is the colname,
#' tab seperated. Multiple formats are allowed and described above)
#' @param melted When TRUE, meaning a long format matrix is supplied to `data`.
#' function will skip preprocess. Default FALSE.
#' @param metadata Giving a metadata file with format specified in example
#' to tell the group information for each sample.
#' @param xvariable The column represents the x-axis values. For unmelted data, the program
#' will use first column as x-variable. If one want to use first row of unmelted data
#' as x-variable, please specify `variable` here (which is an inner name).
#' Or if one want to use other columns in `metadata`.
#' @param yvariable The column represents the digital values.
#' For unmelted data, the program
#' will use `value` as y-variable (which is an inner name).
#' This parameter can only be set when `melted` is TRUE.
#' @param legend_variable The column represents the legend information.
#' Default `xvariable` if not specified.
#' @param xtics_angle Rotation angle for x-axis value. Default 0.
#' @param legend_variable_order Levels for legend variable.
#' Default data order, accept a vector like c('TP16','TP22','TP23') for `legend_variable` column.
#' @param legend_variable_cut Self-define intervals for legend variable when
#' values in `legend_variable` column is continuous numbers.
#' @param xvariable_order xvariable_order Levels for x-axis variable. Default data order, accept input like c('g','a','j','x','s','c','o','u') for Set column.
#' @param xvariable_cut sxvariable_cut Self-define intervals for x-axis variable.
#' @param scaleY Scale y axis. Default FALSE.  Accept TRUE.
#' Also if the supplied number after `y_add` is not 0, this parameter will be set to TRUE
#' @param y_add A number to add if scale is used.
#' Default 0 meaning the minimum non-zero value wolud be used.
#' @param yaxis_scale_mode Give the following `scale_y_log10()`,
#' `coord_trans(y="log10")`, or other legal command for ggplot2 or
#' simply `log2` to set the scale way.
#' @param notch Using notch (sand clock shape) or not. Default FALSE.
#' @param outlier Exclude outliers. Exclude outliers or not, default `FALSE`` means keeping outliers.
#' @param out_scale The scales for one want to set to exclude outliers.
#' Default 1.05. No recommend to change unless you know what you are doing.
#' @param violin Do violin plot plus inner boxplot.
#' @param violin_nb Do violin plot without inner boxplot.
#' @param scale_violin The value given to scale for violin plot.
#' if "area", all violins have the same area (before trimming the tails).
#' If "count", areas are scaled proportionally to the number of observations.
#' If "width", all violins have the same maximum width. 'equal' is also accepted.
#' @param ID_var Other columns one want to treat as ID variable columns
#' except the one given to `xvariable`.
#' @param jitter Do jitter plot instead of boxplot.
#' @param jitter_bp Do jitter plot overlay with violinplot or boxplot or both.
#' @param coordinate_flip Rotate the plot from vertical to horizontal.
#' Usefull for plots with many values or very long labels at X-axis
#' @param facet Wrap plots by given column. This is used to put multiple plot
#' in one picture. Used when `melted` is FALSE, normally a string set should be suitable for this parameter.
#' @param facet_level The levels of wrapping to set the order of each group.
#' @param x_label Xlab label.Default NULL.
#' @param y_label Ylab label.Default NULL.
#' @param nrow 	The number of rows one want when `melted` is used. Default NULL.
#' @param ncol The number of columns one want when `melted` is used. Default NULL.
#' @param scales Paramter for scales for facet. Default `fixed` meaning each inner graph
#' use same scale (x,y range), `free` (variable x, y ranges for each sub-plot),
#' `free_x` (variable x ranges for each sub-plot), `free_y` (variable y ranges for each sub-plot).
#' @param gene Giving one gene ID to do boxplot specifically for this gene.
#' Default FALSE, accept a string.
#' @inheritParams sp_ggplot_layout
#' @inheritParams sp_manual_color_ggplot2
#' @param ... Parametes given to `sp_ggplot_layout`
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#'
#' box_test_data <- data.frame(ID = letters[1:4],
#' Gene = letters[c(8,8,9,9,10,10,11,11)], Expr = runif(16))
#' sp_boxplot(data = box_test_data, xvariable = "ID", value = "Expr", variable ="Gene")
#'
#' ## Not run:
#' box_data = "box.data"
#'
#' sp_boxplot(data = box_data, xvariable = "Gene", value = "Expr", variable="Group")
#' ## End(Not run)
#'
sp_boxplot <- function(data,
                       melted = FALSE ,
                       xvariable = NULL,
                       yvariable = NULL,
                       legend_variable = NULL,
                       statistics= FALSE,
                       xtics_angle = 0,
                       legend_variable_order = NULL,
                       legend_variable_cut = NULL,
                       xvariable_order = NULL,
                       xvariable_cut = NULL,
                       group_variable_for_line = NULL,
                       y_add = 0,
                       yaxis_scale_mode = '',
                       notch = FALSE,
                       par = NULL,
                       outlier = FALSE,
                       out_scale = 1.05,
                       legend.position = 'right',
                       manual_color_vector = NULL,
                       violin = FALSE,
                       violin_nb = FALSE,
                       scale_violin = 'width',
                       ID_var = c(),
                       jitter = FALSE ,
                       jitter_bp =  FALSE ,
                       colormodel = 'srgb',
                       coordinate_flip = FALSE,
                       facet = NULL,
                       facet_level = NULL,
                       x_label = NULL,
                       y_label = NULL,
                       title = NULL,
                       nrow = NULL,
                       ncol = NULL,
                       scales = 'fixed',
                       gene = NULL,
                       metadata = NULL,
                       debug = F,
                       width=0,
                       height=0,
					   extra_ggplot2_cmd=NULL,
                       ...) {
  if (debug) {
    argg <- c(as.list(environment()), list(...))
    print(argg)
  }

  if (!melted) {
    if (sp.is.null(yvariable) ){
      yvariable = "value"
    }
    if(sp.is.null(legend_variable)) {
      legend_variable = "variable"
    }
  }

  if (class(data) == "character") {
    data <- sp_readTable(data, row.names = NULL)
    if (!melted) {
      first_column_variable <- colnames(data)[1]
      if(sp.is.null(xvariable)) {
        xvariable = first_column_variable
      }
      data <-
        melt(data, id.vars = c(ID_var, first_column_variable))
    }
  } else if (class(data) == "data.frame") {
    if (!melted) {
      if(sp.is.null(xvariable)) {
        xvariable = colnames(data)[1]
      }
    }
  } else if (class(data) != "data.frame") {
    stop("Unknown input format for `data` parameter.")
  }

  if(sp.is.null(xvariable) || sp.is.null(yvariable)){
    stop('xvariable or yvariable must be specified!')
  }

  if(sp.is.null(legend_variable)) {
    legend_variable = xvariable
  }

  # print(data)
  if(!sp.is.null(metadata)){
    if (class(metadata) == "character"){
      metadata <- sp_readTable(metadata, row.names = NULL)
   }else if (class(metadata) != "data.frame") {
      stop("Unknown input format for `metadata` parameter.")
    }
    # return(list(data=data, metadata=metadata))
    matched_column <- get_matched_columns_based_on_value(data, metadata,
                                                         only_allow_one_match=T)

    # return(list(data=data, metadata=metadata, matched_column=matched_column))
    data <- merge(data, metadata, by.x=matched_column[1], by.y=matched_column[2])
  }

  data_colnames <- colnames(data)

  if(! (xvariable %in% data_colnames && yvariable %in% data_colnames)){
    stop(paste(xvariable,'or',yvariable,'must be column names of data!'))
  }



  if (yaxis_scale_mode != "") {
    # print(y_add)
    # Give the minimum non-zero value to add to avoid log2(0)
    if (y_add == 0) {
      y_add = sp_determine_log_add(data[[yvariable]])
      # print(paste("153", y_add))
    }
    # print("155")
    # print(data[[yvariable]])
    data[[yvariable]] <- data[[yvariable]] + y_add
    if (yaxis_scale_mode == "log2") {
      data[[yvariable]] <- log2(data[[yvariable]])
    }else if (yaxis_scale_mode == "log10") {
      data[[yvariable]] <- log10(data[[yvariable]])
    }
  }

  if (!sp.is.null(legend_variable_cut)) {
    data[[legend_variable]] <- cut(data[[legend_variable]], legend_variable_cut)
  } else if (!sp.is.null(legend_variable_order)) {
    data = sp_set_factor_order(data, legend_variable, legend_variable_order)
  }

  # May be not needed. Comment out first.
  # data[[legend_variable]] <- as.factor(data[[legend_variable]])

  # A little more complex logic
  # xvariable != legend_variable
  # xvariable == legend_variable and legend not cut
  if (!sp.is.null(xvariable_cut) && (
    (xvariable != legend_variable) || sp.is.null(legend_variable_cut)) ) {
    data[[xvariable]] <- cut(data[[xvariable]], xvariable_cut)
  } else if (!sp.is.null(xvariable_order)) {
    data = sp_set_factor_order(data, xvariable, xvariable_order)
  }

  data[[xvariable]] <- as.factor(data[[xvariable]])

  if (!sp.is.null(facet)) {
    data = sp_set_factor_order(data, facet, facet_level)
  }

  xvariable_en = sym(xvariable)
  yvariable_en = sym(yvariable)
  legend_variable_en = sym(legend_variable)

  p <- ggplot(data, aes(!!xvariable_en,!!yvariable_en))

  if (violin) {
    p <- p + geom_violin(
      aes(fill = !!legend_variable_en),
      stat = "ydensity",
      trim = TRUE,
      scale = scale_violin,
      position = position_dodge(width=0.9)
    ) +
      geom_boxplot(
        aes(fill = !!legend_variable_en),
        alpha = .25,
        width = 0.15,
        position = position_dodge(width = .9),
        outlier.colour = 'NA'
      ) +
      stat_summary(
        aes(group = !!legend_variable_en),
        fun = mean,
        geom = "point",
        fill = "black",
        shape = 19,
        size = 1,
        position = position_dodge(width = .9)
      )
  } else if (violin_nb) {
    p <- p + geom_violin(
      aes(fill = !!legend_variable_en),
      stat = "ydensity",
      #position = "dodge",
      trim = TRUE,
      scale = scale_violin,
      position = position_dodge(width = .9)
    )
  } else if (jitter) {
    p <-
      p + geom_quasirandom(aes(
        colour = !!legend_variable_en,
        group = !!legend_variable_en
      ),
      groupOnX = NULL,
      position = position_dodge(width = .9))
    p <-
      p + stat_summary(
        fun = "mean",
        geom = "text",
        label = "+",
        size = 10,
        color = "black",
        position = position_dodge(width = .9)
      )
  } else {
    if (notch) {
      if (outlier) {
        p <- p + geom_boxplot(
          aes(fill = !!legend_variable_en),
          notch = TRUE,
          notchwidth = 0.3,
          outlier.colour = 'NA'
        )
      } else{
        p <- p + geom_boxplot(aes(fill = !!legend_variable_en),
                              notch = TRUE,
                              outlier.colour = 'red',
                              notchwidth = 0.3,
                              position = position_dodge(width = .9))
      }
    } else {
      if (outlier) {
        p <- p + geom_boxplot(aes(fill = !!legend_variable_en),
                              outlier.colour = 'NA',
                              position = position_dodge(width = .9))
      } else{
        p <- p + geom_boxplot(aes(fill = !!legend_variable_en),
                              outlier.colour = 'red',
                              position = position_dodge(width=0.9))
      }
    }
  }

  if (jitter_bp) {
    p <-
      p + geom_quasirandom(
      # p + geom_point(
        aes(group = !!legend_variable_en),
        varwidth = T,
        groupOnX = TRUE,
        dodge.width = 0.9,
        position = position_dodge(width = .9)
      )
    # p <- p + geom_dotplot(binaxis = 'y',
    #                       aes(group = !!legend_variable_en),
    #                       position = position_dodge(width = .9),
    #              stackdir = 'center',
    #              stackratio = 1.5,
    #              binwidth = .1,
    #              binpositions = "all",
    #              dotsize = 0.6,
    #              alpha = .75,
    #              fill = "lightseagreen",
    #              colour = "lightseagreen",
    #              na.rm = TRUE)
  }

  if(!sp.is.null(group_variable_for_line)){
    group_variable_for_line_en = sym(group_variable_for_line)
    if(! (group_variable_for_line %in% data_colnames)){
      stop(paste(group_variable_for_line,'must be column names of data!'))
    }
    p <- p + geom_line(aes(group=!!group_variable_for_line_en,
                           color=!!group_variable_for_line_en),
                           position=position_quasirandom())
  }


  if (!sp.is.null(yaxis_scale_mode) && (yaxis_scale_mode  != "log2") &&
      (yaxis_scale_mode  != "log10")) {
    p <- p +  eval(parse(text=yaxis_scale_mode))
    # Do not know why add this
    # p <-
    #   p + stat_summary(
    #     fun.y = "mean",
    #     geom = "text",
    #     label = "----",
    #     size = 10,
    #     color = "black"
    #   )
  }

  if (statistics){
    # data$combine_xvariable <- paste0(data[[variable]],"_",data[[xvariable]])
    # model = aov(data[[value]] ~ data$combine_xvariable, data = data)
    # if (length(unique(data[[xvariable]])) == 2) {
    #   library(agricolae)
    #   out = LSD.test(model, "data[[xvariable]]", p.adj = "none")
    #   stat = out$groups
    #   data$stat = stat[as.character(data$combine_xvariable), ]$groups
    # } else{
    #   Tukey_HSD = TukeyHSD(model, ordered = TRUE, conf.level = 0.95)
    #   Tukey_HSD_table = as.data.frame(Tukey_HSD$`data$combine_xvariable`)
    #   Tukey.levels = Tukey_HSD$`data$combine_xvariable`[, 4]
    #   Tukey.labels = data.frame(multcompLetters(Tukey.levels)['Letters'])
    #   Tukey.labels$group = rownames(Tukey.labels)
    #   Tukey.labels = Tukey.labels[order(Tukey.labels$group),]
    #   data$stat = Tukey.labels[as.character(data$combine_xvariable), ]$Letters
    # }
    #
    # max=max(data[,c(value)])
    # min=min(data[,value])
    # x = data[,c('combine_xvariable',value)]
    # y = x %>% group_by(combine_xvariable) %>% summarise_(Max=paste('max(',value,')',sep=""))
    # y=as.data.frame(y)
    # colnames(y) <- c("group","Max")
    # rownames(y)=y$group
    # data$y=y[as.character(data$combine_xvariable),]$Max + (max-min)*0.05
    #
    # x1=factor(data$combine_xvariable,levels=c(unique(data$combine_xvariable)))
    # p <- ggplot(data, aes(x1,!!value_en))
    #
    # p <- p + geom_text(data=data, aes(x=combine_xvariable, y=y,
    #                              color=!!xvariable_en, label=stat)) +
    #   theme(axis.text.x=element_text(angle =90,vjust=0.3))

    if(xvariable != legend_variable){
      data$combine__grp__for__statistis_sp <- paste(data[[xvariable]], data[[legend_variable]],sep="___")
    } else {
      data$combine__grp__for__statistis_sp <- data[[xvariable]]
    }
    formula = as.formula(paste(yvariable,"~","combine__grp__for__statistis_sp"))
    # model = aov(data[[yvariable]] ~ data[[xvariable]], data = data)
    # print(formula)
    model = aov(formula, data=data)
    # print(model)
    if (length(unique(data$combine__grp__for__statistis_sp)) == 2) {
      library(agricolae)
      out = LSD.test(model, "combine__grp__for__statistis_sp", p.adj = "none")
      # print(out)
      stat = out$groups
      data$stat = stat[as.character(data$combine__grp__for__statistis_sp), ]$groups
    } else{
      Tukey_HSD = TukeyHSD(model, ordered = TRUE, conf.level = 0.95)
      # return(Tukey_HSD)
      Tukey_HSD_table = as.data.frame(Tukey_HSD$combine__grp__for__statistis_sp)
      Tukey.levels = Tukey_HSD$combine__grp__for__statistis_sp[, 4]
      Tukey.labels = data.frame(multcompLetters(Tukey.levels)['Letters'])
      Tukey.labels$group = rownames(Tukey.labels)
      Tukey.labels = Tukey.labels[order(Tukey.labels$group),]
      data$stat = Tukey.labels[as.character(data$combine__grp__for__statistis_sp), ]$Letters
      # print(data)
    }

    max=max(data[,c(yvariable)])
    min=min(data[,yvariable])
    x = data[,c(xvariable,yvariable,"combine__grp__for__statistis_sp")]
    # y = x %>% group_by(get(xvariable)) %>% summarise_(Max=paste('max(',yvariable,')',sep=""))
    y = x %>% group_by(combine__grp__for__statistis_sp) %>% summarise(Max=max(!!yvariable_en))
    y=as.data.frame(y)
    # print(y)
    colnames(y) <- c("group","Max")
    rownames(y)=y$group
    data$y=y[as.character(data$combine__grp__for__statistis_sp),]$Max + (max-min)*0.05
    # print(data)

    p <- p + geom_text(data=data, aes(x=!!xvariable_en, y=y,
                                      color=!!legend_variable_en,
                                      label=stat,
                                      group = !!legend_variable_en),
                                      position = position_dodge(width=0.9),
                       show.legend = F)

    p <- sp_manual_color_ggplot2(p,
                            data,
                            legend_variable,
                            manual_color_vector)
  }

  if (outlier) {
    stats <- boxplot.stats(data[[yvariable]])$stats
    ylim_zoomin <- c(stats[1] / out_scale , stats[5] * out_scale)
    p <- p + coord_cartesian(ylim = ylim_zoomin)
  }

  p <- sp_manual_fill_ggplot2(p, data, legend_variable, manual_color_vector)


  if (!sp.is.null(facet)) {
    p <- p + facet_wrap(~  .data[[facet]],
                        nrow = nrow ,
                        ncol = ncol ,
                        scale = scales)
  }
  p <- sp_ggplot_layout(p,
                        xtics_angle = xtics_angle,
                        legend.position = legend.position,
                          extra_ggplot2_cmd = extra_ggplot2_cmd,
                          x_label = x_label,
                          y_label = y_label,
                          title = title,
                          coordinate_flip = coordinate_flip,
                  ...)
  p
}
