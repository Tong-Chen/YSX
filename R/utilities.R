
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#   Generate DOC:              'Ctrl + Shift + Alt + r'




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
generateAbundanceDF <- function(type="Gene", mean=20, nGene=15, nGrp=2, nSample=3) {
  df <- as.data.frame(matrix(rnorm(nGene*nGrp*nSample, mean=mean), nrow=nGene))
  colnames(df) <- paste("Samp", paste(rep(LETTERS[1:nGrp], each=nSample), rep(1:nSample, nGrp), sep="_"), sep="_")
  rownames(df) <- paste(type, letters[1:nGene], sep="_")
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
Matrix2colCorrelation <- function(mat, method="pearson", digits=4, cor_file=NULL) {
  pearson_cor <- round(as.matrix(cor(mat, method=method)),digits=digits)
  hc <- amap::hcluster(t(mat), method=method)
  pearson_cor <- pearson_cor[hc$order, hc$order]
  if(!is.null(file)){
    pearson_cor_output = data.frame(id=rownames(pearson_cor), pearson_cor)
    write.table(pearson_cor_output, file=cor_file,
                quote=F, sep="\t", row.names=F, col.names=T)
  }
  return(list(pearson_cor=pearson_cor,hc=hc))
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
get_lower_tri <- function(cormat){
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
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}



# options(scipen=999)

numCheck <- function(x){
	# Function get from https://stackoverflow.com/questions/10674992/convert-a-character-vector-of-mixed-numbers-fractions-and-integers-to-numeric?rq=1
	# With little modifications
  x <- sapply(x, as.character)
  is.integer  <- grepl("^-?\\d+$", x)
  is.fraction <- grepl("^-?\\d+\\/\\d+$", x)
  is.float <- grepl("^-?\\d+\\.\\d+$", x)
  is.percent <- grepl("[0-9.]+%$", x)
  is.mixed    <- grepl("^-?\\d+ \\d+\\/\\d+$", x)
  return(all(is.integer | is.fraction | is.float | is.mixed | is.percent))
}

mixedToFloat <- function(x){
  x <- sapply(x, as.character)
  is.integer  <- grepl("^-?\\d+$", x)
  is.fraction <- grepl("^-?\\d+\\/\\d+$", x)
  is.float <- grepl("^-?\\d+\\.\\d+$", x)
  is.mixed    <- grepl("^-?\\d+ \\d+\\/\\d+$", x)
  is.percent <- grepl("[0-9.]+%$", x)
  stopifnot(all(is.integer | is.fraction | is.float | is.mixed | is.percent))

  numbers <- strsplit(x, "[ /%]")

  ifelse(is.integer,  as.numeric(sapply(numbers, `[`, 1)),
  ifelse(is.percent,  as.numeric(sapply(numbers, `[`, 1))/100,
  ifelse(is.float,    as.numeric(sapply(numbers, `[`, 1)),
  ifelse(is.fraction, as.numeric(sapply(numbers, `[`, 1)) /
                      as.numeric(sapply(numbers, `[`, 2)),
                      as.numeric(sapply(numbers, `[`, 1)) +
                      as.numeric(sapply(numbers, `[`, 2)) /
					  as.numeric(sapply(numbers, `[`, 3))))))

}

#mixedToFloat(c('1 1/2', '2 3/4', '2/3', '11 1/4', '1'))



