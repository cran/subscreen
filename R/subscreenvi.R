#' (iii) Determine variable importance
#'
#' Determine variable importance for continuous, categorical or right-censored
#' survival endpoints (overall and per treatment group) using random forests
#'
#' @param data The data frame containing the dependent and independent variables.
#' @param y The name of the column in \code{data} that contains the dependent variable.
#' @param cens The name of the column in \code{data} that contains the censoring variable, if \code{y} is an event time (default=NULL).
#' @param trt The name of the column in \code{data} that contains the treatment variable (default=NULL).
#' @param x Vector that contains the names of the columns in \code{data} with the independent variables (default=NULL, i.e. all remaining variables)
#' @param ... additional arguments to be passed to function \code{rfsrc}
#' @return A list containing ordered data frames with the variable importances
#'   (one for each treatment level, one with the ranking variability between the
#'   treatment levels and one with the total results)
#'
#' @import randomForestSRC
#' @import plyr
#' @import stats
#' @keywords variable importance
#' @export subscreenvi
#' @examples
#' \dontrun{
#' require(survival)
#' data(pbc)
#' pbc$status <- ifelse(pbc$status==0,0,1)
#' importance <- subscreenvi(data=pbc, y='time', cens='status', trt='trt', x=NULL)
#' }

subscreenvi <- function(data, y, cens=NULL, x=NULL, trt=NULL, ...){
  
  Importance <- NULL
  # transform character to factor
  if(any(sapply(data, class)=='character')){
    data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], factor)
  }
  
  # build formula
  if (is.null(x)) x <- setdiff(colnames(data),c(trt, y,cens))
  
  x.form <- paste(x, collapse='+')
  
  if (!is.null(cens)){
    mod.form <- paste0('Surv(',y,', ',cens,')~',x.form)
  }else{
    mod.form <- paste0(y,'~',x.form)
  }
  # empty list for results
  result <- list()
  tmp <- list()
  # fit random forest for each treatment level and save variable importance
  
  if (!is.null(trt)){
    
    trt.lev <- levels(factor(data[,trt]))
    
    for (i in 1:length(trt.lev)){
      
      fit <- rfsrc(as.formula(mod.form), data=data[data[,trt]==trt.lev[i],], importance=TRUE, ...)
      vi <- sort(fit$importance, decreasing=TRUE)
      res.df <- data.frame('Variable'=names(vi), 'Importance'=vi)
      rownames(res.df) <- NULL
      result[[paste0('VI.trt.',trt.lev[i])]] <- res.df
      
      res.df[,2] <- 1:nrow(res.df)
      colnames(res.df)[2] <- paste0('rank',i)
      tmp[[paste0('VI.trt.',trt.lev[i])]] <- res.df
      
    }
    
    # summarize trt level results: rank variability over treatment levels
    tmp <- join_all(tmp, by='Variable', type='full')
    tmp$'Importance' <- apply(tmp[,-1], MARGIN=1, FUN=var)
    tmp <- arrange(tmp, desc(Importance))
    result[['VI.RV.trt']] <- tmp[,c('Variable','Importance')]
    
  }
  
  # fit random forest for total data set and save variable importance 
  fit <- rfsrc(as.formula(paste0(mod.form,'+',trt)), data=data, importance=TRUE, ...)
  vi <- sort(fit$importance, decreasing=TRUE)
  res.df <- data.frame('Variable'=names(vi), 'Importance'=vi)
  rownames(res.df) <- NULL
  result[['VI.total']] <- res.df[res.df$Variable!=trt,]
  
  return(result)
  
}

