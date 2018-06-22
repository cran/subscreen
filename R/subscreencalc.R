#' (i) Calculation of the results for the subgroups
#'
#' This function systematically calculates the defined outcome for every combination of subgroups
#' up to the given level (max_comb), i.e. the number of maximum combinations of subgroup defining factors.
#' If, e.g., in a study sex, age group (<=60, >60), BMI group (<=25, >25) are of interest, subgroups of level 2
#' would be, e.g, male subjects with BMI>25 or young females, while subgroups of level 3 would be any combination
#' of all three variables.
#'
#' The evaluation function (eval_function) has to defined by the user. The result needs to be a vector of numerical values,
#' e.g., outcome variable(s) and number of observations/subjects. The input of eval_function is a data frame with the same
#' structure as the input data frame (data) used in the subsreencalc call. See example below.
#' Potential errors occurring due to small subgroups should be caught and handled within eval_function.
#' As the eval_function will be called with every subgroup it may happen that there is only one observation or
#' only one treatment arm or only observations with missing data going into the eval_function. There should always be valid
#' result vector be returned (NAs allowed) and no error causing program abort.
#' For a better display the results may be cut-off to a reasonable range. For example: If my endpoint is a hazard ratio
#' that is expected to be between 0.5 and 2 I would set all values smaller than 0.01 to 0.01 and values above 100 to 100.
#'
#'
#' @param data dataframe with study data
#' @param eval_function name of the function for data analysis
#' @param endpoints vector containing the names of the endpoint variables in data
#' @param treat name of variable in data that contains the treatment identfier, defaults to trtp
#' @param subjectid name of variable in data that contains the subject identifier, defaults to subjid
#' @param factors vector containg the names of variables that define the subgroups, defaults to NULL. If set to NULL, all variables in data are used that are not included in subjectid, treat, and endpoints
#' @param min_comb minimum number of factor combination levels to define subgroups, defaults to 1
#' @param max_comb maximum number of factor combination levels to define subgruops, defaults to 3
#' @param nkernel number of kernels for parallelization (defaults to 1)
#' @param par_functions vector of names of functions used in eval_function to be exported to cluster (needed only if nkernel > 1)
#' @param verbose switch on/off output of computational information
#' @return an object of type SubScreenResult of the form
#' list(sge=H,
#'      max_comb=max_comb,
#'      min_comb=min_comb,
#'      subjectid=subjectid,
#'      endpoints=endpoints,
#'      treat=treat,
#'      factors=factors,
#'      results_total=eval_function(cbind(F,T)))
#' @keywords subgroup analysis
#' @export subscreencalc
#' @examples
#' # get the pbc data from the survival package
#' require(survival)
#' data(pbc, package="survival")
#'
#' # generate categorical versions of some of the baseline covariates
#' pbc$ageg[!is.na(pbc$age)]        <-
#'    ifelse(pbc$age[!is.na(pbc$age)]          <= median(pbc$age,     na.rm=TRUE), "Low", "High")
#' pbc$albuming[!is.na(pbc$albumin)]<-
#'    ifelse(pbc$albumin[!is.na(pbc$albumin)]  <= median(pbc$albumin, na.rm=TRUE), "Low", "High")
#' pbc$phosg[!is.na(pbc$alk.phos)]  <-
#'    ifelse(pbc$alk.phos[!is.na(pbc$alk.phos)]<= median(pbc$alk.phos,na.rm=TRUE), "Low", "High")
#' pbc$astg[!is.na(pbc$ast)]        <-
#'    ifelse(pbc$ast[!is.na(pbc$ast)]          <= median(pbc$ast,     na.rm=TRUE), "Low", "High")
#' pbc$bilig[!is.na(pbc$bili)]      <-
#'    ifelse(pbc$bili[!is.na(pbc$bili)]        <= median(pbc$bili,    na.rm=TRUE), "Low", "High")
#' pbc$cholg[!is.na(pbc$chol)]      <-
#'    ifelse(pbc$chol[!is.na(pbc$chol)]        <= median(pbc$chol,    na.rm=TRUE), "Low", "High")
#' pbc$copperg[!is.na(pbc$copper)]  <-
#'    ifelse(pbc$copper[!is.na(pbc$copper)]    <= median(pbc$copper,  na.rm=TRUE), "Low", "High")
#'
#' pbc$ageg[is.na(pbc$age)]         <- "No Data"
#' pbc$albuming[is.na(pbc$albumin)] <- "No Data"
#' pbc$phosg[is.na(pbc$alk.phos)]   <- "No Data"
#' pbc$astg[is.na(pbc$ast)]         <- "No Data"
#' pbc$bilig[is.na(pbc$bili)]       <- "No Data"
#' pbc$cholg[is.na(pbc$chol)]       <- "No Data"
#' pbc$copperg[is.na(pbc$copper)]   <- "No Data"

#' # redefine censoring variable, consider transplant/death as event
#' pbc$event <- pbc$status
#' pbc$event[pbc$event==2] <- 1
#'
#' pbcdat <- pbc[!is.na(pbc$trt), ]
#'
#' # define function the eval_function()
#' # Attention: The eval_function ALWAYS needs to return a dataframe with one row.
#' #            Include exception handling, like if(N1>0 && N2>0) hr <- exp(coxph(...) )
#' #            to avoid program abort due to errors
#'
#' hazardratio <- function(x) {
#'    N1 <- sum(x$trt==1)
#'    N2 <- sum(x$trt==2)
#'
#'    hr <- tryCatch(exp(coxph(Surv(time, event) ~ trt, data=x)$coefficients[[1]]),
#'                  warning=function(w) {NA})
#'
#'    data.frame(N1=N1, N2=N2, hr=hr)
#'  }
#'
#'  # run subscreen
#'
#' results <- subscreencalc(data=pbcdat,
#'                      eval_function=hazardratio,
#'                      endpoints = c("time", "event"),
#'                      treat="trt",
#'                      subjectid = "id",
#'                      factors=c("ageg", "sex", "bilig", "cholg", "copperg", "astg",
#'                                "albuming", "phosg"))
#'
#' # visualize the results of the subgroup screening with a Shiny app
#' \dontrun{subscreenshow(results, PreSelectTarge="hr", PreSelectXAxis="N1")}


subscreencalc <- function(data,
                      eval_function,
                      endpoints,
                      treat="trtp",
                      subjectid="subjid",
                      factors=NULL,
                      min_comb=1,
                      max_comb=3,
                      nkernel=1,
                      par_functions="",
                      verbose=T) {

  # checks for parameters
  if (!is.data.frame(data)) { stop("Error! subscreen: data has to be a dataframe")}

  if (!is.function(eval_function)) { stop("Error! subscreen: eval_function has to be a function")}

  if ( !is.null(treat) && (!treat == "") && !(treat %in% names(data))) { stop("Error! subscreen: specified treatment variable not in dataframe")}

  if (!(subjectid %in% names(data))) {stop("Error! subscreen: specified subject variable not in dataframe")}

  if (!is.numeric(min_comb)) {stop("Error! subscreen: min_comb has to be a number")}

  if (!is.numeric(max_comb)) {stop("Error! subscreen: max_comb has to be a number")}

  if (!is.null(factors) && (length(factors) != sum(factors %in% names(data)))) {
    for (item in factors) {
      if (!(item %in% names(data))) {stop("Error! subscreen: not all variables in factors included in data")}
    }
  }

  #   if more than one kernel is requested, check for availability of parallel
  if (nkernel > 1) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      cat("Warning: Use of more than one kernel requires package parallel to be installed. \n nkernel set to 1 \n")
    }
  }


  #######################################################################################
  ### Subgroup generation                                                             ###
  #######################################################################################


  createCombinationMatrix <- function(n,k,l){
    ### calculating all possible factor combination
    ### n - total number of factors to choose from
    ### k - minimum number of factors to choose
    ### l - maximum number of factors to choose
    ### result - Matrix with n columns (one per factor)
    ### called in main program
    t(do.call(cbind, lapply(k:l, function(x) utils::combn(n,x,tabulate,nbins=n))))
  }



  sugruCount <- function(M,StAn){
    ### Calculates the number of subgroups for each row in M
    ### called in main program
    return(apply(M,1,function(x) {x=x*StAn; prod(x[x!=0])}))
  }


  sugruCalc <- function(i){
    ### generates all subgroup analyses for a given row m in M
    m=M[i,]
    S=character();
    S=append(S,names(FFF)[(1:length(m))*m])
    d=plyr::ddply(cbind(FFF,TTT),S,eval_function)
    nfactors=sum(m)
    h=cbind(nfactors,d)
    return(h)
  }



  combineDataFrame <- function(h, version=2){
    ### combines the data frames in list h
    ### to the data frame H and fills the missing columns

    ### Set order of the columns: results, "nfactors", factors
    if (version==1) {
      hf=c("nfactors",names(FFF))
      ha=names(h[[1]]); for (i in 2:dim(M)[1]) ha=union(ha,names(h[[i]]))
      ht=setdiff(ha,hf)
      hn=union(ht,hf)

      for (i in 1:dim(M)[1]){
        for(j in hn){
          if (sum(j == names(h[[i]])) == 0){
            h[[i]][j]=rep(NA,dim(h[[i]])[1])
          }
        }
      }

      H=h[[1]]
      for (i in 2:dim(M)[1]){
        H=rbind(H,h[[i]])
      }
    }

    if (version==2) {
      H=as.data.frame(data.table::rbindlist(h, use.names=TRUE, fill=TRUE))
    }
    if (version==3) {
      H=do.call("rbind", h)
    }
    ### identification variable for the subgroup SGID
    SGID=1:dim(H)[1]
    return(cbind(SGID,H))
  }


### import data



### separate data
### Treatment, target variables and subject-id
TTT=data[, (colnames(data) %in% c(subjectid, treat, endpoints))]

### subgroup defining factors (as listed or everything excluding what's in TTT)
if (is.null(factors)) {
  FFF=data[, !(colnames(data) %in% c(subjectid, treat, endpoints))]
  factors <- names(FFF)
} else {
  FFF=data[, (colnames(data) %in% factors)]
}


### number of factors and subjects
    AnFa= dim(FFF)[2]  ### number of factors
    AaS = dim(data)[1] ### number of subjects

### number of factor levels for each factor
    StAn=apply(FFF,2,function(x)length(table(x)))



### analysis function "eval_function" to be filled in with the statistical evaluation




            pt0 <- proc.time() #preparation time

            ### create all possible factor combinations and get number
            M=createCombinationMatrix(AnFa,min_comb,max_comb)
            AnSu=sugruCount(M,StAn)

            pt1 <- proc.time() #start time
            rowsM=dim(M)[1]

            if (nkernel > 1) {
              ### parallel evaluation of subgroup analyses
              clus=parallel::makeCluster(nkernel)
              #print(environment())
              parallel::clusterExport(clus, c("FFF", "TTT", "M", "AaS", "eval_function"), environment())
              parallel::clusterExport(clus, c("ddply"), environment(plyr::ddply))
              if (par_functions!="") parallel::clusterExport(clus, par_functions)
              h <- parallel::parLapplyLB(cl=clus, 1:rowsM,sugruCalc)
              parallel::stopCluster(clus)
            } else {
              # turn off simplify to avoid error if max_comb=1
              h <- sapply(1:rowsM, sugruCalc, simplify = FALSE)
            }

            pt2 <- proc.time() #time 2

            ### create data frames from lists

            H=combineDataFrame(h)

            pt3 <- proc.time() #stop time

            ### elements of z are the existent subgroups
            ### elements of z are smaller as
            ### elements of AnSu, if factor combinations don't exist

            z=numeric(); for (i in 1:length(h)) z[i]=dim(h[[i]])[1]

            if (verbose == T) {
            ### report creation
            cat("\n","Number of Subjects                     ",AaS,          "\n",
                     "Number of Subgroup Factors             ",AnFa,         "\n",
                     "Potential Subgroups                    ",sum(AnSu),    "\n",
                     "Non-existent/empty Subgroups           ",sum(AnSu-z),  "\n",
                     "Existent Subgroups                     ",sum(z),       "\n\n",
                     "Time for SG Analyses (s)               ",pt2-pt1,      "\n",
                     "Time for creating Data Frames (s)      ",pt3-pt2,      "\n",
                     "Overall time used (HH:MM:SS)           ",paste( ((pt3-pt0)[3]/3600)%/%1, (((pt3-pt0)[3]/60)%/%1)%%60, round((pt3-pt0)[3]%%60, digits=4), sep=":"), "\n"
            )
            }


            H <- list(sge=H,
                      max_comb=max_comb,
                      min_comb=min_comb,
                      subjectid=subjectid,
                      endpoints=endpoints,
                      treat=treat,
                      factors=factors,
                      results_total=eval_function(cbind(FFF,TTT)))

            class(H) <- "SubScreenResult"
            H
}

