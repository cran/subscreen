#' Systematic screening of study data for subgroups
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
#' pbc$ageg <- pbc$age <= median(pbc$age)
#' pbc$albuming <- pbc$albumin <= median(pbc$albumin)
#' pbc$phosg <- pbc$alk.phos <= median(pbc$alk.phos)
#' pbc$astg <- pbc$ast <= median(pbc$ast)
#' pbc$bilig <- pbc$bili <= median(pbc$bili)
#' pbc$cholg <- pbc$chol <= median(pbc$chol)
#' pbc$copperg <- pbc$copper <= median(pbc$copper)
#'
#' # redefine censoring variable, consider transplant/death as event
#' pbc$event <- pbc$status
#' pbc$event[pbc$event==2] <- 1
#'
#' pbcdat <- pbc[!is.na(pbc$trt), ]
#'
#' # define function the eval_function()
#' hazardratio <- function(x) {
#'    hr <- tryCatch(exp(coxph(Surv(time, event) ~ trt, data=x)$coefficients[[1]]),
#'                  warning=function(w) {NA})
#'    N1 <- sum(x$trt==1)
#'    N2 <- sum(x$trt==2)
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
#' # visualize the results of the subgroup screening with a Shina app
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


  vako <- function(n,k,l){
    ### calculating all possible factor combination
    ### n - total number of factors to choose from
    ### k - minimum number of factors to choose
    ### l - maximum number of factors to choose
    ### result - Matrix with n columns (one per factor)
    ### called in main program
    t(do.call(cbind, lapply(k:l, function(x) utils::combn(n,x,tabulate,nbins=n))))
  }



  ansu <- function(M,StAn){
    ### Calculates the number of subgroups for each row in M
    ### called in main program
    return(apply(M,1,function(x) {x=x*StAn; prod(x[x!=0])}))
  }


  ersu <- function(i){
    ### generates all subgroup analyses for a given row m in M
    m=M[i,]
    S=character();
    S=append(S,names(FFF)[(1:length(m))*m])
    d=plyr::ddply(cbind(FFF,TTT),S,eval_function)
    nfactors=sum(m)
    h=cbind(nfactors,d)
    return(h)
  }


  erli <- function(i){
    ### generates a list of subject numbers
    ### for each subgroup for a given row m in M
    m=M[i,]
    S=character();
    S=append(S,names(FFF)[(1:length(m))*m])
    d=plyr::ddply(cbind(FFF,TTT),S,suse)
    nfactors=sum(m)
    h=cbind(nfactors,d)
    return(h)
  }


  suda <- function(u){
    ### the list u contains the subject identifiers for each subgroup.
    ### u has the same form and dimension as list h.
    ### u is transformed to a matrix U.
    ### the first coulumns (nfactors and faktoren) are not transfered
    for (i in 1:length(u)){
      uelm=u[[i]]
      lesp=dim(uelm)[2]
      ersp=lesp-AaS+1
      uelm=as.matrix(uelm[1:dim(uelm)[1],ersp:lesp])
      if (i==1)
        U=uelm
      else
        U=rbind(U,uelm)
    }
    U
  }



  koda <- function(h, version=2){
    ### kombiniert die in liste h enthaltenen data frames
    ### zu einem data frame H und ergaenzt dabei evt. fehlende spalten

    ### Reihenfolge der spalten herstellen:
    ### auswertungsergebnisse, nfactors, faktoren
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
    ### identifikator der existenten subgruppen definieren
    SGID=1:dim(H)[1]
    return(cbind(SGID,H))
  }

  # if more than one kernel is requested, check for availability of snowfall
  #if (nkernel > 1) {
  #  if (!requireNamespace("snowfall", quietly = TRUE)) {
  #    cat("Warning: Use of more than one kernel requires snowfall to be installed. \n nkernel set to 1 \n")
  #  } else {
  #    requireNamespace("snowfall")
  #    requireNamespace("snow")
  #  }
  #}

### import data



### separate data
### Treatment, target variables and subject-id
TTT=data[, (colnames(data) %in% c(subjectid, treat, endpoints))]

### subgroup defining factors (everything excluding what's in TTT)
if (is.null(factors)) {
  FFF=data[, !(colnames(data) %in% c(subjectid, treat, endpoints))]
  factors <- names(FFF)
} else {
  FFF=data[, (colnames(data) %in% factors)]
}


### number of factors and subjects
    AnFa= dim(FFF)[2] ### number of faktors
    AaS = dim(data)[1] ### number of subjects

### number of factor levels for each factor
    StAn=apply(FFF,2,function(x)length(table(x)))



### Functions eval_function and suse are called in SubScreen_2016_01_22.R by using funktions ersu and erli

### analysis function "eval_function" to be filled in with the statistical evaluation



suse <- function(D){
   ### prolonging the subject row with missing values to the length of AaS
   h=D$SUBJIDN
   h=sort(h)
   if (length(h) < AaS) h=c(h,rep(NA,AaS-length(h)))
   return(h)
}




            pt0 <- proc.time() #preparation time

            ### create all possible factor combinations and get number
            M=vako(AnFa,min_comb,max_comb)
            AnSu=ansu(M,StAn)

            pt1 <- proc.time() #start time
            rM=dim(M)[1]

            if (nkernel > 1) {
              ### parallel evaluation of subgroup analyses
              clus=parallel::makeCluster(nkernel)
              #print(environment())
              parallel::clusterExport(clus, c("FFF", "TTT", "M", "AaS", "eval_function", "suse"), environment())
              parallel::clusterExport(clus, c("ddply"), environment(plyr::ddply))
              parallel::clusterExport(clus, par_functions)
              h <- parallel::parLapplyLB(cl=clus, 1:rM,ersu)
              parallel::stopCluster(clus)
            } else {
              h <- sapply(1:rM, ersu)
            }

            pt2 <- proc.time() #time 2

            ### create data frames from lists
            H=koda(h)

            pt3 <- proc.time() #stop time

            ### elements of z are the existent subgroups
            ### elements of z are smaller as
            ### elements of AnSu, if factor combinations don't exist in D

            z=numeric(); for (i in 1:length(h)) z[i]=dim(h[[i]])[1]

            if (verbose == T) {
            ### Bericht
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

