### HEADER #####################################################################
##' @title Find cutoff to transform abundance values into binary values
##' 
##' @name .getCutoff
##'
##' @description This function finds the best cutoff to transform abundance 
##' values into binary values while optimising sensitivity and specificity 
##' values based on observations
##' 
##' @param Obs a \code{vector} containing binary observed values (\code{0} or 
##' \code{1})
##' @param Fit a \code{vector} containing relative abundance values (between 
##' \code{0} and \code{1})
##' 
##' @examples
##' 
##' vec.obs = c(rep(0, 60), rep(1, 40))
##' vec.pred = c(sample(x = seq(0, 0.2, 0.01), size = 50, replace = TRUE)
##'              , sample(x = seq(0.15, 1, 0.01), size = 50, replace = TRUE))
##' ## plot(vec.obs, vec.pred)
##' 
##' cutoff = .getCutoff(Obs = vec.obs, Fit = vec.pred)
##' str(cutoff)
##' 
##' @export
##'
## END OF HEADER ###############################################################


.getCutoff <- function(Obs, Fit)
{
  #############################################################################
  
  .testParam_notInValues.m("Obs", Obs, c(0, 1))
  .testParam_notBetween.m("Fit", Fit, 0, 1)
  
  #############################################################################
  
  SumObs = sum(Obs)
  LengObs = length(Obs)
  tt = c(100)
  
  Quant = quantile(Fit)
  if (sum(Quant) > 0 && sum(Quant) < 5)
  {
    if (length(unique(Quant)) > 1)
    {
      i = Quant[2]
      a = 2
      while (i <= Quant[5])
      {
        se = sum((Fit >= i)[Obs == 1]) / SumObs
        sp = sum((Fit < i)[Obs == 0]) / (LengObs - SumObs)
        tt[a] = abs(se - sp)
        if (tt[a] > tt[a - 1])
          break
        i = i + ((Quant[5] - Quant[2]) / 1000)
        a = a + 1
      }
      
      b = (i - ((Quant[5] - Quant[2]) / 1000))
      Cut = as.numeric(b)
      
      Sensitivity = 100 * sum((Fit >= b)[Obs == 1]) / SumObs
      Specificity = 100 * sum((Fit < b)[Obs == 0]) / (LengObs - SumObs)
      
      return(list(Cut = Cut
                  , Sensitivity = Sensitivity
                  , Specificity = Specificity
      ))
    } else
    {
      return(list(Cut = unique(Quant)
                  , Sensitivity = 100
                  , Specificity = 0
      ))
    }
  } else
  {
    return(NA)
  }
}