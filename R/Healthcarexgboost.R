#' Xgboost healthcare
#'
#' healthcare analysis using xgboost algorithm
#' @param  give 10 Important variables
#' @return  It gives json output
#' @export

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(data.table)
library(car)
library(jsonlite)
load("data/xgboost_impvari.RData")

#* @post /predict
predict.default.rate <- function(
  new_APRSeverity,upperage,new_APRDRGCode,new_CCSDiagnosiscode,new_APRMDCCode,
  Log_totalcharge,new_patientDis,new_CCSProcedure,new_AttendingProviderLicense,
  new_OperatingproviderCertificateNumber
) {
  data <- data.table(new_APRSeverity=new_APRSeverity
                     , upperage=upperage
                     , new_APRDRGCode=new_APRDRGCode
                     , new_CCSDiagnosiscode=new_CCSDiagnosiscode
                     , new_APRMDCCode=new_APRMDCCode
                     , Log_totalcharge=Log_totalcharge
                     , new_patientDis=new_patientDis
                     , new_CCSProcedure =new_CCSProcedure
                     , new_AttendingProviderLicense= new_AttendingProviderLicense
                     , new_OperatingproviderCertificateNumber= new_OperatingproviderCertificateNumber
  )
  one_matrix <- data.matrix(data)
  y_pred1 <- predict(bst1, one_matrix)
  class(y_pred1)
  pred1 = matrix(y_pred1, nrow=4, ncol=length(y_pred1)/4)
  pred1 = t(pred1)
  pred1 = max.col(pred1, "last")
  pred.char1 = toupper(letters[pred1])
  pred.char1[pred.char1=="A"]<-"Extreme"
  pred.char1[pred.char1=="B"]<-"Major"
  pred.char1[pred.char1=="C"]<-"Minor"
  pred.char1[pred.char1=="D"]<-"Moderate"
  return(list(default.stringsAsFactors =unbox(as.data.frame(pred.char1[1]))))
}





