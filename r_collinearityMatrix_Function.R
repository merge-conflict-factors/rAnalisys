
inputDirName = "input" # dir name that contains the file sample to be evaluated
setwd(inputDirName)


#Function to print the collinearity Matrix for further diagnostics
#Parameters:
#csvFile: File with the sample to be evaluated. It contains all variables (dependent and independent ones) collected for each merge scenario in the sample.
#predictorsList: list of independent variables that you want to see the collinearity matrix.

generateCollinearityDiagnosticsMatrix <- function(csvFile, predictorsList){
  fileName = csvFile 
  sample <- read.csv(fileName, header=T)
  attach(sample)
  names(sample)
  
  metricsList <- subset (sample, select = predictorsList)
  res2 <- rcorr(as.matrix(metricsList), type = "spearman")
  tmpMatrix <- upper.tri(res2$r) 
  
  row = rownames(res2$r)[row(res2$r)[tmpMatrix]]
  column = rownames(res2$r)[col(res2$r)[tmpMatrix]]
  cor  =(res2$r)[tmpMatrix]
  p = res2$P[tmpMatrix]
  
  data <- data.frame(row, column, cor,p)
  write.table(data, "Collinearity_Diagnostics_Matrix.csv", sep=",", row.names=FALSE, col.names=TRUE) 
  setwd("..")
  
}
