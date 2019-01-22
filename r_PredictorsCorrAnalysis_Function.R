
inputDirName = "input" # dir name that contains the file sample to be evaluated
setwd(inputDirName)

#Function for correlation analysis among one numeric dependent variable and N numeric independent variables
#Parameters:
#csvFile: File with the sample to be evaluated. It contains all variables (dependent and independent ones) collected for each merge scenario in the sample.
#predictorsList: list of variables you want to evaluate at once. The first item must be the dependent variable. The remained items are the independent variables.   
#fileResult: the file with the function outcome.

generateNumericPredictorsCorrelationAnalysis <- function(csvFile, predictorsList, csvResult){
  
  fileName = csvFile#csvFile 
  sample <- read.csv(fileName, header=T)
  # attach(sample)
  # names(sample)
  metricsList <- subset (sample, select = predictorsList)
  res2 <- rcorr(as.matrix(metricsList), type = "spearman")
  tmpMatrix <- upper.tri(res2$r) 

  row = rownames(res2$r)[row(res2$r)[tmpMatrix]]
  column = rownames(res2$r)[col(res2$r)[tmpMatrix]]
  cor  =(res2$r)[tmpMatrix]
  p = res2$P[tmpMatrix]
  
  data <- data.frame(row, column, cor,p)
  finalData <-  data[which(data[,'row'] == predictorsList[1]), ]
  fileResult <- csvResult
  write.table(finalData, fileResult, sep=",", row.names=FALSE, col.names=TRUE, append=TRUE) 
  setwd("..")
  
}


#Function for correlation analysis among one numeric dependent variable and N binary independent variables

#The point-biserial correlation is mathematically equivalent to the Pearson (product moment) correlation, that is, 
#if we have one continuously measured variable X and a dichotomous variable Y. So, we use Pearson correlation in this 
#script. However, you can execute the point-biserial correlation by using the following R function: biserial.cor(x,y), 
#wherein x represents the numeric continuos variable and y represents the binary variable. Before running this the point-biserial R function, make sure you have installed the package "ltm".

#Parameters:
#csvFile: File with the sample to be evaluated. It contains all variables (dependent and independent ones) collected for each merge scenario in the sample.
#predictorsList: list of variables you want to evaluate at once. The first item must be the dependent variable. The remained items are the independent variables.   
#fileResult: the files with the function outcome.

generateBinaryPredictorsCorrelationAnalysis <- function(csvFile, predictorsList, csvResult){

  fileName = csvFile#csvFile 
  sample <- read.csv(fileName, header=T)
  # attach(sample)
  # names(sample)
  metricsList <- subset (sample, select = predictorsList)
  res2 <- rcorr(as.matrix(metricsList), type = "pearson")
  tmpMatrix <- upper.tri(res2$r) 
  
  row = rownames(res2$r)[row(res2$r)[tmpMatrix]]
  column = rownames(res2$r)[col(res2$r)[tmpMatrix]]
  cor  =(res2$r)[tmpMatrix]
  p = res2$P[tmpMatrix]
  
  data <- data.frame(row, column, cor,p)
  finalData <-  data[which(data[,'row'] == predictorsList[1]), ]
  fileResult <- csvResult
  write.table(finalData, fileResult, sep=",", row.names=FALSE, col.names=TRUE, append=TRUE) 
  setwd("..")
  
}
