#Run this function for correlation analysis among one numeric dependent variable and N binary independent variables

source("r_PredictorsCorrAnalysis_Function.R")


# Before running this script you should inform:
# 1) fileInput: the file name contained the sample to be evaluated 
# 2) fileResult: the name for the file that will contain the resulting outome 
# 3) predictorsList: list of variables you want to evaluate at once. The first item must be the dependent numeric variable. The remained items are the binary independent variables.   
# Look at the example bellow

fileInput = "allProjects_AllVariables.csv" 
fileResult = "NumberOfFilesWithConflicts_ModularityFactor.csv"
predictorsList <- c('conflictingFilesNumber','existsCommonSlice')

generateBinaryPredictorsCorrelationAnalysis(fileInput, predictorsList, fileResult)