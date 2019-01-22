source("r_collinearityMatrix_Function.R")

# Before running this script you should inform:
# 1) fileName: the file name contained the sample to be evaluated 
# 2) predictorsList: list of independent variables that you want to see the collinearity matrix   
# Look at the example bellow

fileName = "allProjects_AllVariables.csv" 
predictorsList <- c('existsCommonSlice','numberOfCommitsGeoAverage','numberOfAuthorsGeoAverage','numberOfChangedFilesGeoAverage','numberOfChangedLinesGeoAverage','minimumLifeTimeGeoAverage','contributionConclusionDelay')

generateCollinearityDiagnosticsMatrix(fileName, predictorsList)
