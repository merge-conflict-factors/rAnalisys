
# Before running a model, you should inform the variables list (predictorsList parameter) below with the desired set of predictors you want to evaluate.
# Also inform a description for the model (modelName parameter)
# and the file name (resultFileName parameter) that will contain the result of the model analysis.
# Look at the example bellow

source("r_GLMAnalysis_Function.R")

response <- "isConflicting" 

predictorsList <- c("existsCommonSlice", "numberOfCommitsGeoAverage")
modelName <- "Model II - Modularity and Size Factors"
resultFileName <- "Model_II.txt"


analysisGLM("sampleProjectsList.csv", response, predictorsList, 0, "true", modelName, resultFileName)