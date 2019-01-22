# rAnalisys

The rAnalisys scripts are part of an empirical study infrastructure that aims to analyze merge conflict predictors.

More information here: https://merge-conflict-factors.github.io/merge-conflict-factors/

rAnalisys is a set of R scripts for assessing logistic regression models and correlation analysis we use in this study. They get as input the output result of <a href="https://github.com/merge-conflict-factors/predictorsCollect" title="">predictorsCollect</a> script, more specifically the file named allProjects_AllVariables.csv. This file represents the aggregated sample for this study and contains all variables (dependent and independent ones) needed to run the statistical analysis we present in the paper. To run these scripts, execute the following instructions (we use Linux, so you have to use the correspondent command to reach the same, in case other OS use):

****** Install R distribution and used packages
1) You need to install the R distribution package, in case it does not become available on your machine: sudo apt-get install r-cran-car.
2) After installing R, make sure whether the following packages "aod", "car", and "Hmisc" are installed. Another way, you also need to install those and load their libraries to make the commands and functions available, as follows:

     a) install.packages('aod')
     
        library("aod")
     
     b) install.packages('car')
     
        library("car")
     
     c) install.packages("Hmisc")
     
        library("Hmisc")
     

****** Set up the sample input for analysis

1) Copy the allProjects_AllVariables.csv file (generate as output from <a href="https://github.com/merge-conflict-factors/predictorsCollect" title="">predictorsCollect</a> script) to the input folder in this project.

2) Configure the sampleProjectsList.csv with the name of the file in step 1 above. 

3) Now you can run the analysis the analysis available, as described in the following sections.

OBS: Although in our study we are mainly interested in evaluating the aggregated sample (allProjects_AllVariables.csv), our script can evaluate a list of different samples (for instance, individual project repositories) passed at a once. All you need to do is to set the N files, representing the N samples in that file. 

****************************************************
****** To run the logistic regression Analysis *****
****************************************************
<b>Check for collinearity</b> (If your model contais more then one independent variable)

Since in this study, we evaluate more than one variable in the same model, you need to check the collinearity among these variables, as required when performing regression models. So, if two variables have a strong correlation, they can not be put together in the same model. For instance, if you want to assess variables A, B, and C, you first need to check collinearity among them. Thus, if the correlation between A and C, for instance, is 0.7, then they are strongly correlated. We consider a threshold above 0.59 to state that two variables are strongly correlated. So, you need to choose which variable (in this example, A or C) should be considered to perform a given model, since performing strongly correlated variables in the same model lead to inconsistent outcomes. Back to the example, consider that the variable B has a weak (or even moderate) correlation with both A and C variables. Then you could test two different models: a) one model with variables A and B and b) another model with B and C variables.

So, which variables to choose to assess in a regression model demands a human decision, because it depends on the researcher interest since all variables can be tested based on different combinations, which largely increases depending on the number of variables. Furthermore, models can also be evaluated with only one variable. In this case, no correlation check is needed. 
 
To visualize the variables collinearity matrix, run the script executeCollinearityMatrixDiagnostics.R by using the command: 

Rscript executeCollinearityMatrixDiagnostics.R


As result, the Collinearity_Diagnostics_Matrix.csv file is generate into the input folder. This file shows the correlation value in a pair-wise way. So, by visualizing the collinearity matrix, you can decide how to assemble the variables that will be tested and how many models do you want to test. Finally, you can use our script to asses that, as described below.

<b>Run the Logistic Regression Model analysis</b>

Before running the logistic regression analysis, you should alter the file executeGLMAnalysis.R in the following way:

1) Inform the variables list (predictorsList parameter) with the set of predictors you want to evaluate.
2) Also, inform a description for the model (modelName parameter) and the file name (resultFileName parameter) that will contain the result of the model analysis.

Look at the example below (To each new model you desire to test, you should edit these parameters accordingly)

- predictorsList <- c("existsCommonSlice", "numberOfCommitsGeoAverage")

- modelName <- "Model II - Modularity and Size Factors"

- resultFileName <- "Model_II.txt"

3) Once you have informed the desired parameters, run the script executeGLMAnalysis.R by using the command: 

Rscript executeGLMAnalysis.R

4) After that, a folder named MultipleGLM_Results will be automatically created with the outcome generated in a file named as you informed.  

********************************************************
****** To run the Predictors Correlation Analysis ******
********************************************************

<b> Binary Predictors Correlation Analysis </b>

Before running the correlation analysis for binary predictors, you should alter the file executeBinaryPredictorsCorrAnalysis.R in the following way:

1) fileInput parameter: the file name contained the sample to be evaluated 
2) fileResult parameter: the name for the file that will contain the resulting outcome 
3) predictorsList parameter: the list of variables you want to evaluate at once. The first item must be the numeric dependent variable. The remained items are the binary independent variables.

Look at the example below:

- fileInput = "allProjects_AllVariables.csv" 
- fileResult = "NumberOfFilesWithConflicts_ModularityFactor.csv"
- predictorsList <- c('conflictingFilesNumber','existsCommonSlice')

4) Once you have informed the desired parameters, run the script executeBinaryPredictorsCorrAnalysis.R by using the command: 

Rscript executeBinaryPredictorsCorrAnalysis.R

5) After that, a file named as you informed will be generated in the input folder.  


<b> Numeric Predictors Correlation Analysis </b>

Before running the correlation analysis for numeric predictors, you should alter the file executeNumericPredictorsCorrAnalysis.R in the following way:

1) fileInput parameter: the file name that contains the sample to be evaluated 
2) fileResult parameter: the name for the file that will contain the resulting outcome 
3) predictorsList parameter: the list of variables you want to evaluate at once. The first item must be the numeric dependent variable. The remained items are the numeric independent variables.

Look at the example below:

- fileInput = "allProjects_AllVariables.csv" 
- fileResult = "NumberOfFilesWithConflicts_SizeFactor.csv"
- predictorsList <- c('conflictingFilesNumber','numberOfCommitsGeoAverage','numberOfAuthorsGeoAverage','numberOfChangedFilesGeoAverage','numberOfChangedLinesGeoAverage')

4) Once you have informed the desired parameters, run the script executeNumericPredictorsCorrAnalysis.R by using the command: 

Rscript executeNumericPredictorsCorrAnalysis.R

5) After that, a file named as you informed will be generated in the input folder.  
