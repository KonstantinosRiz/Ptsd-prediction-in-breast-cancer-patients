myPath <- r"{D:\Κωνσταντίνος Data\Σχολής\Διπλωματική Εργασία\Main\Dataset}"
setwd(myPath)

load("DataForPCL_20June22.Rdata")


var(dataPCL$Depression_HADS.0)


########## Split the dataset into potential features and potential labels


features <- subset(dataPCL, select = c(Depression_HADS.0 : M3_PTGI_appreciation_of_life,
                                       M3_MAC_helpless : Sex_Enjoy_BR23.3))

labels <- subset(dataPCL, select = M6_ptsd_PCL5 : M18_DSMdiagnosis_PCL)


########## Decision Tree with few features and label M6_DSMdiagnosis_PCL


# Clean the missing values and drop the first (useless) column
dataPCL_clean_M6 <- subset(dataPCL, !is.na(M6_DSMdiagnosis_PCL))
dataPCL_clean_M6$mrn <- NULL

# Final try
library(caret)
library(rpart)
library(rpart.plot)

data_clean <- subset(dataPCL_clean_M6, !is.na(education_2))

# If I use knnImputation for the missing values it automatically standardizes the data and if I use that I have to keep the mean and std
# values to unstandardize the data and then round features like age to integers (and make the column integer again)

# For now I use median imputation for numeric variables

options(repr.plot.width = 6, repr.plot.height = 5)

my_tree <- rpart(M6_DSMdiagnosis_PCL ~ m3_mental_health_support + FORTH_M3_Mental_health_support,
                 cp = 0.001,
                 data = dataPCL_clean_M6)
my_tree2 <- rpart(M6_DSMdiagnosis_PCL ~ education_2 + FORTH_M3_Mental_health_support + Side_Effects_BR23.0,
                 cp = 0.0000000000001,
                 data = dataPCL_clean_M6)
my_tree3 <- rpart(M6_DSMdiagnosis_PCL ~ .,
                  cp = 0.0000001,
                  data = dataPCL_clean_M6)
prp(my_tree3,
    space=4,
    split.cex=1.2,
    nn.border.col=0)

# TODOs:  1. check if for every label of M6 NA value there is also a NA value at the M6_ptsd feature
#         2. split into features and labels AFTER removing the label's NA values
#         3. combine features with the label I want and try making the complete decision tree
#         4. maybe try PCA to reduce dimensionality

# lowest_number <- 100
# for (row in 1:nrow(dataPCL_clean_M6)) {
#   ptsd_number <- dataPCL_clean_M6[row, "M6_ptsd_PCL5"]
#   ptsd_factor <- dataPCL_clean_M6[row, "M6_DSMdiagnosis_PCL"]
#   cat("Number: ", ptsd_number)
#   cat("\n")
#   cat("Factor: ", ptsd_factor)
#   cat("\n")
#   if (ptsd_factor == 1 && ptsd_number < lowest_number){
#     lowest_number <- ptsd_number
#     cat("Current lowest number is:", lowest_number)
#     cat("\n")
#   }
# }

## RESULT! Lowestnumber with ptsd_factor 1 was 25 but there were others
## with ptsd_factor 0 and at least 29 number -> makes no sense, there is no
## threshold.
