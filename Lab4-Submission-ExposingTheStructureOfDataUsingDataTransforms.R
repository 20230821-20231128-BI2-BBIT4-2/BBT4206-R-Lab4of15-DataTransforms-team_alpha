if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


if (require("readr")) {
  require("readr")
} else {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## e1071 ----
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## factoextra ----
if (require("factoextra")) {
  require("factoextra")
} else {
  install.packages("factoextra", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## FactoMineR ----
if (require("FactoMineR")) {
  require("FactoMineR")
} else {
  install.packages("FactoMineR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

##student dataset
##Execute the following to load the downloaded Crop dataset:
student_dataset <- read_csv("data/20230412-20230719-BI1-BBIT4-1-StudentPerformanceDataset (1).csv",)
                         
  

summary(student_dataset)

student_dataset_grade <- as.numeric(unlist(student_dataset[, 4]))
hist(student_dataset_grade, main = names(student_dataset)[4])





model_of_the_transform <- preProcess(student_dataset, method = c("scale"))
print(model_of_the_transform)
student_dataset_scale_transform <- predict(model_of_the_transform,
                                          student_dataset)

# AFTER
summary(student_dataset_scale_transform)


 student_dataset_grade<- as.numeric(unlist(student_dataset[, 4]))
hist(student_dataset_grade, main = names(student_dataset_scale_transform)[4])
#hist(student_dataset_scale_transform[, 2], main = names(student_dataset_scale_transform)[2])
#hist(student_dataset_scale_transform[, 4], main = names(student_dataset_scale_transform)[4])
#hist(student_dataset_scale_transform[, 5], main = names(student_dataset_scale_transform)[5])
#hist(student_dataset_scale_transform[, 6], main = names(student_dataset_scale_transform)[6])

# Center Data Transform ----

## STEP 4. Apply a Centre Data Transform ----
# The centre data transform calculates the mean of an attribute and subtracts
# it from each value.
summary(student_dataset)
boxplot(student_dataset[, 3], main = names(student_dataset)[3])
boxplot(student_dataset[, 5], main = names(student_dataset)[5])
boxplot(student_dataset[, 6], main = names(student_dataset)[6])
boxplot(student_dataset[, 7], main = names(student_dataset)[7])
boxplot(student_dataset[, 7], main = names(student_dataset)[7])
boxplot(student_dataset[, 8], main = names(student_dataset)[8])
boxplot(student_dataset[, 9], main = names(student_dataset)[9])
boxplot(student_dataset[, 10], main = names(student_dataset)[10])
boxplot(student_dataset[, 11], main = names(student_dataset)[11])
boxplot(student_dataset[, 12], main = names(student_dataset)[12])
boxplot(student_dataset[, 13], main = names(student_dataset)[13])
boxplot(student_dataset[, 14], main = names(student_dataset)[14])

model_of_the_transform <- preProcess(student_dataset, method = c("center"))
print(model_of_the_transform)
student_dataset_center_transform <- predict(model_of_the_transform, # nolint
                                           student_dataset)



boxplot(student_dataset_center_transform[, 1],
        main = names(student_dataset_center_transform)[1])
boxplot(student_dataset_center_transform[, 2],
        main = names(student_dataset_center_transform)[2])
boxplot(student_dataset_center_transform[, 3],
        main = names(student_dataset_center_transform)[3])
boxplot(student_dataset_center_transform[, 5],
        main = names(student_dataset_center_transform)[5])
boxplot(student_dataset_center_transform[, 6],
        main = names(student_dataset_center_transform)[6])
boxplot(student_dataset_center_transform[, 7],
        main = names(student_dataset_center_transform)[7])
boxplot(student_dataset_center_transform[, 8],
        main = names(student_dataset_center_transform)[8])
boxplot(student_dataset_center_transform[, 9],
        main = names(student_dataset_center_transform)[9])
boxplot(student_dataset_center_transform[, 10],
        main = names(student_dataset_center_transform)[10])
boxplot(student_dataset_center_transform[, 11],
        main = names(student_dataset_center_transform)[11])
boxplot(student_dataset_center_transform[, 12],
        main = names(student_dataset_center_transform)[12])
boxplot(student_dataset_center_transform[, 13],
        main = names(student_dataset_center_transform)[13])
boxplot(student_dataset_center_transform[, 14],
        main = names(student_dataset_center_transform)[14])



## STEP 5. Apply a Standardize Data Transform ----
# The standardize data transform ensures that each numeric attribute has a mean
# value of 0 and a standard deviation of 1. This is done by combining the scale
# data transform and the centre data transform.


summary(student_dataset)
sapply(student_dataset[, 4], sd)
model_of_the_transform <- preProcess(student_dataset,
                                     method = c("scale", "center"))
print(model_of_the_transform)
student_dataset_standardize_transform <- predict(model_of_the_transform, student_dataset) # nolint

# AFTER
summary(student_dataset_standardize_transform)
sapply(student_dataset_standardize_transform[, 4], sd)




 ###Normalize Data Transform ----

## STEP 6. Apply a Normalize Data Transform ----
summary(student_dataset)
model_of_the_transform <- preProcess(student_dataset, method = c("range"))
print(model_of_the_transform)
student_dataset_normalize_transform <- predict(model_of_the_transform, # nolint
                                              student_dataset)
summary(student_dataset_normalize_transform)


# Box-Cox Power Transform ----

## STEP 7. Apply a Box-Cox Power Transform ----


# BEFORE
summary(student_dataset_standardize_transform)
# Calculate the skewness before the Box-Cox transform
sapply(student_dataset_standardize_transform[, 4],  skewness, type = 2)
sapply(student_dataset_standardize_transform[, 4], sd)

model_of_the_transform <- preProcess(student_dataset_standardize_transform,
                                     method = c("BoxCox"))
print(model_of_the_transform)
student_dataset_box_cox_transform <- predict(model_of_the_transform,
                                       student_dataset_standardize_transform)

# AFTER
summary(student_dataset_box_cox_transform)

sapply(student_dataset_box_cox_transform[, 4],  skewness, type = 2)
sapply(student_dataset_box_cox_transform[, 4], sd)

# Calculate the skewness after the Box-Cox transform
sapply(student_dataset_box_cox_transform[, 4],  skewness, type = 2)
sapply(student_dataset_box_cox_transform[, 4], sd)



# Calculate the skewness before the Yeo-Johnson transform
sapply(student_dataset[, -1],  skewness, type = 2)

# Calculate the skewness before the Yeo-Johnson transform
sapply(student_dataset_standardize_transform[, 4],  skewness, type = 2)
sapply(student_dataset_standardize_transform[, 4], sd)

model_of_the_transform <- preProcess(student_dataset_scale_transform,
                                     method = c("YeoJohnson"))
print(model_of_the_transform)
student_dataset_yeo_johnson_transform <- predict(model_of_the_transform, # nolint
                                           student_dataset_standardize_transform)

# AFTER
summary(student_dataset_yeo_johnson_transform)

# Calculate the skewness after the Yeo-Johnson transform
sapply(student_dataset_yeo_johnson_transform[, 4],  skewness, type = 2)
sapply(student_dataset_yeo_johnson_transform[, 4], sd)

## STEP 10. ICA Linear Algebra Transform for Dimensionality Reduction ----
if (!is.element("fastICA", installed.packages()[, 1])) {
  install.packages("fastICA", dependencies = TRUE)
}
require("fastICA")

summary(student_dataset)

model_of_the_transform <- preProcess(student_dataset,
                                     method = c("scale", "center", "ica"),
                                     n.comp = 8)
print(model_of_the_transform)
student_dataset_ica_dr <- predict(model_of_the_transform, student_dataset)

summary(student_dataset_ica_dr)
