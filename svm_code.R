####### SVM #########
## Samuel Witham
#### Part 1: Data Cleaning

##Install packages
library(tidyverse)
library(readxl)

##Load Dataset
Project_Data <- read_excel("Project Data.xlsx")

##Working dataset
pitch_data <- Project_Data

##Removing null and pitchout values from pitch type
pitch_data <- pitch_data[!(pitch_data$pitch_type == "PO"),]
pitch_data <- pitch_data[!(pitch_data$pitch_type == "null"),]
pitch_data <- pitch_data[!is.na(pitch_data$pitch_type),]
unique(pitch_data$pitch_type)

##Classify pitches as Fastball(1) or Off-Speed (-1)
pitch_data$speed_factor <- 
              ifelse(pitch_data$pitch_type == "CH", -1,
              ifelse(pitch_data$pitch_type == "CU", -1,
              ifelse(pitch_data$pitch_type == "FC", 1,
              ifelse(pitch_data$pitch_type == "FF", 1,
              ifelse(pitch_data$pitch_type == "FT", 1,
              ifelse(pitch_data$pitch_type == "SI", 1,
              ifelse(pitch_data$pitch_type == "SL", -1,"")))))))


summary(pitch_data$speed_factor)

##Convert speed factor to factor variable type
pitch_data$speed_factor <- as.factor(pitch_data$speed_factor)

##svm dataset
svm_df <- pitch_data[,c("release_pos_x","release_pos_z",
                        "player_name", "speed_factor")]
View(svm_df)

##DF by pitcher
syndergaard <- subset(svm_df, player_name == "Noah Syndergaard")
diaz <- subset(svm_df, player_name == "Edwin Diaz")
degrom <- subset(svm_df, player_name == "Jacob deGrom")
stroman <- subset(svm_df, player_name == "Marcus Stroman")
lugo <- subset(svm_df, player_name == "Seth Lugo")


#### Part 2: SVM

##load libraries
library(e1071)
library(caret)

#### Syndergaard ################################################
##Polynomial
syndergaard <- syndergaard[,c("release_pos_x","release_pos_z",
                              "speed_factor")]
plot(syndergaard$release_pos_x, syndergaard$release_pos_z)

svm_syndergaard_p <- svm(speed_factor~., data = syndergaard,
                       kernel="polynomial", cost=150,
                       scale=FALSE)
print(svm_syndergaard_p)
plot(svm_syndergaard_p, syndergaard)

(pred_syn <- predict(svm_syndergaard_p, syndergaard, type="class"))
(table(pred_syn, syndergaard$speed_factor))

confusionMatrix(syndergaard$speed_factor, 
                  predict(svm_syndergaard_p))

##Radial
svm_syndergaard_r <- svm(speed_factor~., data = syndergaard,
                         kernel="radial", cost=150,
                         scale=FALSE)
print(svm_syndergaard_r)
plot(svm_syndergaard_r, syndergaard)

(pred_syn <- predict(svm_syndergaard_r, syndergaard, type="class"))
(table(pred_syn, syndergaard$speed_factor))

confusionMatrix(syndergaard$speed_factor, 
                predict(svm_syndergaard_r))

##Linear
svm_syndergaard_l <- svm(speed_factor~., data = syndergaard,
                         kernel="linear", cost=100,
                         scale=FALSE)
print(svm_syndergaard_l)
plot(svm_syndergaard_l, syndergaard)

(pred_syn <- predict(svm_syndergaard_l, syndergaard, type="class"))
(table(pred_syn, syndergaard$speed_factor))

confusionMatrix(syndergaard$speed_factor, 
                predict(svm_syndergaard_l))


#### Lugo ################################################
##Polynomial
lugo <- lugo[,c("release_pos_x","release_pos_z",
                "speed_factor")]
plot(lugo$release_pos_x, lugo$release_pos_z)

svm_lugo_p <- svm(speed_factor~., data = lugo,
                  kernel="polynomial", cost=100,
                  scale=FALSE)
print(svm_lugo_p)
plot(svm_lugo_p, lugo)

(pred_syn <- predict(svm_lugo_p, lugo, type="class"))
(table(pred_syn, lugo$speed_factor))

confusionMatrix(lugo$speed_factor, 
                predict(svm_lugo_p))

##Radial
svm_lugo_r <- svm(speed_factor~., data = lugo,
                  kernel="radial", cost=100,
                  scale=FALSE)
print(svm_lugo_r)
plot(svm_lugo_r, lugo)

(pred_syn <- predict(svm_lugo_r, lugo, type="class"))
(table(pred_syn, lugo$speed_factor))

confusionMatrix(lugo$speed_factor, 
                predict(svm_lugo_r))

##Linear
svm_lugo_l <- svm(speed_factor~., data = lugo,
                  kernel="linear", cost=100,
                  scale=FALSE)
print(svm_lugo_l)
plot(svm_lugo_l, lugo)

(pred_syn <- predict(svm_lugo_l, lugo, type="class"))
(table(pred_syn, lugo$speed_factor))

confusionMatrix(lugo$speed_factor, 
                predict(svm_lugo_l))

#### Degrom ################################################
##Polynomial
degrom <- degrom[,c("release_pos_x","release_pos_z",
                    "speed_factor")]
plot(degrom$release_pos_x, degrom$release_pos_z)

svm_degrom_p <- svm(speed_factor~., data = degrom,
                    kernel="polynomial", cost=100,
                    scale=FALSE)
print(svm_degrom_p)
plot(svm_degrom_p, degrom)

(pred_syn <- predict(svm_degrom_p, degrom, type="class"))
(table(pred_syn, degrom$speed_factor))

confusionMatrix(degrom$speed_factor, 
                predict(svm_degrom_p))

##Radial
svm_degrom_r <- svm(speed_factor~., data = degrom,
                    kernel="radial", cost=100,
                    scale=FALSE)
print(svm_degrom_r)
plot(svm_degrom_r, degrom)

(pred_syn <- predict(svm_degrom_r, degrom, type="class"))
(table(pred_syn, degrom$speed_factor))

confusionMatrix(degrom$speed_factor, 
                predict(svm_degrom_r))

##Linear
svm_degrom_l <- svm(speed_factor~., data = degrom,
                    kernel="linear", cost=100,
                    scale=FALSE)
print(svm_degrom_l)
plot(svm_degrom_l, degrom)

(pred_syn <- predict(svm_degrom_l, degrom, type="class"))
(table(pred_syn, degrom$speed_factor))

confusionMatrix(degrom$speed_factor, 
                predict(svm_degrom_l))


#### Stroman ################################################
##Polynomial
stroman <- stroman[,c("release_pos_x","release_pos_z",
                      "speed_factor")]
plot(stroman$release_pos_x, stroman$release_pos_z)

svm_stroman_p <- svm(speed_factor~., data = stroman,
                     kernel="polynomial", cost=50,
                     scale=FALSE)
print(svm_stroman_p)
plot(svm_stroman_p, stroman)

(pred_syn <- predict(svm_stroman_p, stroman, type="class"))
(table(pred_syn, stroman$speed_factor))

confusionMatrix(stroman$speed_factor, 
                predict(svm_stroman_p))

##Radial
svm_stroman_r <- svm(speed_factor~., data = stroman,
                     kernel="radial", cost=100,
                     scale=FALSE)
print(svm_stroman_r)
plot(svm_stroman_r, stroman)

(pred_syn <- predict(svm_stroman_r, stroman, type="class"))
(table(pred_syn, stroman$speed_factor))

confusionMatrix(stroman$speed_factor, 
                predict(svm_stroman_r))

##Linear
svm_stroman_l <- svm(speed_factor~., data = stroman,
                     kernel="linear", cost=100,
                     scale=FALSE)
print(svm_stroman_l)
plot(svm_stroman_l, stroman)

(pred_syn <- predict(svm_stroman_l, stroman, type="class"))
(table(pred_syn, stroman$speed_factor))

confusionMatrix(stroman$speed_factor, 
                predict(svm_stroman_l))

