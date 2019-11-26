#author:yaxin yu
#pre-process the data

#1.1 Extract Data
ILPD <- read.table("./Data/Indian Liver Patient Dataset (ILPD).csv", sep=",")

#1.2 Assign name to variables
names(ILPD) <- c("Age", "Gender", "TB", "DB", "Alkphos", "Sgpt", "Sgot", "TP", "Albumin", "AG_Ratio", "Class")

#1.3 Fill the missing values with the median of the column
ILPD[is.na(ILPD$AG_Ratio),"AG_Ratio"] <- median(ILPD$AG_Ratio,na.rm = T)

#1.4 Replace all “2” in the “class” column with “0” to indicate “non_patient”
ILPD$Class[which(ILPD$Class==2)] <- 0

#1.5 change its type from integer to factor
ILPD$Class <- as.factor(ILPD$Class)

#1.6 Save the dataframe into a file with filename ilpd_processed.Rda. 
saveRDS(ILPD, file="./Data/ilpd_preprocessed.Rda")

