install.packages("dplyr")
library(dplyr)
file <- 'features.txt'
features = read.delim(file,header = FALSE,sep ="")
file <- 'activity_labels.txt'
activity = read.delim(file,header = FALSE,sep ="")
file <- 'train\\X_train.txt'
training_set = read.delim(file,header = FALSE,sep ="")
file <- 'train\\y_train.txt'
training_label = read.delim(file,header = FALSE,sep ="")
file <- 'train\\subject_train.txt'
training_subject = read.delim(file,header = FALSE,sep ="")
file <- 'test\\X_test.txt'
testing_set = read.delim(file,header = FALSE,sep ="")
file <- 'test\\y_test.txt'
testing_label = read.delim(file,header = FALSE,sep ="")
file <- 'test\\subject_test.txt'
testing_subject = read.delim(file,header = FALSE,sep ="")
total_set = rbind(training_set,testing_set)
total_label = rbind(training_label,testing_label)
total_subject = rbind(training_subject,testing_subject)
mean_i = grep('mean()',features[,2],ignore.case = FALSE,fixed= TRUE)
std_i = grep('std()',features[,2],ignore.case = FALSE,fixed = TRUE)
total_i = sort(c(mean_i,std_i))
features_list = features[total_i,2]
total_set = total_set[,total_i]
colnames(total_set) = features_list
id = 1:nrow(total_set)
for(i in 1:dim(activity)[1]){
  for(j in 1:dim(total_label)[1]){
    if(total_label[j,1] %in% activity[i,1] == TRUE){
    id[j] = paste(total_subject[j,1],"-",activity[i,2])
    }
  }
}
total_set$id = id
tidy_set = total_set %>%
  group_by(id) %>% 
  summarise(
    across(everything(), mean)
  )
row_names = tidy_set$id
tidy_set$id = NULL
rownames(tidy_set) = row_names
total_set = total_set[,c(ncol(total_set),1:(ncol(total_set)-1))]
write.table(tidy_set,"tidy_data.txt",row.names = FALSE)
