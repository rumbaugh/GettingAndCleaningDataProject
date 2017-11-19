run_analysis <- function(UCIdatasetpath = './UCI HAR Dataset'){
    activities_key_df <- read.table(paste(UCIdatasetpath, '/activity_labels.txt', sep = ''), col.names = c('ActivityID', 'Activity'))

    load_features <- function(set = 'test'){
        feature_name_df <- read.table(paste(UCIdatasetpath, '/features.txt', sep = ''), col.names = c('ColNum', 'Feature'), as.is = T)
    	mean_or_std_inds <- grep('.*[Mm][Ee][Aa][Nn].*|.*[Ss][Tt][Dd].*', feature_name_df$Feature)
    	features <-  feature_name_df$Feature[mean_or_std_inds]
    	X_classes <- c(rep("NULL", dim(feature_name_df)[1]))
    	X_classes[mean_or_std_inds] = "numeric"
    	X_df <- read.table(paste(UCIdatasetpath, '/', set, '/X_', set, '.txt', sep = ''), colClasses = X_classes, as.is = T, col.names = feature_name_df$Feature)
    	subject_df <- read.table(paste(UCIdatasetpath, '/', set, '/subject_', set, '.txt', sep = ''), as.is = T, col.names = c('SubjectID'))
    	X_df$SubjectID <- subject_df$SubjectID
    	activities_df <- read.table(paste(UCIdatasetpath, '/', set, '/y_', set, '.txt', sep = ''), as.is = T, col.names = c('ActivityID'))
    	activities_df$rowID <- 1:nrow(activities_df)	      
    	activities_df <- merge(activities_df, activities_key_df, sort = F)
    	activities_df <- activities_df[order(activities_df$rowID), ]
    	X_df$Activity <- activities_df$Activity
    	X_df
    }
    
    test_df <- load_features('test')
    train_df <- load_features('train')
    df <- rbind(test_df, train_df)
}

write_analysis <- function(outputfile, UCIdatasetpath = './UCI HAR Dataset'){
    outdf <- run_analysis(UCIdatasetpath = UCIdatasetpath)
    write.table(outdf, outputfile, row.names = F)
}