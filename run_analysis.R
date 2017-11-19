run_analysis <- function(outputfile, mean_outputfile, UCIdatasetpath = './UCI HAR Dataset'){
    ## First, load activities key file
    activities_key_df <- read.table(paste(UCIdatasetpath, '/activity_labels.txt', sep = ''), col.names = c('ActivityID', 'Activity'))
    ## Load file containing feature names
    feature_name_df <- read.table(paste(UCIdatasetpath, '/features.txt', sep = ''), col.names = c('ColNum', 'Feature'), as.is = T)
    ## Pick out columns with means or standard deviations
    mean_or_std_inds <- grep('.*[Mm][Ee][Aa][Nn].*|.*[Ss][Tt][Dd].*', feature_name_df$Feature)
    features <-  feature_name_df$Feature[mean_or_std_inds]
    ## Set up character vector for load_features that will pick out
    ## the right columns
    X_classes <- c(rep("NULL", dim(feature_name_df)[1]))
    X_classes[mean_or_std_inds] = "numeric"

    load_features <- function(set = 'test'){
        ### This function loads one of the files in the dataset that
	### contain features. There are two, which can be chosen by
	### setting 'set' to either 'test' or 'train'.

	## Load data frame from features file
    	X_df <- read.table(paste(UCIdatasetpath, '/', set, '/X_', set, '.txt', sep = ''), colClasses = X_classes, as.is = T, col.names = feature_name_df$Feature)
	## Load subjects file
    	subject_df <- read.table(paste(UCIdatasetpath, '/', set, '/subject_', set, '.txt', sep = ''), as.is = T, col.names = c('SubjectID'))
	## Add subjects to features data frame
    	X_df$SubjectID <- subject_df$SubjectID
	## Load activities file 
    	activities_df <- read.table(paste(UCIdatasetpath, '/', set, '/y_', set, '.txt', sep = ''), as.is = T, col.names = c('ActivityID'))
	## Create column where activity numeric IDs are replaced with 
	## the activity names
    	activities_df$rowID <- 1:nrow(activities_df)	      
    	activities_df <- merge(activities_df, activities_key_df, sort = F)
    	activities_df <- activities_df[order(activities_df$rowID), ]
    	X_df$Activity <- activities_df$Activity
    	X_df
    }
    
    ## Load the test and train feature files and merge the 
    ## two dataframes together
    test_df <- load_features('test')
    train_df <- load_features('train')
    df <- rbind(test_df, train_df)

    ## Get list of subjects
    subjects <- unique(df$SubjectID)
    ## Set up data.frame for mean values. Needs to be empty to begin.
    if (exists('mean_df')) rm(mean_df)
    ## Loop through subjects and the 6 activities
    for (subject in subjects) {
        for (activity in c('STANDING', 'SITTING', 'LAYING', 'WALKING', 'WALKING_DOWNSTAIRS', 'WALKING_UPSTAIRS')) {
	    ## Pick out the columns to average
	    mean_vector <- apply(df[, 1:(dim(df)[2]-2)], 2, mean)
	    if (!exists('mean_df')) {
	        ## If mean_df hasn't been created yet, initialize it
		## with the first row
	        mean_df <- data.frame(rbind(mean_vector))
	    } else {
	        ## Calculate mean of each column (except SubjectID
		## and Activity) and add the row to mean_df
	        mean_df <- data.frame(rbind(mean_df, mean_vector))
	    }
        }
    }
    ## Add SubjectID and Activity columns to mean_df
    mean_df$SubjectID <- rep(subjects, each = 6)
    mean_df$Activity <- rep(c('STANDING', 'SITTING', 'LAYING', 'WALKING', 'WALKING_DOWNSTAIRS', 'WALKING_UPSTAIRS'), length(subjects))

    ## Write the output data frames to the specified files
    write.table(df, outputfile, row.names = F)
    write.table(mean_df, mean_outputfile, row.names = F)
}