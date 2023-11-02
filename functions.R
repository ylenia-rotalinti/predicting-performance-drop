##FUNCTIONS

####
# PREPROCESSING
####

undersampling <- function(dataset, class_column_label) {
  #index used for ordering (will be removed)
  dataset$index <- 1:nrow(dataset)
  
  # Specify the target class you want to undersample
  target_class <- ifelse(sum(dataset[[class_column_label]]==0) > sum(dataset[[class_column_label]]==1), 0, 1)
  
  # Find the indices of samples in the target/non target class
  target_indices <- which(dataset[[class_column_label]] == target_class)
  non_target_indices <- which(dataset[[class_column_label]] != target_class)
  
  # Calculate the number of samples in the majority/minority class
  num_majority_samples <- sum(dataset[[class_column_label]] == target_class)
  num_minority_samples <- sum(dataset[[class_column_label]] != target_class)
  
  # Calculate the number of samples to keep in the majority class
  num_samples_to_keep <- num_minority_samples
  
  # Randomly select the same number of samples from the majority class
  selected_indices <- sample(target_indices, size = num_samples_to_keep, replace = FALSE)
  
  # Combine the selected indices with the minority class indices
  undersampled_indices <- c(selected_indices, non_target_indices)
  
  # Sort the shuffled data by row indices to restore the original order
  undersampled_data <- dataset[undersampled_indices, ]
  undersampled_data_ordered <- undersampled_data[order(undersampled_data$index), ]
  
  return(subset(undersampled_data_ordered, select = -index))
}


#########
# READ DATA
#########

parse_COVID_data <- function(origin) {
  switch (origin,
          "SYN" = {
            path <-
              "Data/2021-04-14_Synthetic_Of_2021-04-14_GTNegAndPositives.csv"
          },
          "REAL" = {
            path <- "Z:/PioneerCOVIDReal/REAL_COVID_CLEANED.csv"
          },
          {
            stop("ORIGIN NOT FOUND!")
          })
  
  #data reader
  df <- read.csv(path, header = TRUE)
  df <-
    subset(df,
           df$Gender != "U" & df$Gender != "I" &
             df$imd_5 != -1 & df$rurban != -1)
  
  #date column
  df$covid_dt <-
    as.Date(df$covid_dt,
            format = ifelse(origin == "REAL", "%d/%m/%Y", "%Y-%m-%d"))
  
  #select meaningful columns
  switch(origin,
         "SYN" = {
           df <- df %>%
             select(-X,-patid,-age,-DeathDate,-Tamiflu_rx) %>%
             filter(covid_dt <= as.Date('2021-04-30'))
           #filter(covid_dt >= as.Date('2020-03-01') & covid_dt <= as.Date('2021-04-30'))
           
           #list unbalanced variables
           unbalanced_var <-
             c("SAMA_rx", "Chloro_Hydroxychloro_rx", "isPositive")
         },
         "REAL" = {
           df <- df %>%
             select(-ARB_rx,
                    -ICSSABA_rx,
                    -AminoTheophy_rx,
                    -SAMA_rx,
                    -Tamiflu_rx) %>%
             filter(covid_dt <= as.Date('2021-10-30'))
           
           #list unbalanced variables
           unbalanced_var <-
             c("AminoTheophy_rx", "LearningDisability")
         },
         {
           stop("ORIGIN NOT FOUND!")
         })
  
  factors_cols <- colnames(select(df, -covid_dt))
  df <- df %>% mutate_at(factors_cols, factor) %>% arrange(covid_dt)
  
  return(df)
}
parse_CVD_data <- function() {
  df <- read.csv("Data/rsample.csv",
                 header = TRUE)
  #check datatypes
  #set the class as 0/1 factor
  df$death <- ifelse(df$death == "yes", 1, 0)
  df$treated.hypertension <-
    ifelse(df$treated.hypertension == "yes", 1, 0)
  
  ##1) factors
  factors_cols <-
    colnames(select(
      df,
      -age,
      -BMI,
      -SBP,
      -DBP,
      -cholesterol.ratio,
      -Diagnosis.Date
    ))
  df <- df %>% mutate_at(factors_cols, factor)
  
  ##2) dates format
  df$Diagnosis.Date <- as.Date(df$Diagnosis.Date, "%d/%m/%Y")
  
  ##3) continuous data
  continuous_cols <-
    colnames(select(df, age, BMI, SBP, DBP, cholesterol.ratio))
  df <- df %>% mutate_at(continuous_cols, as.numeric)
  
  #arrange by the date
  df <- df %>%
    arrange(Diagnosis.Date)
  
  #DISCRETIZATION
  #df[,continuous_cols] <- discretize(df[, continuous_cols], breaks = c(20,20,20,10,12), method='interval')
  
  return(df)
}
parse_synthetic_data <- function(dataset_label) {
  switch (
    dataset_label,
    "Abrupt" = {
      filepath <- "Data/abrupt_dataset_orig.arff"
    },
    "Gradual_W10K" = {
      filepath <- "Data/gradualW10K_dataset_orig.arff"
    },
    "Gradual_W20K" = {
      filepath <- "Data/gradualW20K_dataset_orig.arff"
    },
    "Gradual3DRIFTS_W5K" = {
      filepath <- "Data/gradualW5K_3drifts_dataset.arff"
    },
  )
  
  df <- read.csv(filepath, header = FALSE, comment.char = "@")
  #delete NAN column
  df$V11 <- NULL
  #rename columns
  names(df) <-
    c(
      "salary",
      "commission",
      "age",
      "elevel",
      "car",
      "zipcode",
      "hvalue",
      "hyears",
      "loan",
      "class"
    )
  
  #refactor datatype
  df$class <- ifelse(df$class == "groupB", 1, 0)
  factors_col <- c("elevel", "car", "zipcode", "class")
  df[, factors_col] <- lapply(df[, factors_col], factor)
  return(df)
}

parse_data <- function(dataset_label){
  switch (dataset_label,
          "COVID-SYN" = {df <- parse_COVID_data("SYN")},
          "COVID-REAL" = {df <- parse_COVID_data("REAL")},
          "CVD" = {df <- parse_CVD_data()},
          {
            df<-parse_synthetic_data(dataset_label) #abrupt or gradual
          }
  )
  return(df)
}
set_parameters<-function(dataset_label){
  if(dataset_label %in% c("COVID-SYN", "COVID-REAL")){
      granularity <- "month"          #batch window (day/month/year)
      date_format <- "%Y/%m"           #format of the time column (%Y , %Y-%m)
      date_column_label <- "covid_dt"   #meaningful columns
      class_column_label <-"Death"     #c("Death", "HeartFailure")
      is_unbalanced <- TRUE
      batches_to_group <-5          #how many batches to group (in the first placee)
      is_time_series <- TRUE         #is a evaluation dataset i.e. simulated
      model_trained <- "random_forest"   #c("random_forest", "logistic_regression")
      performance_metric <- "accuracy"   #c("accuracy", "recall")
  } else if(dataset_label=="CVD"){
      granularity <- "year"
      date_format <- "%Y"
      date_column_label <- "Diagnosis.Date"
      class_column_label <- "death"
      is_unbalanced <- TRUE
      batches_to_group <-2
      is_time_series <- TRUE
      model_trained <- "random_forest"
      performance_metric <- "accuracy"
  } else{
      granularity <- 5000
      date_format <- NaN
      date_column_label <- NaN
      class_column_label <- "class"
      is_unbalanced <- FALSE
      batches_to_group <-1
      is_time_series <- FALSE
      model_trained <- "random_forest"
      performance_metric <- "accuracy"
  }
  return(list(name=dataset_label,
              granularity=granularity, 
              date_format=date_format, 
              date_column_label=date_column_label, 
              class_column_label=class_column_label, 
              is_unbalanced=is_unbalanced,
              batches_to_group=batches_to_group,
              is_time_series=is_time_series,
              model_trained=model_trained,
              performance_metric=performance_metric)
  )
}

##########
# SPLIT DATASET INTO BATCHES
##########

data_to_batches <-function(dataset_obj){
  raw_batches<- get_raw_batches(dataset_obj)
  batches<-list(get_first_batch(raw_batches, dataset_obj))
  for(i in (dataset_obj$batches_to_group+1):length(raw_batches)){
    batches<-append(batches, list(list(batch_id=length(batches)+1, 
                                       batch_name=names(raw_batches)[i],
                                       date_start=ifelse(dataset_obj$is_time_series, get_first_day(names(raw_batches)[i], dataset_obj$granularity), ((i-1)*dataset_obj$granularity)+1), 
                                       date_end=ifelse(dataset_obj$is_time_series, get_last_day(names(raw_batches)[i], dataset_obj$granularity), i*dataset_obj$granularity), 
                                       data=raw_batches[[i]])))
  }
  names(batches)<-names(raw_batches)[dataset_obj$batches_to_group:length(raw_batches)]
  return(batches)
  
}
get_raw_batches<-function(dataset_obj){
  #parameters
  data<-parse_data(dataset_obj$name)
  granularity<-dataset_obj$granularity
  date_column_label<-dataset_obj$date_column_label
  date_format<-dataset_obj$date_format
  
  #if the data is unbalanced, undersample without replacement the majority class
  if(dataset_obj$is_unbalanced){
    data<-undersampling(data, dataset_obj$class_column_label)  
  }
  
  #exclude the date column 
  if(dataset_obj$is_time_series){
    return(split(
      data[,!(names(data) %in% c(date_column_label))],
      format(data[date_column_label], date_format)
    ))
  }
  raw_batches <- split(data, (seq(nrow(data))-1) %/% granularity)
  names(raw_batches)<-seq(granularity, nrow(data), by=granularity)/1000
  return(raw_batches)
}
get_first_batch<-function(raw_batches, dataset_obj){
  first_batch<-data.frame()
  for(i in 1:dataset_obj$batches_to_group){
    first_batch<-rbind(first_batch, raw_batches[[i]])
  }
  return(list(batch_id=1, 
              batch_name=names(raw_batches)[dataset_obj$batches_to_group],
              date_start=ifelse(dataset_obj$is_time_series, 
                                get_first_day(names(raw_batches)[1], dataset_obj$date_format), 
                                1 
                               ), 
              date_end=ifelse(dataset_obj$is_time_series, 
                              get_last_day(names(raw_batches)[dataset_obj$batches_to_group], dataset_obj$date_format),
                              dataset_obj$granularity*dataset_obj$batches_to_group
                             ), 
              data=first_batch))
}
get_first_day<-function(date_as_string, granularity){
  flag_year<-ifelse(granularity=="year", TRUE, FALSE)
  new_date<-as.Date(paste0(date_as_string, ifelse(flag_year,"/01/01","/01")))
  return(format(new_date, ifelse(flag_year,"%Y/%m","%Y/%m/%d")))
}
get_last_day<-function(date_as_string, granularity){
  flag_year<-ifelse(granularity=="year", TRUE, FALSE)
  first_day<-as.Date(paste0(date_as_string, ifelse(flag_year,"/01/01","/01")))
  new_date<-ceiling_date(first_day, "month")-1
  return(format(new_date, ifelse(flag_year,"%Y/%m", "%Y/%m/%d")))
}
get_batch_name<-function(raw_batches,i){
  return(names(raw_batches)[i])
}


##########
# SPLIT BATCHES IN SOURCE and TARGET (TRAINING,TEST)
##########

# Perform the 70% - 30% split 
# Also, ensuring that there are no overlapping rows between the training and test data sets and duplicate rows are considered
split_batch<-function(batch_data){
  # Set the seed for reproducibility (optional)
  # set.seed(123)
  
  # Shuffle the rows in batch_data
  shuffled_data <- batch_data[sample(nrow(batch_data)), ]
  
  # Determine the split point
  split_point <- round(0.7 * nrow(shuffled_data))
  
  # Split the data into training and test sets
  training <- shuffled_data[1:split_point, ]
  test <- shuffled_data[(split_point + 1):nrow(shuffled_data), ]
  
  return(list(training=training,
              test=test))
}
split_batches<-function(dataset_obj){
  source<-dataset_obj$batches[[1]]$data
  training<-list()
  test<-list()
  
  for(i in 2:length(dataset_obj$batches)){
    split <- split_batch(dataset_obj$batches[[i]]$data)
    training<-c(training, list(split$training))
    test<-c(test, list(split$test))
  }
  
  names(training)<-names(dataset_obj$batches)[-1]
  names(test)<-names(dataset_obj$batches)[-1]
  
  return(list(source=source,
              training=training, 
              test=test))
}
append_splits_to_dataset_obj <-function(dataset_obj){
  return(append(dataset_obj, 
                split_batches(dataset_obj)))
}
initialise_dataset<-function(dataset_label){
  dataset_obj<-set_parameters(dataset_label)
  dataset_obj$batches<-data_to_batches(dataset_obj)
  dataset_obj<-append_splits_to_dataset_obj(dataset_obj)
  return(dataset_obj)
}


##########
# TRAIN MODEL AND EVALUATE PERFORMANCE
##########
train_model<-function(training_data, class, model){
  fitted_model <- switch (model,
    "random_forest" = {fitted_model <- randomForest(as.formula(paste(class, "~.")), data = training_data)},
    "logistic_regression" = {fitted_model <-  glm(as.formula(paste(class, "~.")), data = training_data, family = binomial(link = "logit"))},
    {
      stop("MODEL NOT IMPLEMENTED!")
    }
  )
  return(fitted_model)
}
compute_model_performance <- function(fitted_model, new_data, class, performance_metric){
  predicted_class<- predict(fitted_model, new_data)
  if("glm" %in% class(fitted_model)){
    predicted_class<-ifelse(predicted_class >= 0.5, 1, 0)
  }
  predicted_class<-as.factor(predicted_class)
  true_class<-new_data[[class]] 
  
  switch (performance_metric,
    "accuracy" = {
      return(confusionMatrix(data=predicted_class, 
                             reference=true_class,
                             positive="1")$overall["Accuracy"])
    },
    "recall" = {
      return(confusionMatrix(data=predicted_class, 
                             reference=true_class,
                             positive="1")$byClass["Recall"])
    },
    {
      stop("PERFORMANCE METRIC NOT FOUND!")
    }
  )
}
#@data_group: "test", "training"
compute_performance_shifts<-function(dataset_obj, data_group){
  source<-dataset_obj$source
  model<-train_model(source, dataset_obj$class_column_label, dataset_obj$model_trained)
  performance<-c()
  for(i in 1:length(dataset_obj[[data_group]])){
    performance<-c(performance, 
                   compute_model_performance(model,
                                             dataset_obj[[data_group]][[i]],
                                             dataset_obj$class_column_label,
                                             dataset_obj$performance_metric))
  }
  performance_shift<-c(0, diff(performance))
  return(data.frame(batch=names(dataset_obj[[data_group]]),
                    performance=performance,
                    performance_shift=performance_shift))
}


##########
# MEASURING DOMAIN-SHIFT METRICS
##########

## 1.DISCRIMINATION ERROR
compute_discrimination_error<-function(source, target, class_column_label){
  source[[class_column_label]]<-as.factor(1)
  target[[class_column_label]]<-as.factor(0)
  
  #data sets balancing (both with the same amount of data)
  # if(nrow(source) > nrow(target)){
  #   source<-sample_n(source, nrow(target))
  # } else {
  #   target<-sample_n(target, nrow(source))
  # }
  
   #to balance the data
  union<-union_all(source,target) 
  
  # Set the seed for reproducibility (optional)
  # set.seed(123)
  
  # Shuffle the rows 
  shuffled_data <- union[sample(nrow(union)), ]
  
  # Determine the split point
  split_point <- round(0.7 * nrow(shuffled_data))
  
  # Split the data into training and test sets
  training <- shuffled_data[1:split_point, ]
  test <- shuffled_data[(split_point + 1):nrow(shuffled_data), ]
  
  model<-train_model(training, class_column_label, "random_forest")
  error <- 1 - compute_model_performance(model, test, class_column_label, "accuracy")
  
  return(error)
}
#@data_group: "training", "test"
compute_discrimination_errors<-function(dataset_obj, data_group){
  discrimination_errors<-c()
  source<-dataset_obj$source
  for (i in 1:length(dataset_obj[[data_group]])){
    discrimination_errors<-c(discrimination_errors, 
            compute_discrimination_error(source, 
                                         dataset_obj[[data_group]][[i]], 
                                         dataset_obj$class_column_label))
  }
  return(discrimination_errors)
}

## 2. CONFIDENCE-BASED DISTANCE
compute_score <-function(probability){
  return(abs(2*probability-1))
}
compute_predictions<-function(fitted_model, new_data){
  return(predict(fitted_model, 
          new_data, 
          type = "prob")[, 2])
}
compute_avr_score<-function(fitted_model, batch_data){
  predictions<-compute_predictions(fitted_model, batch_data)
  score<-compute_score(predictions)
  return(mean(score))
}
compute_avr_scores<-function(dataset_obj, data_group){
  source<-dataset_obj$source
  model<-train_model(source, dataset_obj$class_column_label, "random_forest")
  scores<-c()
  for(i in 1:length(dataset_obj[[data_group]])){
    scores<-c(scores, compute_avr_score(model,
                                        dataset_obj[[data_group]][[i]]))
  }
  return(scores)
  
}

compute_drift_detection_metrics<-function(dataset_obj, data_group){
  discrimination_error_values <-compute_discrimination_errors(dataset_obj, data_group)
  avr_prob_score_values <-compute_avr_scores(dataset_obj, data_group)
  return(data.frame(discrimination_error=discrimination_error_values, avr_prob_score=avr_prob_score_values))
}
append_drift_detection_metrics<-function(dataset_obj, data_group, training_or_test_obj){
  metrics_values<-compute_drift_detection_metrics(dataset_obj, data_group)
  return(cbind(training_or_test_obj, 
               metrics_values))
}
get_metric_name<-function(metric){
  if(metric=="discrimination_error"){return("discrimination error")}
    else if(metric=="avr_prob_score"){return("average score")}
}

## 3. MEAN PERFORMANCE SHIFT
compute_mean_performance_shift<-function(training_obj){
  return(mean(training_obj$performance_shift))
}

##########
# PLOT REGRESSION MODEL
##########
plot_regression_model<-function(training_obj, metric){
  my_plot<- 
    ggplot(training_obj, aes(x=.data[[metric]], y=performance_shift)) + 
    theme(axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=10))+
    theme_bw()+
    stat_smooth(method = "lm", 
                formula = y ~ x,
                #col = "red"
                )+
    geom_point(color="black")+ #"#69b3a2")+
    labs(#title = ~ bold("Regression model"),
         #subtitle = paste("Metric:", get_metric_name(metric)), 
         x=get_metric_name(metric), 
         y="Accuracy shift")
  print(my_plot)
  ggsave(paste0(metric,"_regressionModel.png"), 
         plot = my_plot, width = 6, height = 4, dpi = 300)
}

##########
# TRAIN REGRESSION MODEL AND PREDICT PERFORMANCE SHIFT 
##########

train_linear_regression<-function(training_obj, metric){
  switch (metric,
    "discrimination_error" = return(lm(performance_shift ~ discrimination_error, data = training_obj)),
    "avr_prob_score" = return(lm(performance_shift ~ avr_prob_score, data = training_obj))
  )
}

predict_performance_shift<-function(training_obj, test_obj, metric){
  model <- train_linear_regression(training_obj, metric)
  
  new_data<-data.frame(test_obj[[metric]])
  names(new_data)<-metric

  perf_shift_pred<-data.frame(predict(model, new_data))
  names(perf_shift_pred)<-paste0("perf_shift_pred_", metric)
  return(perf_shift_pred)
}

append_performance_shifts_pred<-function(training_obj, test_obj, metrics){
  performance_shifts_pred<-test_obj
  for(metric in metrics){
    performance_shifts_pred<-cbind(performance_shifts_pred, 
                          predict_performance_shift(training_obj, test_obj, metric))
  }
  return(performance_shifts_pred)
}

##########
# PLOT (TRAINING/TEST) METRICS or PREDICTION ERRORS
##########

#@option: "metrics", "drop_prediction"
plot_drift_detection <-function(training_or_test_obj, metrics, option){
  plot_list<-list()
  switch(option,
         metrics = {plot_obj <- c("performance", "performance_shift", metrics)
                    plot_names <- c("performance", "performance shift", "discrimination error", "avr score")
                    colors <- c("dodgerblue","dodgerblue","firebrick", "forestgreen")},
         drop_prediction ={plot_obj <- c("performance_shift", paste0("perf_shift_pred_", metrics))
                  plot_names<- c("true shift", "DE pred", "APS pred")
                  colors <- c("blue3", "firebrick", "forestgreen")}
  ) 
  for(i in 1:length(plot_obj)){
    plot<- ggplot(training_or_test_obj, 
                  aes(x = batch, y = .data[[plot_obj[i]]], group=1)) + 
      geom_line(colour=colors[i], size=0.6)+
      geom_point(colour=colors[i], size=1.8)+
      theme_bw()+
      ylab(plot_names[i])
    # Conditionally rotate x-axis labels 
    if(Dataset$is_time_series){
      plot <- plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
    plot_list<-append(plot_list, list(plot))
  }
  combined_plot <- grid.arrange(grobs = plot_list, gtable_show = FALSE)
  
  # Save the plot as an image
  ggsave(paste0(option, "_test.png"), 
         plot = combined_plot, width = 6, height = 4, dpi = 300)
  
  # Display the plot in the R session
  print(combined_plot)
} 



##########
# COMPUTE ERROR AND PRINT ALGORITHM OUTPUT
##########

compute_error<-function(training_or_test_obj, metric){
  switch (metric,
    "baseline"={perf_shift_pred=training_or_test_obj$perf_shift_pred_baseline},
    "discrimination_error" = {perf_shift_pred=training_or_test_obj$perf_shift_pred_discrimination_error},
    "avr_prob_score"= {perf_shift_pred=training_or_test_obj$perf_shift_pred_avr_prob_score}
  )
  return(abs(training_or_test_obj$performance_shift-perf_shift_pred))
}
compute_errors<-function(training_or_test_obj, metrics){
  errors<-data.frame(compute_error(training_or_test_obj, "baseline"))
  for(metric in metrics){
    errors<-cbind(errors, 
                  compute_error(training_or_test_obj, metric))
  }
  names(errors)<-paste0("error_", c("baseline", metrics))
  return(errors)
}
compute_statistics<-function(error){
  return(c(mean(error),
           sd(error),
           max(error)))
}
print_output<-function(error){
  return(setNames(
  data.frame(
    t(data.frame(compute_statistics(error$error_baseline),
                 compute_statistics(error$error_discrimination_error),
                 compute_statistics(error$error_avr_prob_score)))
    ,row.names = c("baseline", "discrimination_error", "avr_prob_score"),stringsAsFactors = FALSE
  ), 
  c("MAE","sd(MAE)","Max")
))
}



