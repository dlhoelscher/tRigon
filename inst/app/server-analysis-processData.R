#processing data

#meta data
metadata_df <- eventReactive(input$preprocessBtn, {
  if (is.null(input$metaData)) {
    showNotification(ui="Please select a metadata file to upload",type="error",duration=NULL, closeButton = TRUE)
    validate(
      need(!is.null(input$metaData),"Please select a metadata file to upload")
    )
  } else if (!grepl(".xlsx",input$metaData$datapath)) {
    showNotification(ui="Selected file must be a .xlsx file",type="error",duration=NULL, closeButton = TRUE)
    validate(
      need(grepl(".xlsx",input$metaData$datapath),"Selected file must be a .xlsx file")
    )
  } else {
    rvals$rval_metadatadf <- read_excel(input$metaData$datapath, 1)
    read_excel(input$metaData$datapath, 1)
  }
})

#process files to df
process_df <-  reactive({
  n_files <- nrow(input$featureData)
  rvals$rval_nfiles <- n_files
  if (input$workflowHuman) {#human workflow based on specimens
    clinical_data <- read_excel(input$metaData$datapath,1)
    if (!("pseudonym" %in% (colnames(clinical_data)))) {
      showNotification(ui="Meta data format error",type = "error",duration = NULL, closeButton = TRUE)
      validate(
        need("pseudonym" %in% (colnames(clinical_data)),"Meta data file does not contain necessary columns to process human data. Did you mean to process mouse data?")
      )
    }
    withProgress(message = "Processing", value=0, {
      incProgress(1 / 4)
      my.files <- input$featureData$datapath
      file.names <- gsub("_features.csv", "", input$featureData$name)
      read_df <- function(i){
        fp <- my.files[[i]]
        dt <- fread(fp)
        dt$filename <- file.names[[i]]
        return(dt)
      }
      incProgress(2 / 4)
      l <- lapply(seq_along(my.files), read_df)
      incProgress(3 / 4)
      combined_data <- rbindlist(l, fill=TRUE)
    })
    withProgress(message = "Merging Data", value=0, {
      incProgress(2 / 10)
      combined_data <- combined_data[ , paste0("file_V", 1:max(sapply(spl <- strsplit(combined_data$filename, "_"), length))) := transpose(spl)][]
      incProgress(3 / 10)
      clinical_data <- read_excel(input$metaData$datapath,1)
      col_clinical <- colnames(clinical_data)
      incProgress(4 / 10)
      col_clinical <- col_clinical[! col_clinical %in% c("pseudonym")]
      matches <- apply(combined_data[-1], 2, function(col) any(col %in% clinical_data$pseudonym))
      col_match <- colnames(combined_data[-1])[matches]
      incProgress(5 / 10)
      if (input$keepCalculations == FALSE) {
        #drop summary columns if keeping them is disabled
        combined_data <- dplyr::select(combined_data, -contains("MEAN"))
        combined_data <- dplyr::select(combined_data, -contains("MEDIAN"))
        combined_data <- dplyr::select(combined_data, -contains("STD"))
      }
      # replace artificial -1 values with NA
      combined_data[combined_data == -1] <- NA
      incProgress(6 / 10)
      #merge features with clinical information (biopsy type, disease, etc.)
      rvals$rval_metadatadf <- clinical_data
      feature_analysis <- merge(combined_data,clinical_data, by.x=col_match, by.y="pseudonym")
      incProgress(7 / 10)
      feature_analysis <- feature_analysis %>% rename (patient_ID = col_match)
      feature_analysis <- feature_analysis %>% select(-contains("file_V"), -filename)
      feature_analysis <- Filter(function(x)!all(is.na(x)), feature_analysis)
      feature_analysis <- feature_analysis %>% select(patient_ID, col_clinical, everything())
      rval_ID_metadata = length(unique(clinical_data$pseudonym))
      rval_ID_feature = length(unique(combined_data[[col_match]]))
      clinicalID_df <- data.frame(ID = c(unique(clinical_data$pseudonym)))
      featureID_df <- data.frame(ID = c(unique(combined_data[[col_match]])))
      clinicalID_df$match <- clinicalID_df$ID %in% featureID_df$ID
      rvals$rval_IDmatches_df <- full_join(clinicalID_df, featureID_df, by = "ID")
      rvals$rval_IDmatches_df <- rvals$rval_IDmatches_df %>% rename (patient_ID = "ID")
      rvals$rval_IDmatches_df$match[is.na(rvals$rval_IDmatches_df$match)] <- FALSE
      rvals$rval_IDmatches_df <- rvals$rval_IDmatches_df[order(rvals$rval_IDmatches_df$match, decreasing = TRUE),]
      incProgress(9 / 10)
    })
  } else { #mouse workflow based on slide position and individual tissue pieces
    clinical_data <- read_excel(input$metaData$datapath,1)
    if (!("ID" %in% (colnames(clinical_data)) && ("position" %in% (colnames(clinical_data))))) {
      showNotification(ui="Meta data format error",type = "error",duration = NULL, closeButton = TRUE)
      validate(
        need("file" %in% (colnames(clinical_data)),"Meta data file does not contain necessary columns to process mouse data. Did you mean to process human data?")
      )
    }
    withProgress(message = "Processing", value=0, {
      incProgress(1 / 4)
      my.files <- input$featureData$datapath
      file.names <- gsub("_features.csv", "", input$featureData$name)
      read_df <- function(i){
        fp <- my.files[[i]]
        dt <- fread(fp)
        dt$file <- file.names[[i]]
        return(dt)
      }
      incProgress(2 / 4)
      l <- lapply(seq_along(my.files), read_df)
      incProgress(3 / 4)
      combined_data <- rbindlist(l, fill=TRUE)
    })
    withProgress(message = "Merging Data", value=0, {
      combined_data <- combined_data[ , paste0("file_V", 1:max(sapply(spl <- strsplit(combined_data$file, "_"), length))) := transpose(spl)][]
      n_slidepos <- paste0("file_V", max(sapply(spl <- strsplit(combined_data$file, "_"), length)))
      incProgress(3 / 10)
      clinical_data <- read_excel(input$metaData$datapath,1)
      rvals$rval_metadatadf <- clinical_data
      col_clinical <- colnames(clinical_data)
      incProgress(4 / 10)
      col_clinical <- col_clinical[! col_clinical %in% c("ID")]
      col_clinical <- col_clinical[! col_clinical %in% c("position")]
      incProgress(5 / 10)
      combined_data <- combined_data %>% rename (slidepos = n_slidepos)
      combined_data <- combined_data %>% dplyr::select(file, slidepos,everything())
      #drop summary columns if keeping them is disabled
      if (input$keepCalculations == FALSE) {
        combined_data <- dplyr::select(combined_data, -contains("MEAN"))
        combined_data <- dplyr::select(combined_data, -contains("MEDIAN"))
        combined_data <- dplyr::select(combined_data, -contains("STD"))
      }
      # replace artificial -1 values with NA
      combined_data[combined_data == -1] <- NA
      incProgress(6 / 10)
      #merge features with clinical information (genotype, experiment, etc.)
      incProgress(7 / 10)
      clinical_data$file <- paste(clinical_data$ID, clinical_data$position, sep = "_")
      clinical_data <- clinical_data %>% dplyr::select(-ID, -position)
      feature_analysis <- merge(combined_data, clinical_data, by = "file")
      feature_analysis <- feature_analysis %>% select(-contains("file_V"))
      feature_analysis <- feature_analysis %>% select(file, col_clinical, everything())
      rval_ID_metadata = length(unique(clinical_data$file))
      rval_ID_feature = length(unique(combined_data$file))
      incProgress(8 / 10)
      clinicalID_df <- data.frame(file = c(unique(clinical_data$file)))
      featureID_df <- data.frame(file = c(unique(combined_data$file)))
      clinicalID_df$match <- clinicalID_df$file %in% featureID_df$file
      rvals$rval_IDmatches_df <- full_join(clinicalID_df , featureID_df, by="file")
      rvals$rval_IDmatches_df$match[is.na(rvals$rval_IDmatches_df$match)] <- FALSE
      rvals$rval_IDmatches_df <- rvals$rval_IDmatches_df[order(rvals$rval_IDmatches_df$match, decreasing = TRUE),]
      incProgress(9 / 10)
    })
  }
  rvals$rval_processeddf <- feature_analysis
  names(rvals$rval_processeddf) <- names(rvals$rval_processeddf) %>%  make.names()
  names(rvals$rval_processeddf) <- sub("%", "", names(rvals$rval_processeddf))
  rvals$rval_processeddf
})

processed_data <- eventReactive(input$preprocessBtn,{
  if (is.null(input$featureData) | is.null(input$metaData)) {
    showNotification(ui="Please select feature and metadata files to upload",type="error",duration=NULL, closeButton = TRUE)
    validate(
      need(!is.null(input$featureData),"Please select feature data files to upload"),
      need(!is.null(input$metaData),"Please select a metadata file to upload")
    )
  }
  if (TRUE %in% (!(grepl(".csv",input$featureData$datapath) | grepl(".xlsx",input$metaData$datapath)))) {
    showNotification(ui="Selected feature files must be .csv files, metadata file must be a .xlsx file",type="error",duration=NULL, closeButton = TRUE)
    validate(
      need(grepl(".csv",input$featureData$datapath),"Selected feature files must be .csv files"),
      need(grepl(".xlsx",input$metaData$datapath),"Metadata file must be a .xlsx file")
    )
  } else {
    process_df()
  }
})

# output summary table & metadata
output$SummaryTable <- DT::renderDataTable(processed_data())
output$MetadataTable <- DT::renderDataTable(metadata_df())

#find features in processed data
observeEvent(input$findfeaturesBtn_processeddata, handlerExpr = {
  df_processed <- rvals$rval_processeddf
  if (is.null(dim(df_processed))) {
    showNotification(ui = "Please process data before finding features", type = "error", duration = NULL, closeButton = TRUE)
    validate(
      need(!is.null(dim(df_processed)), "Please process data before finding features")
    )}
  rvals$rval_chosendf <- "processed"
  withProgress(message = "Finding Features", value = 0, {
    incProgress(3 / 6)
    Sys.sleep(1)
    updatePickerInput(session, "feature_ds_variableSelect",
                      choices = sort(names(processed_data())),
                      selected = ""
    )
    updatePickerInput(session, "feature_p_variableSelect",
                      choices = sort(names(processed_data())),
                      selected = ""
    )
    updatePickerInput(session, "feature_stats_variableSelect",
                      choices = sort(names(processed_data())),
                      selected = ""
    )
    updatePickerInput(session, "feature_cluster_variableSelect",
                      choices = sort(names(processed_data())),
                      selected = ""
    )
    updatePickerInput(session, "feature_c_fi_variableSelect",
                      choices = sort(names(processed_data())),
                      selected = ""
    )
    updatePickerInput(session, "feature_r_fi_variableSelect",
                      choices = sort(names(processed_data())),
                      selected = ""
    )
    updatePickerInput(session, "feature_scorr_variableSelect",
                      choices = sort(names(processed_data())),
                      selected = ""
    )
    updatePickerInput(session, "feature_mcorr_variableSelect",
                      choices = sort(names(processed_data())),
                      selected = ""
    )
  })
})

# download processed dataframe
output$download_processed <- downloadHandler(
  filename = function() {
    paste("processed_featuredata", ".csv", sep = "")
  },
  content = function(file) {
    withProgress(
      message = paste0("Saving Data"),
      value = 0,
      {
        shiny::incProgress(1 / 10)
        Sys.sleep(1)
        shiny::incProgress(5 / 10)
        fwrite(rvals$rval_processeddf, file)
      }
    )
  }
)

# download report for processed data
output$report_processeddata <- downloadHandler(
  filename = "report_data_processing.html",
  content = function(file) {
    withProgress(
      message = paste0("Generating Report"),
      value = 0,
      {
        shiny::incProgress(1 / 5)
        src_img <- file.path(getwd(), "www/tRigon_logo.png")
        file.copy(src_img, "tRigon_logo.png")
        tempReport <- file.path(getwd(), "report_data_processing.Rmd")
        file.copy("report_data_processing.Rmd", tempReport, overwrite = FALSE)
        shiny::incProgress(2 / 5)
        # Set up parameters to pass to Rmd document
        params <- list(session_info = devtools::session_info()$platform,
                       human_processing_en = input$workflowHuman,
                       calculations_en = input$keepCalculations,
                       datapath_metadata = input$metaData,
                       datapath_processdata = input$featureData,
                       processed_datadf = rvals$rval_processeddf,
                       meta_datadf  = rvals$rval_metadatadf,
                       matchedID_datadf = rvals$rval_IDmatches_df,
                       n_files = rvals$rval_nfiles)
        shiny::incProgress(3 / 5)
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
        shiny::incProgress(4 / 5)
      }
    )
  }
)
