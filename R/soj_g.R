#' Soj-g function from Marcotte et al 2021
#'
#' @param data A dataframe with raw wrist accelerometer data with column names Timestamp, AxisX, AxisY, AxisZ, VM
#' @param freq Sampling frequency of raw accelerometer data. 80 Hz by default
#' @param step1_sd_threshold Threshold used to classify likely inactive periods
#' @param step2_nest_length Window length for classifying inactive vs active using random forest model
#' @param step3_nest_length Window length for partitioning excessively long sojourns as determined by step3_orig_soj_length_min
#' @param step3_orig_soj_length_min Maximum sojourn window duration allowed until partitioning into nested sojourns
#' @param min_soj_length Minimum sojourn window duration allowed. Any sojourns smaller will be combined with the
#'
#' @example
#'
#' Library Dependencies: matrixStats, data.table, zoo, dplyr, randomForest, tools, AGread

soj_g <- function(data                      = NA,
                  export_format             = "session",
                  freq                      = 80,
                  step1_sd_threshold        = .00375,
                  step2_nest_length         = 5,
                  step3_nest_length         = 60,
                  step3_orig_soj_length_min = 180) {

  # Remove last partial fraction of a second if number of observations is not a clean multiple of the sampling frequency
  if (nrow(data) %% freq != 0) {

    data <- data[1:(nrow(data) - (nrow(data) %% freq)), ]

  }

  #
  # Step 1 - Identify likely inactive periods----
  #
  message(
    paste0(
      "...Identifying likely inactive periods using sd_vm threshold = ",
      step1_sd_threshold
    )
  )
  # Step 1 - Identify likely inactive periods
  data$index <-
    rep(1:ceiling(nrow(data) / freq),
        length.out = nrow(data),
        each       = freq)
  data_summary <-
    data %>%
    group_by(index) %>%
    dplyr::summarize(sd_vm = sd(VM,
                                na.rm = TRUE))
  data_summary$step1_estimate <-
    ifelse(data_summary$sd_vm <= step1_sd_threshold,
           yes = 1,
           no  = 0) # 1 = inactive, 0 = unclassified
  seconds_index <-
    seq(from = 1,
        to   = nrow(data),
        by   = freq)
  diffs <-
    which((dplyr::lag(data_summary$step1_estimate) != data_summary$step1_estimate) == TRUE)
  diffs <-
    c(1, diffs)
  data_summary$step2_sojourn_index <-
    NA
  data_summary$step2_sojourn_duration <-
    NA

  #
  # Step 2 - Segment remaining unlabeled periods into smaller windows, identify whether inactive or active----
  #
  message("...Segmenting remaining unlabeled periods into smaller windows")
  data_summary$step2_sojourn_index <-
    data.table::rleid(data_summary$step1_estimate)
  data_summary$step2_sojourn_duration[diffs] <-
    rle(data_summary$step2_sojourn_index)[[1]]
  data_summary$step2_sojourn_duration <-
    zoo::na.locf(data_summary$step2_sojourn_duration)
  data_summary <-
    data_summary %>%
    group_by(step2_sojourn_index) %>%
    mutate(
      step2_sojourn_index = nest_sojourn(step2_sojourn_index,
                                         orig_soj_length_min = step2_nest_length,
                                         nest_length         = step2_nest_length)
    )
  # data_summary$step2_sojourn_index = sort(unlist(tapply(data_summary$step2_sojourn_index, data_summary$step2_sojourn_index, nest_sojourn, nest_length = step2_nest_length)))

  # Repopulate original data with step1 and 2 sojourn ID
  data$VM_sd_1sec <-
    rep(data_summary$sd_vm,
        length.out = nrow(data),
        each       = freq)
  data$step1_estimate <-
    rep(data_summary$step1_estimate,
        length.out = nrow(data),
        each       = freq)
  data$step2_sojourn_index <-
    rep(data_summary$step2_sojourn_index,
        length.out = nrow(data),
        each       = freq)
  data$step2_sojourn_duration <-
    rep(data_summary$step2_sojourn_duration,
        length.out = nrow(data),
        each       = freq)

  #
  # Step 3 - Compute features within nested sojourns
  #
  message("...Computing signal features in nested sojourn windows")
  ag_step2_summary <-
    ag_feature_calc(
      data,
      samp_freq       = freq,
      window          = "sojourns",
      soj_colname     = "step2_sojourn_index",
      seconds_colname = "step2_sojourn_duration"
    )
  ag_step2_summary$step2_durations <-
    rle(data_summary$step2_sojourn_index)[[1]]
  ag_step2_summary$step2_estimate <-
    predict(MOCAModelData::sojg_stage2_unclassified_rf,
            newdata = ag_step2_summary,
            type    = "class")

  # Append the step2 activity state estimate to the 1-sec summary dataframe
  data_summary$step2_estimate <-
    rep(ag_step2_summary$step2_estimate,
        times = ag_step2_summary$step2_durations)
  # data_summary$step2_estimate = ifelse(data_summary$step1_estimate == 1, 1, data_summary$step2_estimate) # Repopulate inactive periods from step 1 in case the step2 classified it differently
  data_summary$step3_sojourn_index <-
    NA
  data_summary$step3_sojourn_duration <-
    NA
  diffs <-
    which((dplyr::lag(data_summary$step2_estimate) != data_summary$step2_estimate) == TRUE)
  diffs <-
    c(1, diffs)

  # Verify that labels under step2_estimate are character dummy variables
  if (all(!is.na(as.numeric(levels(data_summary$step2_estimate))))) {

    data_summary$step2_estimate <-
      factor(data_summary$step2_estimate,
             levels = c(1, 2),
             labels = c("Stationary", "Active"))

  }

  data_summary$step3_sojourn_index <-
    data.table::rleid(data_summary$step2_estimate)
  data_summary$step3_sojourn_duration[diffs] <-
    rle(data_summary$step3_sojourn_index)[[1]]
  data_summary$step3_sojourn_duration <-
    zoo::na.locf(data_summary$step3_sojourn_duration)
  data_summary <-
    data_summary %>%
    group_by(step3_sojourn_index) %>%
    mutate(
      step3_sojourn_index = nest_sojourn(step3_sojourn_index,
                                         orig_soj_length_min = step3_orig_soj_length_min,
                                         nest_length         = step3_nest_length)
    )

  data$step2_estimate <-
    rep(data_summary$step2_estimate,
        length.out = nrow(data),
        each       = freq)
  data$step3_sojourn_index <-
    rep(data_summary$step3_sojourn_index,
        length.out = nrow(data),
        each       = freq)
  data$step3_sojourn_duration <-
    rep(data_summary$step3_sojourn_duration,
        length.out = nrow(data),
        each       = freq)

  # Compute features in final sojourns
  ag_step3_summary <-
    ag_feature_calc(
      data,
      samp_freq       = freq,
      window          = "sojourns",
      soj_colname     = "step3_sojourn_index",
      seconds_colname = "step3_sojourn_duration"
    )
  ag_step3_summary$step3_durations <-
    rle(data_summary$step3_sojourn_index)[[1]]
  final_step2_estimate <-
    data_summary %>%
    group_by(step3_sojourn_index) %>%
    summarize(step2_estimate = dplyr::first(step2_estimate)) %>%
    select(step2_estimate) %>%
    ungroup() %>%
    as.vector()
  ag_step3_summary$step2_estimate <-
    final_step2_estimate$step2_estimate

  ag_step3_summary$step3_estimate_intensity <-
    predict(MOCAModelData::sojg_stage3_intensity_rf,
            newdata = ag_step3_summary,
            type    = "class")
  ag_step3_summary$step3_estimate_type <-
    predict(MOCAModelData::sojg_stage3_activity_rf,
            newdata = ag_step3_summary,
            type    = "class")
  ag_step3_summary$step3_estimate_locomotion <-
    predict(MOCAModelData::sojg_stage3_locomotion_rf,
            newdata = ag_step3_summary,
            type    = "class")

  if (export_format == "session") {

    session_summary <-
      data.frame(
        Sedentary_minutes     = sum(ag_step3_summary$step3_durations[which(ag_step3_summary$step3_estimate_intensity == "Sedentary")]),
        Light_minutes         = sum(ag_step3_summary$step3_durations[which(ag_step3_summary$step3_estimate_intensity == "Light")]),
        Moderate_minutes      = sum(ag_step3_summary$step3_durations[which(ag_step3_summary$step3_estimate_intensity == "Moderate")]),
        Vigorous_minutes      = sum(ag_step3_summary$step3_durations[which(ag_step3_summary$step3_estimate_intensity == "Vigorous")]),
        MVPA_minutes          = sum(ag_step3_summary$step3_durations[which(ag_step3_summary$step3_estimate_intensity == "Moderate" | ag_step3_summary$step3_estimate_intensity == "Vigorous")]),
        Sitting_Lying_minutes = sum(ag_step3_summary$step3_durations[which(ag_step3_summary$step3_estimate_type == "Sitting_Lying")]),
        Stationary_minutes    = sum(ag_step3_summary$step3_durations[which(ag_step3_summary$step3_estimate_type == "Stationary+")]),
        Walking_minutes       = sum(ag_step3_summary$step3_durations[which(ag_step3_summary$step3_estimate_type == "Walking")]),
        Running_minutes       = sum(ag_step3_summary$step3_durations[which(ag_step3_summary$step3_estimate_type == "Running")]),
        Locomotion_minutes    = sum(ag_step3_summary$step3_durations[which(ag_step3_summary$step3_estimate_locomotion == "Locomotion")]),
        Total_minutes         = sum(ag_step3_summary$step3_durations),
        stringsAsFactors = FALSE
      )
    session_summary[, 1:ncol(session_summary)] <-
      session_summary[, 1:ncol(session_summary)] / 60

    return(session_summary)

  }

  if (export_format == "sojourn") {

    return(ag_step3_summary)

  }

  if (export_format == "seconds") {

    data_summary$step3_estimate_intensity <-
      rep(ag_step3_summary$step3_estimate_intensity,
          times = ag_step3_summary$step3_durations)
    data_summary$step3_estimate_type <-
      rep(ag_step3_summary$step3_estimate_type,
          times = ag_step3_summary$step3_durations)
    data_summary$step3_estimate_locomotion <-
      rep(ag_step3_summary$step3_estimate_locomotion,
          times = ag_step3_summary$step3_durations)

    data_summary$Timestamp <-
      data$Timestamp[
        seq(from = 1,
            to   = nrow(data),
            by   = freq)[1:nrow(data_summary)]
      ]
    data_summary <-
      data_summary %>%
      dplyr::relocate(Timestamp)

    return(data_summary)

  }

  if (export_format == "raw") {

    data_summary$step3_estimate_intensity <-
      rep(ag_step3_summary$step3_estimate_intensity,
          times = ag_step3_summary$step3_durations)
    data_summary$step3_estimate_type <-
      rep(ag_step3_summary$step3_estimate_type,
          times = ag_step3_summary$step3_durations)
    data_summary$step3_estimate_locomotion <-
      rep(ag_step3_summary$step3_estimate_locomotion,
          times = ag_step3_summary$step3_durations)

    data$step3_estimate_intensity <-
      rep(data_summary$step3_estimate_intensity,
          length.out = nrow(data),
          each = freq)
    data$step3_estimate_type <-
      rep(data_summary$step3_estimate_type,
          length.out = nrow(data),
          each = freq)
    data$step3_estimate_locomotion <-
      rep(data_summary$step3_estimate_locomotion,
          length.out = nrow(data),
          each = freq)

    return(data)

  }
}
