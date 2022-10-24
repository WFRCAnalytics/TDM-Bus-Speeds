


clean_gtfs_joint_data <- function(gtfs_with_tdm_lines, maxSpeedDif, maxDistance){
  gtfs_with_tdm_lines %>%
    #filter out missing values
    filter(!is.na(FT), !is.na(aveSpeed)) %>%
    #select only needed attributes
    select(LabelNum, Label, PkOk, stop_sequence, FT, FTCLASS, AREATYPE, aveTimepoint, aveDistance, aveSpeed, AM_SPD, MD_SPD, dist) %>%
    # determine modeled and observed speed
    mutate(Modeled = ifelse(PkOk == "pk", AM_SPD, MD_SPD),
           Observed = aveSpeed) %>%
    # filter out speed discrepancies of larger than 30 mph and distances larger than 2500 meters
    mutate(dif = abs(Observed - Modeled)) %>%
    mutate(filterout = ifelse(dif > maxSpeedDif, TRUE, 
                              ifelse(dist > maxDistance, TRUE, FALSE))) %>%
    filter(filterout == FALSE)
}

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
