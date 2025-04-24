

# Correct for genuine high, directed travelling movement 
if("mean_VeDBA_interval" %in% colnames(df)){
    
df$Verdict_user <- ifelse(
      df$Verdict_IF == "Anomalous" & 
        !is.na(df$mean_VeDBA_interval) & 
        df$mean_VeDBA_interval >= quantile(df$mean_VeDBA_interval, 0.9, na.rm = TRUE) &
        (!is.na(df$Outgoing_speed) & df$Outgoing_speed >= quantile(df$Outgoing_speed, 0.9, na.rm = TRUE) | 
           !is.na(df$Incoming_speed) & df$Incoming_speed >= quantile(df$Incoming_speed, 0.9, na.rm = TRUE)) &
        !is.na(df$Ang_vertex) & df$Ang_vertex < 150,  # Additional condition for Ang_vertex
      "Not Anomalous", ifelse(df$Verdict_IF == "Anomalous" & 
                                is.na(df$mean_VeDBA_interval) & 
                                (!is.na(df$Outgoing_speed) & df$Outgoing_speed >= quantile(df$Outgoing_speed, 0.9, na.rm = TRUE) | 
                                   !is.na(df$Incoming_speed) & df$Incoming_speed >= quantile(df$Incoming_speed, 0.9, na.rm = TRUE)) &
                                !is.na(df$Ang_vertex) & df$Ang_vertex < 150,
                              "Not Anomalous", df$Verdict_user)
    )
    } else {
    # Correct for genuine high, directed travelling movement (without VeDBA)
    df$Verdict_user <- ifelse(
      df$Verdict_IF == "Anomalous" & 
        (!is.na(df$Outgoing_speed) & df$Outgoing_speed >= quantile(df$Outgoing_speed, 0.9, na.rm = TRUE) | 
           !is.na(df$Incoming_speed) & df$Incoming_speed >= quantile(df$Incoming_speed, 0.9, na.rm = TRUE)) &
        !is.na(df$Ang_vertex) & df$Ang_vertex < 150,  # Additional condition for Ang_vertex
      "Not Anomalous",
      df$Verdict_user
    )
    }
    
    # Correct for genuine spikes in GPS during rest
    df$Verdict_user <- ifelse(
      df$Verdict_IF == "Not Anomalous" & 
        !is.na(df$Ang_vertex) & df$Ang_vertex >= 170 & 
        !is.na(df$Dist_circular) & df$Dist_circular <= 25 & 
        (!is.na(df$Outgoing_speed) & df$Outgoing_speed <= quantile(df$Outgoing_speed, 0.1, na.rm = TRUE) |
           !is.na(df$Incoming_speed) & df$Incoming_speed <= quantile(df$Incoming_speed, 0.1, na.rm = TRUE)), 
      "Anomalous", df$Verdict_user)
    
    # Ensure extreme spikes are anomalous
  df$Verdict_user <- ifelse(
    df$Verdict_IF == "Not Anomalous" & 
      !is.na(df$Ang_vertex) & df$Ang_vertex >= 175 & 
      !is.na(df$Dist_circular) & df$Dist_circular <= quantile(df$Dist_circular, 0.1, na.rm = TRUE),
    "Anomalous",
    df$Verdict_user
  )
  # Lastly, ensure no completely unrealistic fix speeds remained after above alterations
  df$Verdict_user <- ifelse(
    df$Verdict_IF == "Not Anomalous" & 
      !is.na(df$Outgoing_speed) & df$Outgoing_speed >= 5,
    "Anomalous",
    df$Verdict_user
  )
    
