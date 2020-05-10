library(data.table)

# Subsetting the data by complaint, returns a tibble
by_complaint <- function(dataset, complaint, complaint_title){
  datetime <- quo(datetime)
  tibble_name <- paste('tz_latest_', complaint, sep='')
  tally_name <- paste('number_of_calls_', complaint, sep='')
  df <- dataset %>% 
    filter(title == complaint_title) %>%
    group_by(month=floor_date(datetime, "week")) %>%
    tally(name = 'number_of_calls') 
  return(df)
}

# Use this one when you want to only match part of the complaint title, e.g. Vehicle, or Traffic
by_complaint_like <- function(dataset, complaint, complaint_title_match_phrase){
  datetime <- quo(datetime)
  tibble_name <- paste('tz_latest_', complaint, sep='')
  tally_name <- paste('number_of_calls_', complaint, sep='')
  df <- dataset %>% 
    filter(title %like% complaint_title_match_phrase) %>%
    group_by(month=floor_date(datetime, "week")) %>%
    tally(name = 'number_of_calls') 
  return(df)
}

# Plotting the data by complaint, returns a bar plot
plot_by_complaint <- function(df, complaint){
  min_date <- as.Date("2017-01-01")
  max_date <- Sys.Date()
  lims <- c(min_date, max_date)
  plot_title <- paste("911 Calls for", complaint, sep=" ")
  ggplot(df, aes(x=as.Date(month), y=number_of_calls)) +
    geom_bar(stat = "identity") + scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", limits = lims) + 
    geom_smooth(size=1.5, method = "gam", color = "red", se = FALSE) +
    theme_minimal() +
    xlab("Month -Year") +
    ylab("Number of Calls") +
    labs(title = plot_title, subtitle = "Montgomery County, PA") +
    theme(axis.text.x=element_text(angle=60, hjust=1))
}
## Example Call: plot_by_complaint(by_complaint(tz_latest, "fever", "EMS: FEVER"))
# --------------------

# Same as above for 2020

# Subsetting the data by complaint, returns a tibble
by_complaint_2020 <- function(dataset, complaint, complaint_title){
  datetime <- quo(datetime)
  tibble_name <- paste('tz_latest_', complaint, sep='')
  tally_name <- paste('number_of_calls_', complaint, sep='')
  df <- dataset %>% 
    filter(datetime>= "2020-01-01" & title == complaint_title) %>%
    group_by(week=floor_date(datetime, "week")) %>%
    tally(name = 'number_of_calls') 
  return(df)
}

# Use this one when you want to only match part of the complaint title, e.g. Vehicle, or Traffic
by_complaint_like_2020 <- function(dataset, complaint, complaint_title_match_phrase){
  datetime <- quo(datetime)
  tibble_name <- paste('tz_latest_', complaint, sep='')
  tally_name <- paste('number_of_calls_', complaint, sep='')
  df <- dataset %>% 
    filter(datetime>= "2020-01-01" & title %like% complaint_title_match_phrase) %>%
    group_by(week=floor_date(datetime, "week")) %>%
    tally(name = 'number_of_calls') 
  return(df)
}


plot_by_complaint_2020 <- function(df, complaint){
  min_date <- as.Date("2020-01-01")
  max_date <- Sys.Date()
  lims <- c(min_date, max_date)
  plot_title <- paste("911 Calls for", complaint, sep=" ")
  ggplot(df, aes(x=as.Date(week), y=number_of_calls)) +
    geom_bar(stat="identity") + scale_x_date(date_breaks = "week", limits = lims) +
    geom_smooth(method = "gam", color = "red", se = FALSE) +
    theme_minimal() +
    xlab("Week") +
    ylab("Number of Calls") +
    labs(title = plot_title, subtitle = "Montgomery County, PA") +
    theme(axis.text.x=element_text(angle=60, hjust=1))
}
