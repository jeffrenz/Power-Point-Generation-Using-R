#load data
data <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
data$date <- as_datetime(data$date)
data$state   <- state.abb[match(data$state, state.name)]