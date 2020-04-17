
#Total cases by state
total_cases <- function(x){
  
data %>% 
    filter(state==x) %>%
    filter(date == max(date)) %>%
    group_by(date) %>%
    summarise("Total Cases"=sum(cases)) %>%
    select(`Total Cases`)%>%
    as.numeric() %>%
    comma()
}

#Heat map of cases by county

state_map_function <- function(x){
  plot_usmap(data = data, values = "cases", color="white", include = x) +
    scale_fill_continuous(name = "Cases", label = scales::comma, low="grey", high="red") +
    theme(legend.position = "right")
}

#Cumulative cases and fatalities
state_chart_function <- function(x){
  data %>%
    filter(state == x) %>%
    group_by(state, date) %>%
    summarise("Cases"= sum(cases), "Deaths"=sum(deaths)) %>%
    gather(key = "Outcome", value="total", -date,-state) %>%
    ggplot(aes(x=date,y=total,color=Outcome))+
    ggtitle(label="Cumulative Confirmed Cases and Fatalities")+
    scale_y_continuous(labels = scales::comma)+
    ylab(label = "Cumulative Cases") +
    xlab(label = NULL)+
    geom_line(size=1.3)
}

#Chart percent change

percent_change_chart <- function(x){
  change_by_state <- data %>% 
    filter(state == x) %>%
    group_by(date) %>%
    summarise("cum_cases"= sum(cases)) %>%
    mutate(pct_change = (cum_cases-lag(cum_cases))/lag(cum_cases)) %>%
    mutate(days_to_double = log(2)/(log(1 + pct_change)))
  change_by_state %>% 
    ggplot(aes(x=date,y=pct_change)) +
    geom_smooth(size = 1.5) + 
    geom_line() +
    scale_y_continuous(labels = scales::percent)+
    ylab(label = "Percet increase in new cases")+
    xlab(label = NULL) +
    ggtitle("Daily increase in new cases over time")
    
}

#DF of percent change

percent_change_df <- function(x){
  change_by_state <- data %>% 
    filter(state == x) %>%
    group_by(date) %>%
    summarise("cum_cases"= sum(cases)) %>%
    mutate(pct_change = (cum_cases-lag(cum_cases))/lag(cum_cases)) %>%
    mutate(days_to_double = log(2)/(log(1 + pct_change)))
  
  change_by_state$cum_cases <- comma(change_by_state$cum_cases)
  
  
  
  return(tail(change_by_state,15) %>%
           arrange (desc(date)))
}


#key figures

key_figures <- function(x){

change_by_state <- data %>% 
  filter(state == x) %>%
  group_by(date) %>%
  summarise("cum_cases"= sum(cases)) %>%
  mutate(pct_change = (cum_cases-lag(cum_cases))/lag(cum_cases)) %>%
  mutate(days_to_double = log(2)/(log(1 + pct_change)))

change_by_state <- tail(change_by_state, 15) %>%
  arrange(desc(date))

change_by_state$pct_change <- change_by_state$pct_change %>%
  round(3) %>%
  percent()
change_by_state$days_to_double <- round(change_by_state$days_to_double,0)

change_by_state$cum_cases <- comma(change_by_state$cum_cases)

colnames(change_by_state) <- c("Date","Cases","Change","Days to double")

kable(change_by_state)
}














