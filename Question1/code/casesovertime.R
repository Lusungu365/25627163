
casesovertime <- function(data){
    df <- data %>%
        mutate(Year= year(date)) %>%
        mutate(Month= month(date)) %>%
        mutate(Day= mday(date)) %>%
        mutate(month_year = make_datetime(Year, Month))

    df <- df %>%
        mutate(continent=factor(continent)) %>%
        group_by(continent,month_year) %>%
        summarise_at(vars(total_cases,new_cases,new_deaths), ~mean(.,na.rm=TRUE))%>%
        na.omit(.)

    g <-
        df %>%
        ggplot(na.rm=TRUE) +
        geom_line(aes(x = month_year , y = total_cases, color = continent),size=0.9,
                  alpha = 0.8) +scale_x_datetime( date_breaks = "1 months" ,  date_labels = "%b-%y", expand = c(0.01,0))

    g
}

