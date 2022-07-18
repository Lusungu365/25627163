

area_plot <- function(data, xaxis_size = 5, xaxis_rows = 3){

    #converting date
    lweather<-data %>%   mutate(date = ymd(date)) %>%
        mutate(Year= year(date)) %>%
        mutate(Month= month(date)) %>%
        mutate(month_year = make_datetime(Year, Month))

 lweather <- lweather %>%
        group_by(month_year) %>%
        summarise_at(vars(cloud_cover, sunshine,global_radiation,
        max_temp, mean_temp, min_temp,precipitation,
        pressure, snow_depth), ~mean(.,na.rm=TRUE))%>%
        na.omit(.)

    g <-
        lweather %>%
        ggplot(aes(x = month_year, y = mean_temp)) +
        geom_area( fill = "lightblue")


    g


}
