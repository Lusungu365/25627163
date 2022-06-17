deathsdiseases <- function(data1,data2){
Deaths <-  data1 %>%  select(c(`Entity`,`Year`,`Code`,`Deaths - Tuberculosis - Sex: Both - Age: All Ages (Number)`,
                                    `Deaths - Cardiovascular diseases - Sex: Both - Age: All Ages (Number)`,
                                    `Deaths - Lower respiratory infections - Sex: Both - Age: All Ages (Number)`,
                                    `Deaths - Chronic respiratory diseases - Sex: Both - Age: All Ages (Number)`))
df <- data2 %>%
    mutate(Year= year(date)) %>%
    mutate(Month= month(date)) %>%
    mutate(Day= mday(date)) #%>%
    #mutate(month_year = make_datetime(Year, Month))

df <- df %>%
    mutate(continent=factor(continent)) %>%
    group_by(continent,iso_code,Year) %>%
    summarise_at(vars(total_cases,new_cases,new_deaths), ~mean(.,na.rm=TRUE))%>%
    na.omit(.)

Deaths <- Deaths %>% rename(iso_code = Code)

df <- df %>% select("iso_code","continent")
df <- left_join(Deaths,df, by="iso_code")
df <- df %>%
    mutate(continent=factor(continent)) %>%
    group_by(continent) %>%
    summarise_at(vars("Deaths - Tuberculosis - Sex: Both - Age: All Ages (Number)",
                      "Deaths - Cardiovascular diseases - Sex: Both - Age: All Ages (Number)",
                      "Deaths - Lower respiratory infections - Sex: Both - Age: All Ages (Number)",
                      "Deaths - Chronic respiratory diseases - Sex: Both - Age: All Ages (Number)"), ~mean(.,na.rm=TRUE))%>%
    na.omit(.)

df <- df %>%  rename("chronic_respiratory_deaths"="Deaths - Chronic respiratory diseases - Sex: Both - Age: All Ages (Number)") %>%
    rename("Tuberculosis_deaths"="Deaths - Tuberculosis - Sex: Both - Age: All Ages (Number)") %>%
    rename("Cardiovascular_deaths"="Deaths - Cardiovascular diseases - Sex: Both - Age: All Ages (Number)") %>%
    rename("Lower_Respiratory_infections_deaths"="Deaths - Lower respiratory infections - Sex: Both - Age: All Ages (Number)")

df <- df %>%  gather(disease, Numberofdeaths, Tuberculosis_deaths:chronic_respiratory_deaths)


g <-  df %>%
    ggplot() +
    geom_point(aes(x = continent, y = Numberofdeaths, color=disease), size = 2, alpha = 0.8)
g
}

