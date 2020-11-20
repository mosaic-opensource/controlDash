library(tidyverse)
library(gridExtra)
library(lubridate)
library(plotly)
library(ggthemes)
library(scales)
library(rmarkdown)
library(readr)
library(RColorBrewer)
library(dygraphs)
library(broom)

load("./Data/GABASIC.RData")
load("./Data/GAPAGEINFO.RData")
load("./Data/GAADSINV.RData")
load("./Data/GASALES.RData")

# table with users, sessions, pv and unique pv per date
fundamentals <- gaBasic %>% 
  group_by(date) %>% 
  summarise(
    users = sum(users),
    sessions = sum(sessions),
    pageViews = sum(pageviews),
    uniquePageViews = sum(uniquePageviews)
  ) %>% 
  left_join(
    gaPages %>% 
      group_by(date) %>% 
      summarise(
        bounceRate = mean(bounceRate, na.rm = TRUE),
        avgTimeOnPage = mean(avgTimeOnPage, na.rm = TRUE),
        bouncRate_median = median(bounceRate, na.rm = TRUE),
        avgTimeOnPage_median = median(avgTimeOnPage, na.rm = TRUE)
      ),
    by = "date"
  )

line_sessions_date <-  ggplotly(
  ggplot(data = fundamentals, 
         aes(y = sessions, 
             x = date)
  ) + 
    geom_area(
      fill = "#69b3a2", alpha=0.5
    ) + 
    geom_line(
      color="#69b3a2"
    ) +
    labs(x = "", y = "Sessions") +
    scale_x_date(date_breaks  = "1 month") + 
    theme(axis.text.x = element_text(angle = 90)) +
    theme_solarized()
)


bar_sessions_month <- ggplot(data = fundamentals, 
                             aes(y = sessions, 
                                 x = month(date, label=TRUE, abbr=TRUE))
) + 
  geom_bar(stat="identity",
           fill = "#69b3a2", alpha=0.7) +
  labs(x = "Mês") +
  scale_y_continuous(labels = scales::comma) + 
  theme_solarized() + 
  theme(text = element_text(size = 10))

bar_users_month <- ggplot(data = fundamentals, 
                          aes(y = users/1000, 
                              x = month(date, label=TRUE, abbr=TRUE))
) + 
  geom_bar(stat="identity",
           fill = "#DD1C1A", alpha=0.6) +
  labs(x = "Mês") + 
  scale_y_continuous(labels = scales::comma) +
  theme_solarized() +
  theme(text = element_text(size = 10))

bar_pageViews_month <- ggplot(data = fundamentals, 
                              aes(y = pageViews/1000, 
                                  x = month(date, label=TRUE, abbr=TRUE))
) + 
  geom_bar(stat="identity",
           fill = "#F0C808", alpha=0.6) +
  labs(x = "Mês") + 
  scale_y_continuous(labels = scales::comma) +
  theme_solarized()+ 
  theme(text = element_text(size = 10))

bar_uniquePageViews_month <- ggplot(data = fundamentals, 
                                    aes(y = uniquePageViews/1000, 
                                        x = month(date, label=TRUE, abbr=TRUE))
) + 
  geom_bar(stat="identity",
           fill = "#086788", alpha=0.5) +
  labs(x = "Mês") +
  scale_y_continuous(labels = scales::comma) +
  theme_solarized() + 
  theme(text = element_text(size = 10))


bar_avgBounceRate_month <- fundamentals %>%
  group_by(month = month(date, label=TRUE, abbr=TRUE)) %>% 
  summarise(bounceRate = mean(bounceRate, na.rm = TRUE)) %>% 
  ggplot( 
    aes(y = bounceRate/100, 
        x = month)
  ) + 
  geom_bar(stat="identity",
           fill = "#FFB5C2", alpha=0.7) +
  labs(x="Mês") +
  scale_y_continuous(labels = scales::percent) +
  theme_solarized() +
  theme(text = element_text(size = 10))

bar_avgTimeOnPage_month <-  fundamentals %>%
  group_by(month = month(date, label=TRUE, abbr=TRUE)) %>% 
  summarise(avgTimeOnPage = mean(avgTimeOnPage, na.rm = TRUE)) %>% 
  ggplot( 
    aes(y = avgTimeOnPage/60, 
        x = month)
  ) + 
  geom_bar(stat="identity",
           fill = "#7A28CB", alpha=0.5) +
  labs(x="Mês") +
  theme_solarized() +
  theme(text = element_text(size = 10))


trafic_sources <- gaBasic %>% 
  mutate(
    t_users = sum(users),
    t_sessions = sum(sessions)) %>% 
  group_by(medium) %>% 
  summarize(
    users = sum(users),
    sessions = sum(sessions),
    sessions_perc = round(sum(sessions)/mean(t_sessions)*100,2),
    users_perc = round(sum(users)/mean(t_users)*100,2)) %>% 
  arrange(desc(sessions_perc))

sessions_sources <- trafic_sources %>% 
  mutate(g = ifelse(sessions_perc > 10 ,medium,"other")) %>% 
  group_by(g) %>% 
  summarise(sessions_perc = sum(sessions_perc))

labels <- paste(sessions_sources$g,"\n",sessions_sources$sessions_perc," %")
sessions_origin_pie <- pie(sessions_sources$sessions_perc, labels = labels, border = "white", col = brewer.pal(5, "YlOrRd"), main = "Sessões por origem")

users_sources <- trafic_sources %>% 
  mutate(g = ifelse(users_perc > 10 , medium, "other")) %>% 
  group_by(g) %>% 
  summarise(users_perc = sum(users_perc))

labels <- paste(users_sources$g,"\n",users_sources$users_perc," %")
users_origin_pie <- pie(users_sources$users_perc, labels = labels, border = "white", col = brewer.pal(5, "YlOrRd"), main = "Users por origem")


grouped_sales <- export %>% 
  group_by(date) %>% 
  summarise(
    quant_trans = n(),
    value_trans = sum(totalValue),
    value_avg = mean(totalValue, na.rm = TRUE)
  )

grouped_ads <- gaAds %>% 
  group_by(date) %>% 
  summarise(ga_investment = sum(adCost))

roas_base <- grouped_sales %>% 
  left_join(grouped_ads, by = "date")

# correcting missing dates

dateIndex = data.frame(index = seq(min(roas_base$date), max(roas_base$date), by = "days"))
roas_base <- left_join(dateIndex, roas_base, by = c("index" = "date")) 

## On case of NA we assume that investment is in line with previous day values
roas_base <- tidyr::fill(roas_base, ga_investment)


data <- select(roas_base,-value_avg, -quant_trans) %>% 
  gather(key = "variable", value = "value", -index)


cac_roas <- ggplotly(
  ggplot(data = data, 
         aes(x = index, y  = value, color = variable, fill = variable)) + 
    geom_area(alpha=0.2, position = 'identity') + 
    geom_line() +
    labs(x = "", y = "Google Ads investimento (euros)") +
    scale_y_continuous(labels = scales::comma) + 
    scale_x_date(date_breaks  = "5 day") + 
    theme_solarized() + 
    theme(text = element_text(size = 10),
          axis.text.x = element_text(angle = 45))
)

rm(data)
