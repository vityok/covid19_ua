library(tidyverse)
library(ggplot2)

## zvit_date,registration_area,priority_hosp_area,edrpou_hosp,
## legal_entity_name_hosp,legal_entity_lat,legal_entity_lng,
## person_gender,person_age_group,add_conditions,is_medical_worker,
## new_susp,new_confirm,active_confirm,new_death,new_recover
area_dyn_csv <- read.csv('../covid19_by_area_type_hosp_dynamics.csv')
area_dyn <- as.data.frame(area_dyn_csv)

daily_area_dyn <- area_dyn %>%
    select(zvit_date, new_susp, new_confirm, new_death) %>%
    group_by(zvit_date) %>%
    summarise(new_susp = sum(new_susp),
              new_confirm = sum(new_confirm),
              new_death = sum(new_death))

daily_area_dyn <- daily_area_dyn %>%
    mutate(Cdate = as.Date(strptime(zvit_date, "%Y-%m-%d"))) %>%
    mutate(Weekday_Num = strftime(Cdate, format="%u")) %>%
    mutate(Weekday_Class = case_when(
               Weekday_Num == "6" ~ "Субота та Неділя",
               Weekday_Num == "7" ~ "Субота та Неділя",
               TRUE ~ "Решта"))

(ggplot(daily_area_dyn,aes(x=Cdate))
    + geom_point(aes(y=new_susp, colour=Weekday_Class))
    + geom_point(aes(y=new_confirm, colour=Weekday_Class))
    + geom_line(aes(y=new_susp))
    + geom_line(aes(y=new_confirm))
    + geom_line(aes(y=new_death))
)

ggsave("daily_area_dyn.png", width=9, height=5.7, dpi=98)

## collapse several columns into a single column "value" (key-value
## pair) factored by the column named "variable"
daily_area_gath <- daily_area_dyn %>%
    select(Cdate, new_susp, new_confirm, new_death) %>%
    pivot_longer(-Cdate, names_to = "variable", values_to = "value")

(ggplot(daily_area_gath, aes(x=Cdate,y=value))
    + geom_point(aes(color = variable))
    + geom_line(aes(color = variable, linetype = variable))
    + theme_light()
)

ggsave("daily_area_gath.png", width=9, height=5.7, dpi=98)

## Створення об'єктів типу "часовий ряд" на основі фреймів даних
ts.new_confirm <- ts(daily_area_dyn$new_confirm, frequency=7)
ts.new_susp    <- ts(daily_area_dyn$new_susp, frequency=7)

## Корелограма нових підозр (просто перегляд)
acf(ts.new_susp)

ccf(ts.new_susp, ts.new_confirm)

## Побудова моделі Холта-Вінтерса
hw.new_susp <- HoltWinters(ts.new_susp, seasonal="multiplicative") # additive

plot(hw.new_susp)

## Прогноз із використанням моделі Холта-Вінтерса на 14 "періодів"
## спостережень. В даному випадку, це 14 днів або 2 тижні
fc.new_susp <- predict(hw.new_susp, 14)

## Графік моделі та прогнозу буде збережено у PNG файл
png("holt-winters_14days_forecast-2020-05-12.png", width=640)
plot(hw.new_susp, fc.new_susp)
dev.off()
