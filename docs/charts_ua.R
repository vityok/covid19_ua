library(tidyverse)

Sys.setlocale(category="LC_ALL",locale="uk_UA.utf8" )

## Таблиця в файлі covid19_by_area_type_hosp_dynamics.csv має доволі
## велику кількість стовпчиків, нижче їх перелічено для зручності
## подальшого використання в скрипті:
##
## zvit_date,registration_area,priority_hosp_area,edrpou_hosp,
## legal_entity_name_hosp,legal_entity_lat,legal_entity_lng,
## person_gender,person_age_group,add_conditions,is_medical_worker,
## new_susp,new_confirm,active_confirm,new_death,new_recover
area_dyn <- read_csv('../covid19_by_area_type_hosp_dynamics.csv')

daily_area_dyn <- area_dyn %>%
    select(zvit_date, new_susp, new_confirm, new_death, active_confirm) %>%
    group_by(zvit_date) %>%
    summarise(new_susp = sum(new_susp),
              new_confirm = sum(new_confirm),
              new_death = sum(new_death),
              active_confirm = sum(active_confirm))

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
    select(Cdate, new_susp, new_confirm, new_death, active_confirm) %>%
    pivot_longer(-Cdate, names_to = "variable", values_to = "value")

(ggplot(daily_area_gath, aes(x=Cdate,y=value))
    + geom_point(aes(color = variable))
    + geom_line(aes(color = variable, linetype = variable))
##    + geom_smooth(aes(color = variable))
    + theme_light()
)

ggsave("daily_area_gath.png", width=9, height=5.7, dpi=98)

## %%%%%%% Графік поширення за областями %%%%%%%

daily_area_reg_dyn <- area_dyn %>%
    select(zvit_date, registration_area, new_susp, new_confirm, new_death) %>%
    group_by(zvit_date, registration_area) %>%
    summarise(new_susp = sum(new_susp),
              new_confirm = sum(new_confirm),
              new_death = sum(new_death))

daily_area_reg_dyn <- daily_area_reg_dyn %>%
    mutate(Cdate = as.Date(strptime(zvit_date, "%Y-%m-%d")))

(ggplot(daily_area_reg_dyn, aes(x=Cdate, y=new_susp))
    + geom_line()
    + facet_wrap(vars(registration_area), ncol = 4)
    + theme_light()
)

ggsave("daily_area_regions_dyn_susp.png", width=9.5, height=7.7, dpi=98)


## %%%%%%% Аналіз часових рядів %%%%%%%

## Створення об'єктів типу "часовий ряд" на основі фреймів
## даних. Частота 7 рекомендована для щоденних вимирів та тижневій
## "природній" періодичності.
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
png("holt-winters_14days_forecast.png", width=640)

plot(hw.new_susp, fc.new_susp)

dev.off()
