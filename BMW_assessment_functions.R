library(tidyverse)
library(lubridate)
library(dataxray)
library(correlationfunnel)
library(kableExtra)
library(corrplot)
library(tidymodels)
library(plotly)
source("data/theme.R")


# 1. Reading the data

# data about the painting process
df_process <- read_csv("data/P_Data.csv")


# data max and min of parameter per color
df_params <- read_csv("data/P_Parameters.csv")

# data of color/code
df_colors <- read_csv("data/P_Material.csv")

# data environmental variables per day
df_sensor <- read_csv("data/Sensor_Data.csv")



# data defect per component per day
df_defects <- read_csv("data/Q_event_Data.csv")

# data id car and id defect
df_ids <- read_csv("data/Q_igefs_Data.csv")


# 2. Simple data analysis

df_process_xray <- df_process %>% 
  select(-`...1`) %>% 
  make_xray() %>% 
  select(c(ORDER,TYPE,VAR,n,missing, distinct)) %>% 
  mutate(`% missing`=100*missing/n)
  

df_params_xray <- df_params %>% 
  select(-id) %>% 
  make_xray() %>% 
  select(c(ORDER,TYPE,VAR,n,missing, distinct)) %>% 
  mutate(`% missing`=100*missing/n)

df_colors_xray <- df_colors %>% 
  select(-id) %>% 
  make_xray() %>% 
  select(c(ORDER,TYPE,VAR,n,missing, distinct)) %>% 
  mutate(`% missing`=100*missing/n)

df_sensor_xray <- df_sensor %>% 
  select(-id) %>% 
  make_xray() %>% 
  select(c(ORDER,TYPE,VAR,n,missing, distinct)) %>% 
  mutate(`% missing`=100*missing/n)

df_defects_xray <- df_defects %>% 
  select(-`...1`) %>% 
  make_xray() %>% 
  select(c(ORDER,TYPE,VAR,n,missing, distinct)) %>% 
  mutate(`% missing`=100*missing/n)

df_ids_xray <- df_ids %>% 
#  select(-`...1`) %>% 
  make_xray() %>% 
  select(c(ORDER,TYPE,VAR,n,missing, distinct)) %>% 
  mutate(`% missing`=100*missing/n)

# 3 Combining datasets
# Identify cars wit and without defects

df_cars_defects <- df_defects %>% 
  right_join(df_ids)


df_cars_no_defects <- df_process %>% 
  anti_join(df_cars_defects,by = c("igef"))

df_cars_no_defects %>% 
  group_by(igef) %>% 
  count() %>% 
  ungroup() %>% 
  distinct(n)

df_cars_defects %>% 
  group_by(igef) %>% 
  count() %>% 
  ungroup() %>% 
  distinct(n)

df_cars_defects %>% 
  distinct(igef)

df_cars_no_defects <- df_cars_no_defects %>% 
  mutate(status="OK") %>% 
  mutate(year= year(timestamp),
         month=month.name[month(timestamp)], 
         day=factor(weekdays(timestamp),levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
         hour=hour(timestamp),
         minutes=minute(timestamp),
         seconds=second(timestamp)) 

df_cars_no_defects %>% bind_rows(df_cars_defects) %>% 
  distinct(igef, .keep_all=TRUE) %>% 
  count(status) %>% 
  mutate(percentage=percent(n/sum(n)))

df_process_dates <- df_process %>% 
  mutate(year= year(timestamp),
         month=month.name[month(timestamp)], 
         day=factor(weekdays(timestamp),levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
         hour=hour(timestamp),
         minutes=minute(timestamp),
         seconds=second(timestamp)) 

df_process_dates %>% 
group_by(year, month,day)  %>% 
  distinct(igef) %>% 
  count()
  

df_cars_defects_dates <- df_cars_defects %>% 
  left_join(df_process_dates,by = c("igef","component"),relationship = "one-to-one" )


# 
# df_cars_defects_dates %>% 
#   group_by(month, day,hour) %>% 
#   distinct(igef,.keep_all=TRUE) %>% 
#   count() %>% 
#  # filter(month=="January") %>% 
# #  mutate(hour=as.character(hour)) %>% 
#   mutate(d=paste(day,hour)) %>% 
#   ggplot(aes(x = reorder(d,n), y = n, fill = d)) +
#   geom_col(position = "dodge") +
#   theme(legend.position = "") +
#   labs(x = "Day", y = "Number of cars") +
#   ggtitle("Number of cars with defects per day during March 2023") +
#   coord_flip() +
#   facet_grid(rows = vars(month))
#   

df_cars_defects_dates %>% 
  group_by(month, day,hour) %>% 
  distinct(igef,.keep_all=TRUE) %>% 
  count() %>% 
  #filter(month=="February") %>% 
  ggplot(aes(x = day, y = n, fill = day)) +
  geom_col(position = "dodge") +
  theme(legend.position = "") +
  labs(x = "Day", y = "Number of cars") +
  ggtitle("Number of cars with defects per day during February 2023") +
  facet_grid(rows = vars(reorder(month,-n)), cols = vars(hour))
  
df_9150 <- df_cars_defects_dates %>%
  distinct(igef, .keep_all=TRUE) %>% bind_rows(
df_cars_no_defects %>%
  distinct(igef, .keep_all=TRUE)
)




df_cars_no_defects_components <- df_process %>% 
  anti_join(df_cars_defects,by = c("igef","component")) %>% 
  mutate(year= year(timestamp),
         month=month.name[month(timestamp)], 
         day=factor(weekdays(timestamp),levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
         hour=hour(timestamp),
         minutes=minute(timestamp),
         seconds=second(timestamp),
         status="OK") 

df_45750 <- df_cars_no_defects_components %>% 
  bind_rows( df_cars_defects_dates)

df_45750_simple <-  df_45750 %>% 
  select(igef,component,color,presure,voltage,current,spin,status,day,hour) %>% 
  mutate(status_gral=ifelse(status!="OK","Defective",status),
         status_gral_bin=ifelse(status=="OK",1,0)) 

M_1 <-  cor(df_45750_simple %>% 
          select(status_gral_bin,voltage,current,spin,presure,hour))



############## sensor



df_sensor_dates <- df_sensor %>% 
  mutate(year= year(date),
         month=month.name[month(date)], 
         day=factor(weekdays(date),levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
         hour=hour(date),
         minutes=minute(date)) %>% 
        filter(year==2023) %>% 
  select(-id) %>% 
  select(date:minutes, everything()) 


df_45750_min_proc <- df_45750 %>%
  mutate(minutes_measurement_proc=ifelse(minutes<15,0,
                                    ifelse(minutes>=15 & minutes<30,15,
                                           ifelse(minutes>=30 & minutes<45,30,
                                                  ifelse(minutes>=45,45,minutes)))))




df_45750_full_sensor <- df_45750_min_proc %>% 
  left_join(df_sensor_dates,by=c("year","month","day","hour", 'minutes_measurement_proc'='minutes'),relationship = "many-to-many" ) %>% 
  distinct(igef,component, .keep_all = TRUE)

df_45750_somecols_sensor <- df_45750_full_sensor %>% 
select(timestamp:component,status,presure:hour,minutes_measurement_proc:PM10) %>% 
  select(igef:spin,Temp:PM10)

    
df_45750_somecols_sensor_Temp <- df_45750_somecols_sensor %>%
  group_by(status) %>%
  reframe(Temp=Temp,Humy=Humy,PM2=PM2,PM10=PM10,n=n(),
          median=round(median(Temp),2),
          mean=round(mean(Temp),2),
          q_25 = round(quantile(Temp, c(0.25)),2),
          q_75 = round(quantile(Temp, c(0.75)),2))

df_45750_somecols_sensor_Humy <- df_45750_somecols_sensor %>%
  group_by(status) %>%
  reframe(Temp=Temp,Humy=Humy,PM2=PM2,PM10=PM10,n=n(),
          median=round(median(Humy),2),
          mean=round(mean(Humy),2),
          q_25 = round(quantile(Humy, c(0.25)),2),
          q_75 = round(quantile(Humy, c(0.75)),2))

df_45750_somecols_sensor_PM2 <- df_45750_somecols_sensor %>%
  group_by(status) %>%
  reframe(Temp=Temp,Humy=Humy,PM2=PM2,PM10=PM10,n=n(),
          median=round(median(PM2),2),
          mean=round(mean(PM2),2),
          q_25 = round(quantile(PM2, c(0.25)),2),
          q_75 = round(quantile(PM2, c(0.75)),2))

df_45750_somecols_sensor_PM10 <- df_45750_somecols_sensor %>%
  group_by(status) %>%
  reframe(Temp=Temp,Humy=Humy,PM2=PM2,PM10=PM10,n=n(),
          median=round(median(PM10),2),
          mean=round(mean(PM10),2),
          q_25 = round(quantile(PM10, c(0.25)),2),
          q_75 = round(quantile(PM10, c(0.75)),2))

# df_45750_somecols_sensor_PM10%>% 
#    distinct(status,mean,median,q_25,q_75) %>%
#   kable(caption = "Summary of status vs PM10") %>%
#   kable_classic(full_width = F, html_font = "Cambria") %>%
#   kable_styling(latex_options = "HOLD_position")
# 
# 
# ggplotly(
#   df_45750_somecols_sensor_PM10 %>% 
#     #  distinct(status,median) %>% View()
#     ggplot(aes(x = status , y = PM10, group = status)) +
#     geom_boxplot(aes(color = status)) 
#      )
# 
# 

df_45750_somecols_sensor_presure <-  df_45750_somecols_sensor %>%
  group_by(status) %>%
  reframe(presure,voltage,current,spin,n=n(),
          median=round(median(presure),2),
          mean=round(mean(presure),2),
          q_25 = round(quantile(presure, c(0.25)),2),
          q_75 = round(quantile(presure, c(0.75)),2))


ggplotly(
  df_45750_somecols_sensor_Temp  %>%
    #  distinct(status,median) %>% View()
    ggplot(aes(x = status , y = Temp, group = status)) +
    geom_jitter(color="gray", size=0.4, alpha=0.3)+
    geom_boxplot(aes(color = status))
     )

df_45750_somecols_sensor_voltage_2colors <-  df_45750_somecols_sensor %>%
  filter(color!=824) %>% 
  group_by(status) %>%
  reframe(presure,voltage,current,spin,n=n(),
          median=round(median(voltage),2),
          mean=round(mean(voltage),2),
          q_25 = round(quantile(voltage, c(0.25)),2),
          q_75 = round(quantile(voltage, c(0.75)),2))

df_45750_somecols_sensor_voltage_1colors <-  df_45750_somecols_sensor %>%
  filter(color==824, voltage>1000) %>% 
  group_by(status) %>%
  reframe(presure,voltage,current,spin,n=n(),
          median=round(median(voltage),2),
          mean=round(mean(voltage),2),
          q_25 = round(quantile(voltage, c(0.25)),2),
          q_75 = round(quantile(voltage, c(0.75)),2))




df_45750_somecols_sensor_current <-  df_45750_somecols_sensor %>%
  group_by(status) %>%
  reframe(presure,voltage,current,spin,n=n(),
          median=round(median(current),2),
          mean=round(mean(current),2),
          q_25 = round(quantile(current, c(0.25)),2),
          q_75 = round(quantile(current, c(0.75)),2))


df_45750_somecols_sensor_spin <-  df_45750_somecols_sensor %>%
  group_by(status) %>%
  reframe(presure,voltage,current,spin,n=n(),
          median=round(median(spin),2),
          mean=round(mean(spin),2),
          q_25 = round(quantile(spin, c(0.25)),2),
          q_75 = round(quantile(spin, c(0.75)),2))

# df_45750_somecols_sensor_current  %>%
#    distinct(status,mean,median,q_25,q_75) %>%
#   kable(caption = "Summary of status vs presure") %>%
#   kable_classic(full_width = F, html_font = "Cambria") %>%
#   kable_styling(latex_options = "HOLD_position")


# ggplotly(
#   
#   df_45750_somecols_sensor_spin %>%
#     ggplot(aes(x = status , y = spin, group = status)) +
#     geom_boxplot(aes(color = status)) +
#     geom_jitter(color="gray", size=0.4, alpha=0.09) 
#   
#      )








################ models

## model 1

df_45750_full_sensor_model_1 <- df_45750_full_sensor %>% 
  select(igef, component,status,presure:spin) %>% 
  mutate(defects=as.factor(ifelse(status!="OK","Defective",status))) 


#splitting
#set.seed(123)
set.seed(452)
df_45750_full_sensor_model_1_train <- df_45750_full_sensor_model_1 %>% 
  sample_frac(.7)

df_45750_full_sensor_model_1_test <- df_45750_full_sensor_model_1 %>% 
  anti_join(df_45750_full_sensor_model_1_train, by=c("igef","component"))

# df_45750_full_sensor_model_1_train %>%
#   group_by(defects) %>%
#   count() %>%
#   ungroup() %>%
#   mutate(proportion =n/sum(n))
# 
# df_45750_full_sensor_model_1_test %>%
#   group_by(defects) %>%
#   count() %>%
#   ungroup() %>%
#   mutate(proportion =n/sum(n))

# model

glm.parameters <- df_45750_full_sensor_model_1_train %>% 
 glm(defects~ presure+voltage+current+spin,.,family = 'binomial')


df_45750_full_sensor_model_1_train_2 <- df_45750_full_sensor_model_1_train %>%
  mutate(probs=predict(glm.parameters,type="response"),
         prediction=ifelse(probs>0.87,"OK","Defective"),
         correct=ifelse(prediction==defects,"YES","NO"))

df_45750_full_sensor_model_1_test_2 <- df_45750_full_sensor_model_1_test %>%
  mutate(probs=predict(glm.parameters,type="response",df_45750_full_sensor_model_1_test),
         prediction=ifelse(probs>0.87,"OK","Defective"),
         correct=ifelse(prediction==defects,"YES","NO"))

df_45750_full_sensor_model_1_train_2 %>%
  count(correct) %>%
  mutate(proportion=round(n/sum(n),2))

## model 2

df_45750_full_sensor_model_2 <- df_45750_full_sensor %>% 
  select(igef, component,status,Temp:PM10) %>% 
  mutate(defects=as.factor(ifelse(status!="OK","Defective",status))) 


#splitting
set.seed(452)

df_45750_full_sensor_model_2_train <- df_45750_full_sensor_model_2 %>% 
  sample_frac(.7)

df_45750_full_sensor_model_2_test <- df_45750_full_sensor_model_2 %>% 
  anti_join(df_45750_full_sensor_model_1_train, by=c("igef","component"))

# df_45750_full_sensor_model_2_train %>%
#   group_by(defects) %>%
#   count() %>%
#   ungroup() %>%
#   mutate(proportion =n/sum(n))
# # 
# df_45750_full_sensor_model_2_test %>%
#   group_by(defects) %>%
#   count() %>%
#   ungroup() %>%
#   mutate(proportion =n/sum(n))
# 
# model

glm.factors <- df_45750_full_sensor_model_2_train %>% 
  glm(defects ~ Temp+Humy+PM2+PM10,.,family = 'binomial')




df_45750_full_sensor_model_2_train_2 <- df_45750_full_sensor_model_2_train %>%
  mutate(probs=predict(glm.factors,type="response"),
         prediction=ifelse(probs>0.87,"OK","Defective"),
         correct=ifelse(prediction==defects,"YES","NO"))

df_45750_full_sensor_model_2_test_2 <- df_45750_full_sensor_model_2_test %>%
  mutate(probs=predict(glm.factors,type="response",df_45750_full_sensor_model_2_test),
         prediction=ifelse(probs>0.87,"OK","Defective"),
         correct=ifelse(prediction==defects,"YES","NO"))




# df_45750_full_sensor_model_2_train_2 %>% 
#   count(correct) %>% 
#   mutate(proportion=round(n/sum(n),2)) 
# 
# table(df_45750_full_sensor_model_2_test_2$defects,df_45750_full_sensor_model_2_test_2$prediction) 
# 
# df_45750_full_sensor_model_2_test_2 %>% 
#   count(correct) %>% 
#   mutate(proportion=round(n/sum(n),2)) 



#### tests


ggplotly(
  df_45750_somecols_sensor_Temp  %>%
    #  distinct(status,median) %>% View()
    ggplot(aes(x = status , y = Temp, group = status)) +
    #geom_jitter(color="gray", size=0.4, alpha=0.3)+
    #geom_boxplot(aes(color = status))
    geom_point()
)


df_45750_somecols_sensor %>% 
  group_by(status, color,Temp) %>% 
  count() %>% 
  filter(status!="OK") %>% 
  ggplot(aes(x = Temp , y = n, group = status)) +
  #geom_jitter(color="gray", size=0.4, alpha=0.3)+
  #geom_boxplot(aes(color = status))
  geom_point(aes(color = status))

