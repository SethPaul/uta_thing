journeys <- readr::read_csv('./../../../Users/seth/Documents/uta_journeys.csv')
murray_station <- c(40.66013, -111.89534)
lehi_station <- c(40.425278, -111.896389)

library(ggplot2)
library(dplyr)
library(jsonlite)
library(lubridate)
library(caret)
library(xgboost)

journeys %>%
  group_by(recordedattime_day= day(with_tz(recordedattime, 'America/denver')), 
           recordedattime_dow= wday(with_tz(recordedattime, 'America/denver')), direction) %>%
  summarise(n()) 


journeys %>%
  mutate(
    recordedattime= with_tz(recordedattime, 'America/denver'),
    recordedattime_day= day(recordedattime)) %>%
  filter(recordedattime_day != max(recordedattime_day)) %>%
  filter(direction=='SOUTHBOUND') %>%
  mutate(recordedattime_dow= wday(recordedattime),
    vehicleref= as.integer(vehicleref),
        dist_to_murray = sqrt((murray_station[1]-Latitude)^2+(murray_station[2]-Longitude)^2),
         is_murray = dist_to_murray< 0.01,
    vehicleref <300) %>% 
  filter((vehicleref > 100)) %>% 
  filter((vehicleref < 400)) %>%
  ggplot(aes(x=recordedattime, y= dist_to_murray, color=recordedattime_dow )) +
  geom_point() +
  facet_grid(vehicleref~.)

  journeys %>%
    filter(direction=='SOUTHBOUND') %>%
  mutate(
    
    recordedattime_dow= wday(with_tz(recordedattime, 'America/denver')),
    vehicleref= as.integer(vehicleref),
    dist_to_murray = sqrt((murray_station[1]-Latitude)^2+(murray_station[2]-Longitude)^2),
    is_murray = dist_to_murray< 0.01) %>% 
  # filter((vehicleref == 110)) %>%
  # filter((recordedattime_day == 13)) %>%
  # filter((recordedattime_hour > 15)) %>%
  # filter((recordedattime_hour < 21)) %>%
    # head(20)
    
  group_by(vehicleref) %>%
  arrange(recordedattime) %>%
  mutate(did_arrive = is_murray != lag(is_murray) & is_murray,
         arrivals = cumsum(as.integer(did_arrive))) %>%
  group_by(arrivals, vehicleref) %>%
  mutate(tta = max(recordedattime_unix)-recordedattime_unix ) %>%
    filter(tta<4000) %>%
    group_by(vehicleref) %>%
    # filter(recordedattime_day != max(recordedattime_day)) %>%
    
    ggplot(aes(x=dist_to_murray, y=tta, color=as.factor(recordedattime_dow))) +
    geom_point() +
    facet_grid(recordedattime_dow~.)
    
  ggplot(aes(x=recordedattime, y= dist_to_murray, color=tta, shape= did_arrive)) +
  geom_point() +
  facet_grid(vehicleref~.)

  
  
  train <- 
    journeys %>%
    mutate(
      recordedattime= with_tz(recordedattime, 'America/denver'),
      recordedattime_day= day(recordedattime),
      recordedattime_dow= wday(recordedattime, locale = Sys.getlocale("LC_TIME"))
      ) %>%
    filter(direction=='SOUTHBOUND') %>%
    mutate(
      vehicleref= as.integer(vehicleref),
      dist_to_murray = sqrt((murray_station[1]-Latitude)^2+(murray_station[2]-Longitude)^2),
      is_murray = dist_to_murray< 0.009
      ) %>% 
    group_by(vehicleref) %>%
    arrange(recordedattime) %>%
    mutate(did_arrive = is_murray != lag(is_murray) & is_murray,
           arrivals = cumsum(as.integer(did_arrive))) %>%
    group_by(arrivals, vehicleref) %>%
    mutate(tta = max(recordedattime_unix)-recordedattime_unix,
           last_speed = lag(speed,1),
           last_speed2 = lag(speed,2),
           last_lat = lag(Latitude,1),
           last_lat2 = lag(Latitude,2),
           last_long = lag(Longitude,1),
           last_long2 = lag(Longitude,2),
           last_dist = lag(dist_to_murray,1),
           last_dist2 = lag(dist_to_murray,2)
           ) %>%
    filter(tta<4000) %>%
    group_by(vehicleref) %>%
    filter(recordedattime_day != max(recordedattime_day)) %>%
    ungroup() %>% 
    filter(complete.cases(.)) %>%
    # select(-dist_to_murray, -Latitude,-Longitude)
  # %>%
  #   # View()
    select(
  #     Latitude, Longitude, dist_to_murray, 
      last_dist, last_dist2, last_lat, last_lat2,last_long, last_long2 ,
      recordedattime_dow, tta, recordedattime_hour, speed, last_speed, last_speed2, recordedattime
      )
  # 
  
  dummy_dow = dummyVars(~dow, data = train %>% mutate(dow=as.factor(recordedattime_dow)))
  train_dow <-as.data.frame(predict(dummy_dow, train %>% mutate(dow=as.factor(recordedattime_dow))))
   
  train_w_dow <- train %>% 
    select(-recordedattime_dow) %>%
    cbind(train_dow)
  train %>%
    ggplot(aes(x=dist_to_murray, y=tta, color=recordedattime_dow)) +
    geom_point() +
    facet_grid(recordedattime_dow~.)
  
  
  xgb_test <- train_w_dow %>% filter(recordedattime_hour >= 20  & recordedattime_hour <= 22)
  xgb_train <- train_w_dow %>% filter((recordedattime_hour < 20) | recordedattime_hour > 22)
  
  xgb_model <- xgboost( 
    label = xgb_train$tta, data = xgb_train %>%
      select(-tta, -recordedattime_hour,-recordedattime) %>%
      data.matrix(),
    objective = 'reg:linear',
    nrounds =1000
    # max_tree_depth=20,
    # booster = "gbtree",
    # colsample_bytree=0.800,
    # subsample=0.40,
    # lambda=20,
    # alpha=5
    )
  
  importance_matrix <- xgb.importance(model = xgb_model)
  print(importance_matrix)
  xgb.plot.importance(importance_matrix = importance_matrix)
  
  
  xgb_predict <- predict(xgb_model, 
                         newdata = xgb_test %>%
                           select(-tta, -recordedattime_hour, -recordedattime) %>%
                           data.matrix())
  
  
  plot(xgb_predict)
  plot(xgb_predict- xgb_test$tta)
  mean(xgb_predict- xgb_test$tta)
  sd(xgb_predict- xgb_test$tta)
  
  cbind(xgb_test,xgb_predict) %>% 
    mutate(diff = tta-xgb_predict,
           row_num = row_number()) %>%
    reshape2::melt(measure.vars=c('tta','xgb_predict', 'diff' )) %>% 
    # head(100)
    ggplot(aes(x=last_dist, y=value, color = variable)) +
    geom_point() +
    facet_grid(variable ~., scales = 'free_y') +
    geom_abline(aes(intercept=300, slope =0))+
    geom_abline(aes(intercept=-300, slope =0))

    fit <- lm(tta ~ 
              dow.1 + 
              dow.2 + 
              dow.3 + 
              dow.4 + 
              dow.5 + 
              dow.6 + 
              dow.7 + last_dist+ last_dist2+ last_lat+ last_lat2+last_long+ last_long2 +
             speed+ last_speed+ last_speed2
            , data=xgb_train)
  summary(fit)
  pr = fitted(fit)
  plot(pr- xgb_train$tta)
  
  pr_t = predict(fit, newdata = xgb_test)
  plot(pr_t- xgb_test$tta)
  mean(pr_t- xgb_test$tta)
  sd(pr_t- xgb_test$tta)
  
  
  
  together <- journeys %>%
    mutate(
      recordedattime= with_tz(recordedattime, 'America/denver'),
      recordedattime_day= day(recordedattime),
      recordedattime_dow= wday(recordedattime, locale = Sys.getlocale("LC_TIME"))
    ) %>%
    filter(direction=='SOUTHBOUND') %>%
    mutate(
      vehicleref= as.integer(vehicleref),
      dist_to_murray = sqrt((murray_station[1]-Latitude)^2+(murray_station[2]-Longitude)^2),
      is_murray = dist_to_murray< 0.009
    ) %>% 
    group_by(vehicleref) %>%
    arrange(recordedattime) %>%
    mutate(did_arrive = is_murray != lag(is_murray) & is_murray,
           arrivals = cumsum(as.integer(did_arrive))) %>%
    group_by(arrivals, vehicleref) %>%
    mutate(tta = max(recordedattime_unix)-recordedattime_unix ) %>%
    filter(tta<4000) %>%
    filter(complete.cases(.)) %>%
    filter(recordedattime_day != max(recordedattime_day)) %>%
    ungroup() %>% mutate(predicted_arrival=fitted(fit))
  
  together %>%
    reshape2::melt(id.vars=c('recordedattime', 'vehicleref'), measure.vars=c('tta', 'predicted_arrival')) %>%
    ggplot(aes(x=recordedattime, y=value, color=variable)) +
    geom_point() +
    facet_grid(vehicleref~.)
  
  together %>%
    # reshape2::melt(id.vars=c('recordedattime', 'vehicleref'), measure.vars=c('tta', 'predicted_arrival')) %>%
    ggplot(aes(x=recordedattime, y=tta-predicted_arrival)) +
    geom_point() +
    facet_grid(vehicleref~.)
  
  
  dummyVars(~as.factor(recordedattime_dow), data = train)
  # lat, long, tta
journeys %>%
  mutate(
    vehicleref= as.integer(vehicleref),
    dist_to_murray = sqrt((murray_station[1]-Latitude)^2+(murray_station[2]-Longitude)^2),
    is_murray = dist_to_murray< 0.005,
    vehicleref <300) %>% 
  filter((vehicleref == 110)) %>% 
  ggplot(aes(x=recordedattime, y= dist_to_murray, color=is_murray )) +
  geom_point() +
  facet_grid(vehicleref~.)
