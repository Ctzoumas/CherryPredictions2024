install.packages('readxl')
install.packages('lubridate')
install.packages('stringr')
install.packages('ggplot2')
install.packages('tidyverse')
library(readxl)
library(lubridate)
library(stringr)
library(ggplot2)
library(tidyverse)



cherry <- read.csv("washingtondc.csv") %>% 
  bind_rows(read.csv("liestal.csv")) %>% 
  bind_rows(read.csv("kyoto.csv")) %>% 
  bind_rows(read.csv("vancouver.csv"))

install.packages("rnoaa")
library(rnoaa)

stations <- ghcnd_stations()

#get max temperatures
get_temperature <- function (stationid) {
  ghcnd_search(stationid = stationid, var = c("tmax"), 
               date_min = "1950-01-01", date_max = "2024-05-31")[[1]] %>%
    mutate(year = as.integer(format(date, "%Y")),
           month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
           season = cut(month, breaks = c(0, 2, 5, 8, 11),
                        include.lowest = TRUE,
                        labels = c("Winter", "Spring", "Summer", "Fall")),
           year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, season) %>%
    summarize(tmax_avg = mean(tmax, na.rm = TRUE))
}
#get min temperatures
get_mintemp <- function (stationid){
  ghcnd_search(stationid = stationid, var = c("tmin"), 
               date_min = "1950-01-01", date_max = "2024-05-31")[[1]] %>%
    mutate(year1 = as.integer(format(date, "%Y")),
           month1 = as.integer(strftime(date, '%m')) %% 12, # make December "0"
           season1 = cut(month1, breaks = c(0, 2, 5, 8, 11),
                         include.lowest = TRUE,
                         labels = c("Winter", "Spring", "Summer", "Fall")),
           year1 = if_else(month1 == 0, year1 + 1L, year1)) %>%
    group_by(year1, season1) %>%
    summarize(tmin_avg = mean(tmin, na.rm = TRUE))
}
#get average seasonal precipitation
get_prcp <- function (stationid){
  ghcnd_search(stationid = stationid, var = c("prcp"), 
               date_min = "1950-01-01", date_max = "2024-05-31")[[1]] %>%
    mutate(year2 = as.integer(format(date, "%Y")),
           month2 = as.integer(strftime(date, '%m')) %% 12, # make December "0"
           season2 = cut(month2, breaks = c(0, 2, 5, 8, 11),
                         include.lowest = TRUE,
                         labels = c("Winter", "Spring", "Summer", "Fall")),
           year2 = if_else(month2 == 0, year2 + 1L, year2)) %>%
    group_by(year2, season2) %>%
    summarize(prcp_avg = mean(prcp, na.rm = TRUE))
}

historic_temperatures <-
  tibble(location = "washingtondc", get_temperature("USC00186350"), get_mintemp("USC00186350"), get_prcp("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_temperature("GME00127786"), get_mintemp("GME00127786"), get_prcp("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_temperature("JA000047759"), get_mintemp("JA000047759"), get_prcp("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_temperature("CA001108395"), get_mintemp("CA001108395"), get_prcp("CA001108395")))


historic_temperatures$tmax_avg <- historic_temperatures$tmax_avg/10
historic_temperatures$tmin_avg <- historic_temperatures$tmin_avg/10

historic_temperatures <- historic_temperatures%>%
  select(-c(year1, season1, year2, season2))


cherry <- cherry %>%
  filter(year >=1950)

 
test <-  merge(cherry, historic_temperatures, by = c("location", "year"), all = TRUE)
test <- na.omit(test)
test <- test %>%
  select(- bloom_date) %>%
  filter(season == "Spring")

#training data set for test
test_1 <- test%>%filter(year<2019)
#test data set for test
test_2 <- test%>%filter(year>=2019)

in_sample <- test %>%
  filter(year <= 2018)

out_sample <- test %>%
  filter(year > 2018)

#getting daily temperatures
start_date = "1951-01-01"
end_date = "2023-12-31"
station_id = c("USC00186350", "GME00127786", "JA000047759", "CA001108395")

weather_data <- ncdc(datasetid='NORMAL_DLY', stationid=paste0('GHCND:',station_id),
                     datatypeid='dly-tavg-normal',
                     startdate = start_date, enddate = end_date,limit=365)


install.packages('tree')
library(tree)
cherry_tree <- tree(bloom_doy ~ .-year, data = test)
plot(cherry_tree)
text(cherry_tree)

treepre <- predict(cherry_tree, newdata = test)
sqrt(mean((test$bloom_doy - treepre)^2)) #7.58 for RMSPE

install.packages('randomForest')
library(randomForest)

set.seed(123)
cherry_bag <- randomForest(bloom_doy ~ . - year, data = test, mtyr = ncol(test)-1, importance = TRUE)
importance(cherry_bag)
varImpPlot(cherry_bag)

rfpre <- predict(cherry_bag, newdata = test)
sqrt(mean((test$bloom_doy - rfpre)^2)) #6.389 for RMSPE

# remotes::install_github("vdorie/dbarts")
# remotes::install_github("vdorie/stan4bart")

library('lme4') 
fit_re <- lmer(bloom_doy ~ tmin_avg + tmax_avg +
                 (1 | location) + (tmin_avg | location) +  (tmin_avg | location),
               data = test)

sqrt(mean((predict(fit_re) - test$bloom_doy)^2)) #8.2
 


library("stan4bart") 
fit <- stan4bart(bloom_doy ~ bart(. - location - tmin_avg - tmax_avg) + 
                   (1 + tmin_avg + tmax_avg | location),
                 test2)#,
                 #cores = 1, seed = 0,
                 #verbose = 1)

bloom_doy_pred <- rowMeans(predict(fit))

sqrt(mean((bloom_doy_pred - test$bloom_doy)^2))  #3.866

bloom_doy_lower <- apply(predict(fit), 1, quantile, prob = .025)
bloom_doy_upper <- apply(predict(fit), 1, quantile, prob = .975)
plot(test$bloom_doy, bloom_doy_pred)

tibble(truth = test$bloom_doy,
       pred = bloom_doy_pred,
       lower = bloom_doy_lower,
       upper = bloom_doy_upper) %>%
  ggplot() +
  aes(x = truth,
      y = pred, 
      ymin = lower,
      ymax = upper) +
  geom_ribbon(fill = "gray") +
  geom_point() 
       
lmfit <- lm(bloom_doy ~ tmax_avg + location, data = test)
summary(lmfit)

lmpre <- predict(lmfit)
sqrt(mean((lmpre - test$bloom_doy)^2))  #8.211 


test2 <- left_join(test, 
                   left_join(temp, conversion), 
                   by = c("location", "year"))

 
library(data.table)
variable_names <- names(test2)
setnames(test2, old = variable_names[grepl("^\\d", variable_names)],
         new = paste0("D", variable_names[grepl("^\\d", variable_names)]))

test2 <- test2 %>%
  select(-id)

library(randomForest)
set.seed(123)
test3 <- test2[test2$season == "Spring",]
test3_1 <- test3[test3$year < 2019 | test3$location == "vancouver",]
test3_2 <- test3[test3$year >= 2019 & test3$location != "vancouver",]
cherry_bag_new <- randomForest(bloom_doy ~ . , data = test3_1, mtyr = ncol(test2)-1, 
                               importance = TRUE, na.action = na.omit)
importance(cherry_bag_new)
varImpPlot(cherry_bag_new)
#in sample
rfpre_in <- predict(cherry_bag_new, newdata = test3_1, na.action = na.omit)
sqrt(mean((test3_1$bloom_doy - rfpre_in)^2, na.rm = TRUE))
#out of sample
rfpre_out <- predict(cherry_bag_new, newdata = test3_2, na.action = na.omit)
sqrt(mean((test3_2$bloom_doy - rfpre_out)^2, na.rm = TRUE))

library("stan4bart") 
fit2 <- stan4bart(bloom_doy ~ bart(. - location - tmin_avg - tmax_avg) + 
                   (1 + tmin_avg + tmax_avg | location),
                 data = na.omit(test3_1), 
                 bart_args = list(keepTrees = TRUE))
#in sample
bloom_doy_pred2_1 <- rowMeans(predict(fit2))
sqrt(mean((bloom_doy_pred2_1 - na.omit(test3_1)$bloom_doy)^2)) 

#out of sample
bloom_doy_pred2_2 <- rowMeans(predict(fit2, newdata = test3_2))
sqrt(mean((bloom_doy_pred2_2 - test3_2$bloom_doy)^2)) 

qplot(bloom_doy_pred2_1, na.omit(test3_1)$bloom_doy) + 
  labs(title = "Actual Bloom Dates vs Stan4Bart Predicted Bloom Dates", x = "Predicted Bloom Dates (Day of the Year)", y = "Actual Bloom Dates (Day of the Year)") +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(predicted, actual), color = "red",
              data = 
                tibble(actual = test3_2$bloom_doy,
                       predicted = bloom_doy_pred2_2)) +
  coord_flip()



predict(fit2)
#cores = 1, seed = 0,
#verbose = 1)

library(xlsx)
grr <- test3%>%filter(year == 2023)
write_excel_csv(grr, file = "data4predictions.csv")

some2024data <- temp%>%filter(year == 2024)
write_excel_csv(some2024data, file = "some2024data.csv")



#trying without precipitation
noprcp <- test3_1%>%select(-prcp_avg)

set.seed(123)
rfnoprcp <- randomForest(bloom_doy ~ . , data = noprcp, mtyr = ncol(noprcp)-1, 
                               importance = TRUE, na.action = na.omit)
sqrt(mean((predict(rfnoprcp) - na.omit(noprcp)$bloom_doy)^2)) 

urmom<-get_prcp("USC00305798")

library(readr)
data2024 <- read_excel("data2024.xlsx", skip = 1)
data2024$season <- as.factor(data2024$season)
data2024$bloom_doy <- as.numeric(data2024$bloom_doy)
data2024$bloom_doy <- 0


data2024DCKYLI <- data2024 %>% filter(location != "Newyork")

model <- stan4bart(bloom_doy ~ bart(. - location - tmin_avg - tmax_avg) + 
                    (1 + tmin_avg + tmax_avg | location),
                  data = na.omit(test3), 
                  bart_args = list(keepTrees = TRUE))

location <- data2024DCKYLI$location
prediction <- round(rowMeans(predict(model, newdata = data2024DCKYLI)), digits = 0)

lower <- round(apply(predict(model, newdata = data2024DCKYLI), 1, quantile, prob = .025), digits = 0)
upper <- round(apply(predict(model, newdata = data2024DCKYLI), 1, quantile, prob = .975), digits = 0)

submission <- data.frame(location, prediction, lower, upper)
submission <- rbind(submission, c("newyork", newyorkpre, nylower, nyupper))
submission <- submission %>% arrange(factor(location, 
                                            levels = c("washingtondc", "liestal"
                                                       ,"kyoto", "vancouver", "newyork")))

#dont run this yet
write.csv(submission, file = "cherrypredictions.csv", row.names = FALSE)

#newyork
newyork <- read_excel("newyorkprediction1.xlsx")
newyorkpre <- round(mean(rowMeans(predict(model, newdata = newyork))), digits = 0)
nylower <- round(quantile(colMeans(predict(model, newdata = newyork)), prob = .025), digits = 0)
nyupper <- round(quantile(colMeans(predict(model, newdata = newyork)), prob = .975), digits = 0)

rbind(c("newyork", newyorkpre, nylower, nyupper), submission)



#idk
bloom_doy_lower <- apply(predict(model), 1, quantile, prob = .025)
bloom_doy_upper <- apply(predict(model), 1, quantile, prob = .975)


