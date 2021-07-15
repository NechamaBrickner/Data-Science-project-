
# Data Science class
# project  - Hurricane 

#####################################

#part 0 load packages

library(tidyverse)
library(Hmisc)
library(ggcorrplot)
library(viridis)
library(sf)
library(latticeExtra)
library(gganimate)
library(gifski)

####################################

# part I arranging the data

# set working directory 
#setwd("C:\\Users\\anyg1\\Documents\\Nechama's Docs\\BGU\\courses with english file\\data and spatial data management\\finale project\\final project idea hurricane\\new_hurricane data\\raw")

#loading the raw data frames and cleaning them to fit our needs 

# hurricane data 
Hurricane = as.tibble(read.csv("ibtracs.ALL.list.v04r00_1.csv"))

Hurricane1970_2020_new = Hurricane %>%
  select(SID:LON, DIST2LAND, LANDFALL, USA_LAT, USA_LON, USA_STATUS:USA_SSHS, STORM_SPEED, STORM_DIR) %>%
  filter(BASIN == "NAT" & SEASON >= 1970 & NAME != "NOT_NAMED") %>%
  mutate(ISO_TIME2 = ISO_TIME) %>%
  separate(ISO_TIME2, into = c ("day", "month", "year_time"), sep = "/", convert = T) %>%
  separate(year_time, into = c ("year", "time"), sep = 4, convert = T) %>%
  separate(time, into = c("hour", "min"), sep = ":", convert = T) %>%
  select(-min, -SEASON, -NUMBER) %>%
  filter(hour == 0 | hour == 3 | hour == 6 | hour == 9 | hour == 12 | hour == 15 | hour == 18 | hour == 21)
# selecting the columns that we need, and filtering the data to get the hurricanes in the north Atlantic basin 
# from the year 1970 forward all with names (getting rid of all the not_named).
# coping the ISO_TIME column to separate it in to year, month, day, hour, and min columns 
# so that they can be used for joins. 
# getting rid of some columns that aren't needed. 
# filtering the data to get the hours every 3 hours.


#annual avg sea surface temp anomaly
annual_avg_ss_temp_anomaly = as.tibble(read.csv("annual-average-sea-surface-temperature-6.csv"))

annual_avg_ss_temp_anomaly = annual_avg_ss_temp_anomaly %>%
  select(year.year, Global.ocean.number, North.Atlantic.number)%>%
  filter(year.year >= 1970)%>%
  rename(year = year.year, Global_ocean_SST_anom = Global.ocean.number, North_Atlantic_SST_anom = North.Atlantic.number)%>%
  group_by(year)%>%  
  summarise(avg_Global_ocean_SST_anom = mean(Global_ocean_SST_anom), North_Atlantic_SST_anom = mean(North_Atlantic_SST_anom)) 
# selecting the columns that we need, and filtering the data to get the observations from 1970 and forward.
# renaming the columns so its easier to work with.
# each year has 3 observation so to make it 1 observation per year using group and summarize we can get 
# the average sea surface anomaly temperature per year

#global monthly temp anomaly
global_monthly_temp_anomaly = as.tibble(read.csv("monthly_csv_global_temp_anoamly.csv"))

global_monthly_temp_anomaly = global_monthly_temp_anomaly %>%
  spread(key = Source, value = Mean) %>%
  separate(Date, into = c("year", "month", "day"), sep = "-", convert = T) %>%
  filter( year >= 1970) %>%
  select(-day, -GCAG)
# the data is not tidy it needs to be spreed into 2 columns using the source column as the key and the mean column as the value.
# the date column need to be separated into year, month, day so that it can be filtered by year and used for joins.
# filtering the data to get the observations from 1970 forward.
# getting rid of 2 columns that aren't needed.

# north atlantic oscillation 
NAO = read.csv("NAO_org.csv")

NAO = NAO %>%
  separate(date, into = c("year", "month"), sep = 4) %>%
  filter(year >= 1970)
# separating the date column to year and month so the data can be filtered by year and used for joins.
# filtering the data to get the observations from 1970 forward.

# Co2 annual avg ppm
co2_annual_avg_ppm = read_csv("co2_annual_avg.csv")

co2_annual_avg_ppm = co2_annual_avg_ppm %>%
  filter(Year >= 1970)%>%
  select(-Entity, -Code) %>%
  rename(CO2_avg_annual = "CO2 concentrations (NOAA, 2018)")
# filtering the data to get the observations from 1970 forward.
# getting rid of 2 columns that aren't needed.


# atlantic multidecadal oscillation
AMO = read_csv("amo_d.csv") 

AMO = AMO %>%
  gather(2:13, key = "month", value = "AMO")
# gather columns 2:13 (each column is a month first is 1-jan, second is 2-feb...) the key is month and the value is AMO.


# set working directory 
setwd("C:\\Users\\anyg1\\Documents\\Nechama's Docs\\BGU\\courses with english file\\data and spatial data management\\finale project\\final project idea hurricane\\new_hurricane data\\procesed")

# save all the cleaned up tables in the processed file.
write.csv(Hurricane1970_2020_new, "Hurricane1970_2020.csv")
write.csv(annual_avg_ss_temp_anomaly, "annual_avg_ss_temp_anomaly.csv")
write.csv(global_monthly_temp_anomaly, "global_monthly_temp_anomaly.csv")
write.csv(NAO, "NAO.csv")
write.csv(co2_annual_avg_ppm, "co2_annual_avg_ppm.csv")
write.csv(AMO, "AMO.csv")

### using Arcmap we connected the bathymetric data to the Hurricane1970_2020_new table using the Extract Values to Points tool.

#### from now on import the cleaned up tables!!

#Hurricane
Hurricane1970_2020_new = read_csv("hurricane_1970-2020_+bathymetry.csv")

#annual avg sea surface temp anomaly
annual_avg_ss_temp_anomaly = read_csv("annual_avg_ss_temp_anomaly.csv")

#global monthly temp anomaly
global_monthly_temp_anomaly = read_csv("global_monthly_temp_anomaly.csv")

#north atlantic oscillation 
NAO = read_csv("NAO.csv")

# Co2 annual avg ppm
co2_annual_avg_ppm = read_csv("co2_annual_avg_ppm.csv")

# atlantic multidecadal oscillation
AMO = read_csv("AMO.csv")

# to join the tables together we need to know the column names. 
colnames(Hurricane1970_2020_new)
colnames(annual_avg_ss_temp_anomaly)
colnames(global_monthly_temp_anomaly)
colnames(NAO)
colnames(co2_annual_avg_ppm)
colnames(AMO)


#full join to save all rows

join1 = annual_avg_ss_temp_anomaly %>%
  full_join(global_monthly_temp_anomaly, by = "year")

join1 = join1 %>%
  full_join(co2_annual_avg_ppm, by = c("year" = "Year"))

NAO$month = as.numeric(NAO$month)

join2 = NAO %>%
  full_join(AMO, by = c("year", "month"))

join3 = join1 %>%
  full_join(join2, by = c("year", "month"))

#in this join we will use a left joim to keep only the rows that match the hurricanes
joinall = Hurricane1970_2020_new %>%
  left_join(join3, by = c("year", "month"))

#after joining all 5 tables together we need to know the column names to clean it up.
colnames(joinall)

Hurricane1970_2020_finale = joinall%>%
  select(-X1.x.x, -X1.y.x, -X1.x.y, -X1.y.y, -X1, -FID, -Field1) %>%
  rename(depth = RASTERVALU) %>%
  select(SID, ISO_TIME, year, month, day, hour, NAME, BASIN:depth, everything()) %>%
  arrange(year, month, day, hour)
# getting rid of columns that aren't needed.
# renaming the rastervalu to depth.
# arranging the table using select so the columns are in order.
# arranging the observations in chronological order using year, month, day and hour.

#saving the finale table to the prossesed file.
write.csv(Hurricane1970_2020_finale, "Hurricane1970_2020_finale.csv")

# remove all the other tables from the "workplace" 
rm(AMO, annual_avg_ss_temp_anomaly, co2_annual_avg_ppm, global_monthly_temp_anomaly, 
   Hurricane1970_2020_new, join1, join2, join3, joinall, NAO)

##################################################

# part II statistical analysis 

# set working directory 
#setwd("C:\\Users\\anyg1\\Documents\\Nechama's Docs\\BGU\\courses with english file\\data and spatial data management\\finale project\\final project idea hurricane\\new_hurricane data\\procesed")

  #load finale table
  Hurricane1970_2020_finale = read_csv("Hurricane1970_2020_finale.csv")
  Hurricane1970_2020_finale = Hurricane1970_2020_finale %>%
    filter(year < 2015)
  
  # to get more information were are going to make 2 more tables 
  # the first hurricanes catagory 4 & 5
  # the second the min max and avg of all the numeric columns 
  # grouped by the year and name to get the min max and avg
  # value per hurricane 
  
  #hurricanes level 4 & 5
  Hurricane_level_4_5 = Hurricane1970_2020_finale %>%
    filter(USA_SSHS == 5 | USA_SSHS == 4) %>%
    group_by(year, NAME) %>%
    count() %>%
    select(-n) %>%
    left_join(Hurricane1970_2020_finale, by = c("year", "NAME"))
  
  write.csv(Hurricane_level_4_5, "Hurricane_level_4_5.csv")
  
  #summarizes min avg max all in 1 table
  Hurricane1970_2020_finale_summary = Hurricane1970_2020_finale %>%
    select(4:8, 14, 15, 19:30)%>%
    group_by(year, NAME) %>%
    summarise_all(list(min = min, mean = mean, max = max), na.rm = T)
  
  write.csv(Hurricane1970_2020_finale_summary,
            "Hurricane1970_2020_finale_summary.csv")


  # because there are more than 60 rows the data doesn't need 
  # to be normally distributed also there are more than 
  # 5000 rows Shapiro test can't be used
  
  # cheek correlation between the numeric columns 
  cor1 = rcorr(as.matrix(Hurricane1970_2020_finale[ ,c(14,15,19:30)])) 
  cor1_df = as.data.frame(cor1$r)
  p_val_df = as.data.frame(cor1$P)
  
  # multiple linear regression
  fit = lm(USA_SSHS ~ depth + 
             avg_Global_ocean_SST_anom + 
             North_Atlantic_SST_anom + 
             GISTEMP + CO2_avg_annual + nao + AMO,
            data = Hurricane1970_2020_finale)
  summary(fit)
  
  
  # multiple linear regression with the summery table
  fit2 = lm(USA_SSHS_max ~ depth_min + avg_Global_ocean_SST_anom_max +
              North_Atlantic_SST_anom_max +GISTEMP_max + 
              CO2_avg_annual_max + nao_max + AMO_max, 
              data = Hurricane1970_2020_finale_summary) 
  summary(fit2)

# chi squared test 
# 1
chisq.test(Hurricane1970_2020_finale_summary$USA_SSHS_max,
           Hurricane1970_2020_finale_summary$avg_Global_ocean_SST_anom_max)

# 2
chisq.test(Hurricane1970_2020_finale_summary$USA_SSHS_max,
           Hurricane1970_2020_finale_summary$North_Atlantic_SST_anom_max)

# 3
chisq.test(Hurricane1970_2020_finale_summary$USA_SSHS_max,
           Hurricane1970_2020_finale_summary$CO2_avg_annual_max)

# 4
chisq.test(Hurricane1970_2020_finale_summary$USA_SSHS_max,
           Hurricane1970_2020_finale_summary$GISTEMP_max)

# 5
chisq.test(Hurricane1970_2020_finale_summary$USA_SSHS_max,
           Hurricane1970_2020_finale_summary$nao_max)

# 6
chisq.test(Hurricane1970_2020_finale_summary$USA_SSHS_max,
           Hurricane1970_2020_finale_summary$AMO_max)

# 7
chisq.test(Hurricane1970_2020_finale_summary$CO2_avg_annual_max,
           Hurricane1970_2020_finale_summary$AMO_max)

###########################################

#plots

#setwd("C:\\Users\\anyg1\\Documents\\Nechama's Docs\\BGU\\courses with english file\\data and spatial data management\\finale project\\final project idea hurricane\\new_hurricane data\\plots")

hurricane_by_year = Hurricane1970_2020_finale %>%
  group_by(year)%>%
  count(NAME) %>%
  group_by(year) %>%
  count()
# number of hurricane a year
pdf("nunber of hurricanes a year.pdf")
hurricane_by_year %>%
  ggplot(aes(year, n)) +
  geom_point(col= "darkturquoise") +
  labs(title = "Number of hurricanes per year",
       x = "Year",
       y = "Number of hurricanes")+
  geom_smooth(method = lm, color = "maroon3")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off

# count by month
pdf("sum_hurricane_By_month.pdf", 
    width = 9, height = 7)
Hurricane1970_2020_finale_summary %>%
  ggplot(aes(x= as.factor(month_max))) +
  geom_bar(aes(fill=as.factor(USA_SSHS_max)),position = "dodge")+
  scale_fill_viridis(discrete = T, option = "D")+
  labs(
    title = "Sum of hurricanes by Catagory in each month",
    subtitle = "hurricanes from 1970-2014",
    x = "Month",
    y = "Sum of hurricanes by Catagory",
    fill = "Hurricane Catagory")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
dev.off()

# density plot of each Hurricane intensity over the years
pdf("density.pdf", 
    width = 9, height = 7)
Hurricane1970_2020_finale_summary %>%
  filter(USA_SSHS_max > (-3)) %>%
  ggplot(aes(x = year,group = as.factor(USA_SSHS_max), fill = as.factor(USA_SSHS_max)))+
  geom_density(alpha = 0.4)+
  facet_wrap(~ USA_SSHS_max)+
  labs(title = "Density of hurricanes by Catagory over the years 1970-2014", 
       x = "year",
       y = "Density",
       fill = "Hurricane Catagory")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

# usual line chart
obj1 <- xyplot(AMO_max ~ year, Hurricane1970_2020_finale_summary, type = "l", lwd=2, ylab = "AMO", xlab = "Year")
obj2 <- xyplot(CO2_avg_annual_max ~ year, Hurricane1970_2020_finale_summary, type = "l", lwd=2, ylab = "Co2 annual mean")
# graph 2 lines AMO & Co2
pdf("CO2_AMO.pdf", 
    width = 9, height = 7)
doubleYScale(obj1, obj2, add.ylab2 = TRUE)
dev.off()

#correlation
pdf("cor1.pdf", 
    width = 9, height = 7)
ggcorrplot(cor1_df, hc.order = TRUE, 
           type = "lower",
           lab = TRUE, 
           lab_size = 2, 
           method="circle",
           colors = c("maroon3", "lavender",
                      "darkturquoise"),
           ggtheme = theme_gray())
dev.off()


#################################################################

###gifs

shp = st_read("dissolve.shp")
shp_df = fortify(shp)

# gif of hurricanes category 4 & 5 by year from 1971-2014
gif_by_year= Hurricane_level_4_5 %>%
  group_by(year,NAME)%>%
  ggplot()+
  geom_sf(data = shp, color= "grey")+
  geom_point(aes(x = LON, y = LAT, color = USA_SSHS)) +
  labs( title = "Hurricanes with max category 4 and 5 paths (1971-2014)",
        color = "Hurricane Catagory")+
  theme(plot.title = element_text(hjust = 0.5))+
  transition_states(year, transition_length = 1, state_length = 1)
animate(gif_by_year, duration = 20, fps = 5, render = gifski_renderer())
anim_save("gif_by_year.gif", animation = last_animation())




