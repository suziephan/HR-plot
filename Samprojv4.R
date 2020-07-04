#Rscript --vanilla  Samprojv3.R 2020-04-01 "Heckman, Emily","el-Mona, Shaheera" 4

args = commandArgs(trailingOnly = TRUE)
cat(args[2], sep ='\n')
#install.packages()

library(dplyr)
library(DBI)
library(lubridate)
library(stringr)
library(ggplot2)
library(chron)
library(hms)
library(ggrepel)
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(echarts4r)
library(echarts4r.assets)
library(grid)
library(cowplot)
library(magick)

# Connect to the default postgres database
con <- dbConnect(RPostgres::Postgres(),dbname = 'checkin_co', 
                 host = 'localhost', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = 'postgres',
                 password = '1')
res <- dbSendQuery(con,"SELECT * FROM timeinfo")
dt <- data.frame(dbFetch(res))
con <- dbConnect(RPostgres::Postgres(),dbname = 'checkin_co', 
                 host = 'localhost', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 5432, # or any other port specified by your DBA
                 user = 'postgres',
                 password = '1')
work <- dbSendQuery(con,"SELECT * FROM shiftinfo")
dt2 <- data.frame(dbFetch(work))

dt[duplicated(dt),]


spread_error <- function (dt) {
   x<-dt%>%filter(dt$type =="login")
   y<- dt%>%filter(dt$type =="logout")
   dt <-merge(x, y , by =c("datework","employee"), all.x = TRUE)
   ifelse(identical(dt$user_id.x, dt$user_id.y),dt1 <-dt[c(3,1,2,5,8)],dt1<-"NAN")
  return (dt1)
}
dt1<- spread_error(dt)
colnames(dt1) <- c("userid","datework","employee","login","logout")
dt<-merge(x = dt1, y = dt2, by =c("userid","datework","employee"), all.x= TRUE)

dt$workday <-substr(weekdays(dt$datework),1,3)

dt$"worktime" <- seconds_to_period(difftime(dt$logout, dt$login,units = "secs"))
dt$worktime <- hms::hms(dt$worktime)
worktime_cv <- function (z ) round(hour(z)+minute(z)/60 + second(z)/3600,1)
dt$"worktime2" <- worktime_cv(dt$worktime)


#plot by date

temp1 <- dt %>% 
  gather(key = "type", value = "actual_shift", starts_with("log"))%>%
  select(-c("expected_login","expected_logout"))
temp2 <-dt%>%
  gather(key = "type", value = "assigned_shift", contains("expected"))%>%
  select(-c("login","logout"))%>%
  mutate(type = str_replace_all(type, "expected_",""))

dt2_new <-merge(x =temp1, y = temp2, by = c("userid","datework","employee","type"), all.x = TRUE)
dt2_new <- dt2_new%>%
  select(-c("workday.y","worktime.y","worktime2.y"))
            #"work_att1.y",#"work_att2.y",#"work_att.y"

colnames(dt2_new) <- c("userid","datework","employee", "type",         
                       "workday","worktime", "worktime2", "actual_shift", "assigned_shift")
                       #"work_att1", "work_att2","work_att"

#write_csv(dt2_new, "/Users/NT/Documents/Project/Samproj/dt2_new.csv")
############## PLOT BY DATE###################3
#dt2_new[dt2_new$actual_shift=="","actual_shift"] <- NA

attitude <-function (x, y,z) {
  att = ifelse(z =="login" & x-y >0 ,"ontime",
               ifelse(z =="login" & x-y <0 ,"late",
                      ifelse(z =="logout" & x-y >0 ,"left early","ontime")))
  return (att)
}

#print(class(chron (times = args[4])))
d_time_in <- c("09:00:00")
d_time_out <- c("17:00:00")


dt2_new%>%
  filter(datework == args[1])%>%
  mutate (assigned_shift = ifelse(is.na(assigned_shift),ifelse(type =="login", 
                                  ifelse(is.na(args[4]),d_time_in, args[4]),
                                  ifelse(is.na(args[5]),d_time_out, args[5])),assigned_shift))%>%
  mutate(assigned_shift = as_hms(assigned_shift))%>%
  mutate(work_att1 = attitude(assigned_shift,actual_shift,type)) %>%
  ggplot+
  geom_point(aes(x = employee, y = assigned_shift, color = type, size =2))+
  geom_line(aes(x = employee, y = actual_shift, group = employee))+
  geom_point(aes(x = employee, y = actual_shift ),size =1)+
  scale_color_brewer(palette = "Dark2")+
  coord_flip()+
  scale_y_time(breaks = seq(6*3600,22*3600,3600), labels = paste0(c(6:22)))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(color = "grey", size = 0.5, linetype ="dotted"),
        panel.border = element_rect(fill = "NA",color = "black"), axis.text.x = element_text(angle = 0))

ggsave("/Users/NT/Documents/Project/Samproj/plotbydate2.pdf")

#plot by employee by month
x<- args[2]
vector_convert <- function (x) {
  y <-unlist(strsplit(x, ","))
  list1 <- y[seq(1,length(y),2)]
  list2 <- y[seq(2,length(y),2)]
  z <- mapply(paste0,list1,",",list2)
  return  (z) 
}

z <- vector_convert(x) 
z
dt2_new %>%
  filter(employee %in% z)%>%
  filter(month(datework)  == args[3])%>%
  mutate (assigned_shift = ifelse(type =="login", 
                                  ifelse(is.na(args[4]),d_time_in, args[4]),
                                  ifelse(is.na(args[5]),d_time_out, args[5])))%>%
  mutate(assigned_shift = as_hms(assigned_shift))%>%
  mutate(work_att = attitude(assigned_shift,actual_shift,type)) %>%
  ggplot+
  geom_line(aes(x = datework, y = assigned_shift, group = datework),color = "#CDCDCD", size = 4)+
  geom_line(aes(x = datework, y = actual_shift, group = datework, color = work_att), size =1)+
  geom_point(aes(x = datework, y = actual_shift))+
  scale_y_time(breaks =seq(3600,23*3600, 3600),  name ="hours", labels = c(paste0(c(1:12),":00","AM"),paste0(c(1:11),":00","PM")))+
  scale_x_date(breaks = "3 days", date_labels = "%a-%e")+
  theme(axis.text.x = element_text(angle = 0))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey", size = 0.5, linetype ="dotted"),
        panel.grid.major.x = element_line(color = "grey", size = 0.5, linetype ="dotted"))

ggsave("/Users/NT/Documents/Project/Samproj/plotbyemployee3.pdf")


#late plot by diff time in one month with heat map
dt%>%
  mutate (expected_login = ifelse(is.na(expected_login),ifelse(is.na(args[4]),d_time_in, args[4]),as.character(expected_login))) %>%
  mutate (expected_logout = ifelse(is.na(expected_logout),ifelse(is.na(args[5]),d_time_out, args[5]), as.character(expected_logout)))%>%
  mutate (expected_login = as_hms(expected_login), expected_logout = as_hms(expected_logout))%>%
  mutate(time_in = as.numeric(difftime(expected_login, login), units = "mins"))%>%
  mutate(time_out = as.numeric(difftime(expected_logout, logout), units = "mins"))%>%
  filter(month(datework)  == args[3])%>%
  ggplot()+
  geom_tile(aes(x = datework, y = employee, fill = time_in))+
  scale_fill_distiller(palette = "RdPu") +
  scale_x_date(breaks = "1 week", date_labels = "%a-%e")+
  theme(axis.text.x = element_text(angle = 0),
        panel.background = element_rect(fill = "white"))
ggsave("/Users/NT/Documents/Project/Samproj/lateplot5.pdf")

#early leaving plot by diff time in one month with heat map
#scale_fill_gradient(low="white", high="blue") 
dt%>%
  mutate(time_in = as.numeric(difftime(expected_login, login), units = "mins"))%>%
  mutate(time_out = as.numeric(difftime(expected_logout, logout), units = "mins"))%>%
  filter(month(datework)  == args[3])%>%
  mutate (assigned_shift = ifelse(type =="login", 
                                  ifelse(is.na(args[4]),d_time_in, args[4]),
                                  ifelse(is.na(args[5]),d_time_out, args[5])))%>%
  mutate(assigned_shift = as_hms(assigned_shift))%>%
  mutate(work_att = attitude(assigned_shift,actual_shift,type)) %>%
  ggplot()+
  geom_tile(aes(x = datework, y = employee, fill = time_out))+
  scale_fill_distiller(palette = "RdPu") + #RdPu
  scale_x_date(breaks = "1 day", date_labels = "%a-%e")+
  theme(
        panel.background = element_rect(fill = "white"))
ggsave("/Users/NT/Documents/Project/Samproj/heatmapo6.pdf") 


#write_csv(df,"/Users/NT/Documents/Project/Samproj/samproj.csv")




