#Rscript --vanilla  plotter.r 2020-04-01 "Heckman, Emily","el-Mona, Shaheera" 4 "09:00:00" "17:00:00" "checkin_co" "output"

args = commandArgs(trailingOnly = TRUE)
cat(args[1], sep ='\n')
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
library(naniar)


# Connect to the default postgres database
con <- dbConnect(RPostgres::Postgres(),dbname = args[6], 
                 host = 'localhost',
                 port = 5432, 
                 user = 'postgres',
                 password = '1')
res <- dbSendQuery(con,"SELECT * FROM timeinfo")
dt <- data.frame(dbFetch(res))
con <- dbConnect(RPostgres::Postgres(),dbname = args[6], 
                 host = 'localhost', 
                 port = 5432, 
                 user = 'postgres',
                 password = '1')
work <- dbSendQuery(con,"SELECT * FROM shiftinfo")
dt2 <- data.frame(dbFetch(work))

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

d_time_in <- c("09:00:00")
d_time_out <- c("17:00:00")
print("data dt before mutate")
print(head(dt,6))
dt <- dt%>%
  mutate (expected_login = ifelse(is.na(expected_login),ifelse(is.na(args[4]),d_time_in, args[4]),as.character(expected_login))) %>%
  mutate (expected_logout = ifelse(is.na(expected_logout),ifelse(is.na(args[5]),d_time_out, args[5]),as.character(expected_logout))) %>% 
  mutate (expected_login = as_hms(expected_login), expected_logout = as_hms(expected_logout))%>%
  mutate(work_att1 = ifelse(expected_login-login>0,"ontime","late"),
         work_att2 = ifelse(expected_logout - logout <0,"ontime","left early"))%>%
  mutate(work_att = ifelse(work_att1=="ontime" & work_att2 == "ontime", "good", 
                           ifelse(work_att1=="late" & work_att2=="ontime","late", 
                                  ifelse(work_att1=="ontime" & work_att2=="left early",
                                         "left early","came late & left early"))))
print("dt after mutate everything")
print("\n")
print(head(dt,6))

#plot by date

temp1 <- dt %>% 
  gather(key = "type", value = "actual_shift", starts_with("log"))%>%
  select(-c("expected_login","expected_logout"))
print("temp1")
print(head(temp1,5))
temp2 <-dt%>%
  gather(key = "type", value = "assigned_shift", contains("expected"))%>%
  select(-c("login","logout"))%>%
  mutate(type = str_replace_all(type, "expected_",""))
print("temp2")
print(head(temp2,5))
dt2_new <-merge(x =temp1, y = temp2, by = c("userid","datework","employee","type"), all.x = TRUE)

print("dt2_new 1st")
print(head(dt2_new,5))
dt2_new <-dt2_new %>%
  select(-matches("\\.y$")) %>%
  rename_at(vars(ends_with(".x")),list(~str_replace(.,".x","")))
print("dt2_new after rename")
print(head(dt2_new,5))
#write_csv(dt2_new, "/Users/NT/Documents/Project/Samproj/dt2_newv2.csv")
#dt2v2 <- read.csv("/Users/NT/Documents/Project/Samproj/dt2_newv2.csv")
############## PLOT BY DATE###################3
#dt2_new[dt2_new$actual_shift=="","actual_shift"] <- NA

attitude <-function (x, y,z) {
  att = ifelse(z =="login" & x-y >0 ,"ontime",
               ifelse(z =="login" & x-y <0 ,"late",
                      ifelse(z =="logout" & x-y >0 ,"left early","ontime")))
  return (att)
}

dt2_new%>%
  filter(datework == args[1])%>%
  mutate (assigned_shift = ifelse(is.na(assigned_shift),ifelse(type =="login", 
                                                               ifelse(is.na(args[4]),d_time_in, as.character(args[4])),
                                                               ifelse(is.na(args[5]),d_time_out, as.character(args[5]))),assigned_shift))%>%
  mutate(assigned_shift = as_hms(assigned_shift))%>%
  ggplot+
  geom_point(aes(x = employee, y = assigned_shift, color = type, size =2))+
  geom_line(aes(x = employee, y = actual_shift, group = employee))+
  geom_point(aes(x = employee, y = actual_shift ),size =1)+
  scale_color_brewer(palette = "Dark2")+
  facet_wrap(~work_att)+
  coord_flip()+
  scale_y_time(breaks = seq(6*3600,22*3600,3600*2), labels = paste0(seq(6,22,2)),
               minor_breaks = seq(6*3600,22*3600,3600))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(color = "grey", size = 0.5, linetype ="dotted"),
        panel.border = element_rect(fill = "NA",color = "black"), axis.text.x = element_text(angle = 0))+
  ylab("hour")

ggsave(filename = paste0(as.character(args[7]),"_1",".png")) 
#ggsave("/Users/NT/Documents/Project/Samproj/plotbydate2.pdf")

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


dt2_new %>%
  filter(employee %in% z)%>%
  filter(month(datework)  == args[3])%>%
  mutate (assigned_shift = ifelse(is.na(assigned_shift),ifelse(type =="login", 
                                                               ifelse(is.na(args[4]),d_time_in, as.character(args[4])),
                                                               ifelse(is.na(args[5]),d_time_out, as.character(args[5]))),as.character(assigned_shift)))%>%
  mutate(assigned_shift = as_hms(assigned_shift))%>%
  ggplot+
  geom_line(aes(x = datework, y = assigned_shift, group = datework),color = "#CDCDCD", size = 4)+
  geom_line(aes(x = datework, y = actual_shift, group = datework, color = work_att), size =1)+
  geom_point(aes(x = datework, y = actual_shift))+
  scale_y_time(breaks =seq(3600,23*3600, 3600),  name ="hours", labels = c(paste0(c(1:12),":00","AM"),paste0(c(1:11),":00","PM")))+
  scale_x_date(breaks = "3 days", date_labels = "%a-%e")+
  theme(axis.text.x = element_text(angle = 0))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey", size = 0.5, linetype ="dotted"),
        panel.grid.major.x = element_line(color = "grey", size = 0.5, linetype ="dotted"))+
  scale_color_discrete(name = "work attitude")

ggsave(filename = paste0(as.character(args[7]),"_2",".png"))


#late plot by diff time in one month with heat map
dt%>%
  mutate(time_in = as.numeric(difftime(expected_login, login), units = "mins"))%>%
  mutate(time_out = as.numeric(difftime(expected_logout, logout), units = "mins"))%>%
  filter(month(datework)  == args[3])%>%
  ggplot()+
  geom_tile(aes(x = datework, y = employee, fill = time_in))+
  scale_fill_distiller(name = "check in duration",palette = "RdPu") +
  scale_x_date(breaks = "1 week", date_labels = "%a-%e")+
  theme(axis.text.x = element_text(angle = 0),
        panel.background = element_rect(fill = "white"))
ggsave(filename = paste0(as.character(args[7]),"_3",".png"))

#early leaving plot by diff time in one month with heat map
#scale_fill_gradient(low="white", high="blue") 
dt%>%
  mutate(time_in = as.numeric(difftime(expected_login, login), units = "mins"))%>%
  mutate(time_out = as.numeric(difftime(expected_logout, logout), units = "mins"))%>%
  filter(month(datework)  == args[3])%>%
  ggplot()+
  geom_tile(aes(x = datework, y = employee, fill = time_out))+
  scale_fill_distiller(name = "check out duration",palette = "RdPu") + #RdPu
  scale_x_date(breaks = "4 days", date_labels = "%a-%e")+
  theme(
    panel.background = element_rect(fill = "white"))+
ggsave(filename = paste0(as.character(args[7]),"_4",".png"))

dt%>%
  filter(month(datework)  == args[3])%>%
  ggplot()+
  geom_tile(aes(x = datework, y = employee, fill = worktime2))+
  scale_fill_distiller(name = "total work hours",palette = "Oranges") + #RdPu
  scale_x_date(breaks = "7 days", date_labels = "%a-%e")+
  theme(
    panel.background = element_rect(fill = "white"))

ggsave(filename = paste0(as.character(args[7]),"_5",".png"))





