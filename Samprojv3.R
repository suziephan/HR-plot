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
#write.csv(dt,"/Users/NT/Documents/Project/samproj.csv")

dt<-merge(x = dt, y = dt2, by =c("userid","datework","employee"), all.x= TRUE)
dt<-dt[!is.na(dt$workday),]


#dt$"logindiff" <- difftime(dt$login, dt$expected_login, units = "secs")

#td <- seconds_to_period(dt$logindiff)
#dt$logindiff <- sprintf('%02d:%02d:%02d',  td@hour, minute(td), second(td))

#dt$"logoutdiff" <- difftime(dt$logout, dt$expected_logout,units = "secs")

#td1 <- seconds_to_period(dt$logoutdiff)
#dt$logoutdiff <- sprintf('%02d:%02d:%02d',  td1@hour, minute(td1), second(td1))

dt$"worktime" <- seconds_to_period(difftime(dt$logout, dt$login,units = "secs"))
dt$worktime <- hms::hms(dt$worktime)
#dt$worktime <- sprintf('%02d:%02d:%02d',  dt$worktime@hour, minute(dt$worktime), second(dt$worktime))
worktime_cv <- function (z ) round(hour(z)+minute(z)/60 + second(z)/3600,1)
dt$"worktime2" <- worktime_cv(dt$worktime)

dt<- dt%>%
  mutate(work_att1 = ifelse(expected_login-login>0,"ontime","late"),
         work_att2 = ifelse(expected_logout - logout <0,"ontime","left early"))%>%
  mutate(work_att = ifelse(work_att1=="ontime" & work_att2 == "ontime", "good", 
                           ifelse(work_att1=="late" & work_att2=="ontime","late", 
                                  ifelse(work_att1=="ontime" & work_att2=="left early","left early","came late & left early"
                                  ))))

unique(dt$work_att)
#plot by date

temp1<-dt%>%
  gather(key = "type", value = "hour", contains("log"))%>%
  filter(grepl("login", type))%>%
  spread(type,hour)%>%
  rename(assigned_shift=expected_login,actual_shift=login)%>%
  mutate(type = "login" )
  
temp2<-dt%>%
  gather(key = "type", value = "hour", contains("log"))%>%
  filter(grepl("logout", type))%>%
  spread(type,hour)%>%
  rename(assigned_shift=expected_logout,actual_shift=logout)%>%
  mutate(type = "logout" )

dt2_new <- rbind(temp1, temp2)
dt2_new$employee <- factor(dt2_new$employee, levels = unique(dt2_new[order(dt2_new$work_att),"employee"]))

write_csv(dt2_new, "/Users/NT/Documents/Project/Samproj/dt2_new.csv")
############## PLOT BY DATE###################3
#dt2_new[dt2_new$actual_shift=="","actual_shift"] <- NA

dt2_f <- dt2_new%>%
  filter(datework == args[1])%>%
  select(employee,work_att, assigned_shift, actual_shift, type, workday)


name_factor <- unique(dt2_f[order(dt2_f$work_att),"employee"])


dt2_new%>%
  filter(datework  == as.Date(args[1]))%>%
  select(employee,work_att, assigned_shift, actual_shift, type, workday)%>%
  mutate(employee = factor(employee, levels = name_factor))%>%
  ggplot+
  geom_line(aes(x = employee, y = actual_shift, color = work_att))+
  geom_point(aes(x = employee, y = actual_shift ),size =1)+
  geom_point(aes(x = employee, y = assigned_shift, shape= type), size =2)+
  coord_flip()+
  theme(panel.background = element_blank(), panel.grid = element_blank())+
  scale_y_time(breaks = seq(6*3600,22*3600,3600), labels = paste0(c(6:22),":00"))

#ggsave("/Users/NT/Documents/Project/Samproj/plotbydate1.pdf")

dt2_new%>%
  filter(datework ==args[1])%>%
  ggplot+
  geom_line(aes(x = employee, y = actual_shift, color = work_att))+
  geom_point(aes(x = employee, y = actual_shift ),size =1)+
  geom_point(aes(x = employee, y = assigned_shift, shape= type), size =2)+
  coord_flip()+
  scale_y_time(breaks = seq(6*3600,22*3600,3600), labels = paste0(c(6:22)))+
  facet_grid(~ work_att)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(color = "grey", size = 0.5, linetype ="dotted"),
        panel.border = element_rect(fill = "NA",color = "black"), axis.text.x = element_text(angle = 45))
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
  ggplot+
  geom_line(aes(x = datework, y = assigned_shift, group = datework),color = "#CDCDCD", size = 4)+
  geom_line(aes(x = datework, y = actual_shift, group = datework, color = work_att), size =1)+
  geom_point(aes(x = datework, y = actual_shift))+
  scale_y_time(breaks =seq(3600,23*3600, 3600),  name ="hours", labels = c(paste0(c(1:12),":00","AM"),paste0(c(1:11),":00","PM")))+
  scale_x_date(breaks = "1 day", date_labels = "%a-%e")+
  theme(axis.text.x = element_text(angle = 45))+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey", size = 0.5, linetype ="dotted"),
        panel.grid.major.x = element_line(color = "grey", size = 0.5, linetype ="dotted"))


ggsave("/Users/NT/Documents/Project/Samproj/plotbyemployee3.pdf")
#count plot by employee per every month 
dt2_new %>%
  mutate(working_month = month(datework)) %>%
  group_by(working_month)%>%
  filter(employee %in% z)%>%
  group_by(employee, working_month)%>%
  count(work_att)%>%
  ggplot+
  geom_col(aes(x = working_month, y = n, fill = work_att),position=position_dodge() )+
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank())+
  geom_text_repel(aes(x = working_month, y = n, label = n), position =  position_dodge2(width = 0.9),
                  hjust  = 0.1, vjust = -0.8)+
  facet_wrap(~ employee)+
  labs(title = paste0("Working attitude of ", args[2]))
  
ggsave("/Users/NT/Documents/Project/Samproj/countplot4.pdf")
                 
#late plot by diff time in one month with heat map
dt%>%
  mutate(time_in = as.numeric(difftime(expected_login, login), units = "mins"))%>%
  mutate(time_out = as.numeric(difftime(expected_logout, logout), units = "mins"))%>%
  filter(month(datework)  == args[3])%>%
  ggplot()+
  geom_tile(aes(x = datework, y = employee, fill = time_in))+
  scale_fill_distiller(palette = "RdPu") +
  scale_x_date(breaks = "1 day", date_labels = "%a-%e")+
  theme(axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "white"))
ggsave("/Users/NT/Documents/Project/Samproj/lateplot5.pdf")

#early leaving plot by diff time in one month with heat map
#scale_fill_gradient(low="white", high="blue") 
dt%>%
  mutate(time_in = as.numeric(difftime(expected_login, login), units = "mins"))%>%
  mutate(time_out = as.numeric(difftime(expected_logout, logout), units = "mins"))%>%
  filter(month(datework)  == args[3])%>%
  ggplot()+
  geom_tile(aes(x = datework, y = employee, fill = time_out))+
  scale_fill_distiller(palette = "RdPu") + #RdPu
  scale_x_date(breaks = "1 day", date_labels = "%a-%e")+
  theme(axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "white"))
ggsave("/Users/NT/Documents/Project/Samproj/heatmapo6.pdf") 

#compare number of time lates between employee in 1 month
dt%>%
  mutate(working_month = month(datework)) %>%
  filter(working_month  == args[3])%>%
  group_by(employee)%>%
  count(work_att1)%>%
  ggplot()+
  geom_col(aes(x = employee, y = n, fill = work_att1))+
  facet_wrap(~work_att1)+
  theme(axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "white"))+
  geom_text(aes(x = employee, y = n, label =n), vjust = -0.2)
ggsave("/Users/NT/Documents/Project/Samproj/heatmapin7.pdf")

# make color with column plot and e_pictorial for late
sad_face <- "https://1.bp.blogspot.com/-klwxpFekdEQ/XOubIhkalyI/AAAAAAAAHlE/25psl9x4oNkbJoLc2CKTXgV2pEj6tAvigCLcBGAs/s1600/pencil.png"
#symbol = paste0("image://",sad_face
dt%>%
  mutate(working_month = month(datework)) %>%
  filter(working_month  == args[3])%>%
  group_by(employee)%>%
  count(work_att1)%>%
  filter(work_att1 == "late")%>%
  e_charts(employee)%>%
  e_pictorial(n, symbol ="rect" ,symbolRepeat = FALSE,z = -1)%>%
  e_theme("westeros")%>%
  e_labels(fontSize =16, fontWeight = "bold", position = "right", offset=c(4,0))

#Pie Clock per person in one month

clock <- png::readPNG("/Users/NT/Downloads/clock3.png")

myplot<-dt2_new%>% 
  mutate(working_month = month(datework)) %>%
  filter(working_month  == 3)%>%
  filter(employee  %in% z)%>%
  group_by(employee)%>%
  count(work_att1)%>%
  mutate (percent = round(n/sum(n)*100,0))%>%
  ggplot(aes(x = "", y = n, fill = work_att1))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0, direction = 1)+
  facet_grid(~employee)+
  theme(axis.line = element_line(), axis.title = element_blank())+
  theme(panel.background = element_rect(fill = "transparent", colour = NA),  
       plot.background = element_rect(fill = "transparent", colour = NA))+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "top")+
  geom_text(aes( y = n, 
                label = paste0(percent,"%")), size=5, position = position_stack(vjust = 0.5))
myplot
p3 <- ggdraw()+draw_image(clock)

aligned_plots1 <- align_plots(myplot, p4, align="hv", axis="tblr", greedy = TRUE)

ggdraw(aligned_plots1[[1]])+draw_plot(aligned_plots1[[2]])

ggsave("/Users/NT/Documents/Project/Samproj/pieclock8.pdf")

p4 <-plot_grid(p3,p3,p3, align = "h", scale =1 , ncol = 3)

plot_grid(myplot, p4, nrow =2, align = "v", axis ="l" )


 