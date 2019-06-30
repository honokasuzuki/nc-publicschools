### manipulating/creating datasets to be used in Tableau ###

#1) full original data
nc_data = read.csv("NC Schools.csv", header = T)

#remove duplicate rows
nc_data = unique(nc_data)

library(dplyr)
#select fields related to school demographics, performance, and climate
use = nc_data %>% select(School.Name.x,District.Name,
                         lon,lat,
                         Category_Cd,
                         starts_with("SPG."),
                         grade,
                         EVAAS.Growth.Status,
                         total_pop,
                         avg_daily_attend_pct:stud_internet_comp_num,
                         wap_per_classroom,
                         books_per_student,
                         X_1_to_1_access)
use$State = rep("NC", nrow(use))

write.csv(use, "NC_All_Schools.csv")
colnames(use)
summary(use)

######################

#2) SPG data to compare individual school and NC average by school category and academic year 
#create a new manipulated dataset where each row is a school from a particular academic year
#fields are school name, district name, school category, academic year, school's SPG for that year, and the NC average SPG for that year
library(tidyverse)

#select fields
grade_time = use %>% select(School.Name.x, District.Name, Category_Cd, starts_with("SPG.Score")) %>% 
  gather(Year, School_Value, -c(School.Name.x:Category_Cd))

#calculate the NC average SPG for each academic year and school category
NC16_score = use %>% group_by(Category_Cd) %>% summarise(mean = mean(SPG.Score))
NC14_score = use %>% group_by(Category_Cd) %>% summarise(mean = mean(SPG.Score_14))
NC15_score = use %>% group_by(Category_Cd) %>% summarise(mean = mean(SPG.Score_15))

#create function to replace level names of the field academic year to be cleaner 
change_year = function(x){
  if(x == "SPG.Score"){
    x = "2016-17"
  }else if(x == "SPG.Score_14"){
    x = "2014-15"
  }else if(x == "SPG.Score_15")
    x = "2015-16"
}

#apply the function to entire column
grade_time$Year = sapply(grade_time$Year, change_year)

# nc_average = function(x){
#   if(x == "2016-17"){
#     x = NC16_Score[which(NC16_score$Category_Cd == grade_time[,]),2]
#   }else if(x == "2014-15"){
#     x = NC14_score
#   }else if(x == "2015-16")
#     x = NC15_score
# }

#loop through each row of grade_time data to fill in respective NC average SPG value for that school category/academic year combination
for (i in 1:nrow(grade_time)){
  if(grade_time$Year[i] == "2016-17"){
    data = NC16_score
  }else if(grade_time$Year[i] == "2014-15"){
    data = NC14_score
  }else if(grade_time$Year[i] == "2015-16"){
    data = NC15_score
  }
  grade_time$NC_Value[i] = round(data[which(data$Category_Cd == grade_time$Category_Cd[i]),2],2)
}

grade_time$NC_Value = unlist(grade_time$NC_Value)
write.csv(grade_time, "SPScore_Time.csv")


######################

#3) student behavior data to compare individual school and NC average by type of incident and school category
#create a new manipulated dataset where each row is a school and a particular type of behavioral incident (expelled, suspension, etc.)
#fields are school name, district name, school category, incident type, school's incidence count, and the NC average incidence count

#select fields
behavior = use %>% select(School.Name.x, 
                          District.Name, 
                          Category_Cd, 
                          expelled_per_c_num, 
                          crime_per_c_num, 
                          short_susp_per_c_num, 
                          long_susp_per_c_num)
behavior = behavior %>% gather(Type, School_Value, -c(School.Name.x:Category_Cd))

#calculate the NC average count for each incident type and school category
NC_Expel = use %>% group_by(Category_Cd) %>% summarise(mean = mean(expelled_per_c_num))
NC_Crime = use %>% group_by(Category_Cd) %>% summarise(mean = mean(crime_per_c_num))
NC_Short = use %>% group_by(Category_Cd) %>% summarise(mean = mean(short_susp_per_c_num))
NC_Long = use %>% group_by(Category_Cd) %>% summarise(mean = mean(long_susp_per_c_num))

# nc_average = function(x){
#   if(x == "expelled_per_c_num"){
#     x = NC_Expel
#   }else if(x == "crime_per_c_num"){
#     x = NC_Crime
#   }else if(x == "short_susp_per_c_num"){
#     x = NC_Short
#   }else if(x == "long_susp_per_c_num"){
#   x = NC_Long
#   }
# }

#loop through each row of behavior data to fill in respective NC average count for that school category/incident type combination
for (i in 1:nrow(behavior)){
  if(behavior$Type[i] == "expelled_per_c_num"){
    data = NC_Expel
  }else if(behavior$Type[i] == "crime_per_c_num"){
    data = NC_Crime
  }else if(behavior$Type[i] == "short_susp_per_c_num"){
    data = NC_Short
  }else if(behavior$Type[i] == "long_susp_per_c_num"){
    data = NC_Long
  }
  behavior$NC_Value[i] = round(data[which(data$Category_Cd == behavior$Category_Cd[i]),2],2)
}

behavior$NC_Value = unlist(behavior$NC_Value)

write.csv(behavior, "Behavior.csv")

#######################

all = left_join(use, grade_time, by = c("School.Name.x", "District.Name"))
all = left_join(all, behavior, by = c("School.Name.x", "District.Name"))
all %>% group_by(District.Name, School.Name.x) %>% summarise(n = n()) %>% View()

#######################

#4) school resources data to compare individual school and NC average by type of resource and school category
#create a new manipulated dataset where each row is a school and a particular type of school resource (books, computers, etc.)
#fields are school name, district name, school category, resource type, school's count, and the NC average count

#select fields
resources = use %>% select(School.Name.x, 
                           District.Name, 
                           Category_Cd, 
                           books_per_student, 
                           wap_per_classroom, 
                           stud_internet_comp_num)
resources = resources %>% gather(Type, School_Value, -c(School.Name.x:Category_Cd))

#calculate the NC average count for each resource type and school category
NC_Books = use %>% group_by(Category_Cd) %>% summarise(mean = mean(books_per_student))
NC_WAP = use %>% group_by(Category_Cd) %>% summarise(mean = mean(wap_per_classroom))
NC_Comp = use %>% group_by(Category_Cd) %>% summarise(mean = mean(stud_internet_comp_num))

#loop through each row of resources data to fill in respective NC average count for that school category/resource type combination
for (i in 1:nrow(resources)){
  if(resources$Type[i] == "books_per_student"){
    data = NC_Books
  }else if(resources$Type[i] == "wap_per_classroom"){
    data = NC_WAP
  }else if(resources$Type[i] == "stud_internet_comp_num"){
    data = NC_Comp
  }
  resources$NC_Value[i] = round(data[which(data$Category_Cd == resources$Category_Cd[i]),2],2)
}

resources$NC_Value = unlist(resources$NC_Value)

write.csv(resources, "Resources.csv")

#######################

#the SPG, behavior, and resources data will be left-joined with the full All Schools dataset when pulled in Tableau 
