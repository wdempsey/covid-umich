data_files = list.files(path = "D:/fb-data/all-fb-data/monthly_data/")

file_name = "../data/fb_test_data.csv"

library("lubridate")

for(i in 1:length(data_files)) {

  temp = read.csv(paste("D:/fb-data/all-fb-data/monthly_data/",data_files[i], sep=""))
  
  state_temp = subset(temp,is.element(A3b, c(15)))  #subset(temp,is.element(A3b, c(15,18,23,36))) # Bringing in Ohio/Michigan/Indiana/Kentucky for more data on this problem.
  
  time <- ymd_hms(state_temp$StartDatetime)
  day_df = data.frame("date" = date(time), "weight" = state_temp$weight)
  
  fever <- function(x) {any(as.numeric(unlist(strsplit(x, split = ",")))==1)}
  cough <- function(x) {any(as.numeric(unlist(strsplit(x, split = ",")))==2)}
  shortness <- function(x) {any(as.numeric(unlist(strsplit(x, split = ",")))==3)}
  diffbreath <- function(x) {any(as.numeric(unlist(strsplit(x, split = ",")))==4)}
  
  day_df$fever = unlist(lapply(state_temp$B2,fever))
  day_df$cough = unlist(lapply(state_temp$B2,cough))
  day_df$shortness = unlist(lapply(state_temp$B2,shortness))
  day_df$diffbreath = unlist(lapply(state_temp$B2,diffbreath))
  day_df$symptoms = (day_df$fever & day_df$cough) | day_df$shortness | day_df$diffbreath
  
  day_df$contact = state_temp$C11
  
  ### WAVE 3 Testing Info (only on symptomatic people)
  wave456_included = any(state_temp$wave >=4)
  if(!wave456_included) {
    day_df$gottested = as.numeric(is.element(state_temp$B5, c(1,2,3)))
    day_df$postest =  as.numeric(is.element(state_temp$B5, c(1)))
    day_df$negtest =  as.numeric(is.element(state_temp$B5, c(2)))
  } else { 
    day_df$gottested = as.numeric(is.element(state_temp$B5, c(1,2,3)) * (state_temp$wave < 4) + (state_temp$B10 == 1)*(state_temp$wave >= 4))
    day_df$postest =  as.numeric(is.element(state_temp$B5, c(1)) * (state_temp$wave < 4) + (state_temp$B10a == 1)*(state_temp$wave >= 4))
    day_df$negtest =  as.numeric(is.element(state_temp$B5, c(2)) * (state_temp$wave < 4) + (state_temp$B10a == 2)*(state_temp$wave >= 4))
  }
  
  day_df$gender = state_temp$D1 # 1 = MALE, 2 = Female, 3 = Non-binary, 4 = Self-Describe, 5 = Prefer not to answer
  day_df$age = state_temp$D2 #  18-24 years (1), 25-34 years (2), 35-44 years (3), 45-54 years (4), 55-64 years (5), 65-74 years (6), 75 years or older (7)
  if(!wave456_included) {
    day_df$hospital = state_temp$B6 == 1
  } else{
    tempwave123 = state_temp$B6 == 1
    tempwave123[is.na(tempwave123)] = 0
    tempwave456 = grepl("4", state_temp$B7) | grepl("5", state_temp$B7) | grepl("6", state_temp$B7)
    tempwave456[is.na(tempwave456)] = 0
    day_df$hospital = tempwave123 * (state_temp$wave < 4) + tempwave456 *(state_temp$wave >= 4)
  }
  
  test_df = subset(day_df, gottested == 1)
 
  day_df$weight_gottested = day_df$weight * day_df$gottested
  day_df$weight_postested = day_df$weight * day_df$postest
  
  if(file.exists(file_name)){
    write.table(file = file_name, x = day_df, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
  } else{
    write.table(file = file_name, x = day_df, row.names = FALSE, sep = ",")
  }
  
}
