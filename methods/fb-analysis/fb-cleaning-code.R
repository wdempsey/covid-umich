data_files = list.files(path = "./all-fb-data/")

file_name = "indiana_data.csv"

library("lubridate")

for(i in 1:length(data_files)) {

  temp = read.csv(paste("./all-fb-data/",data_files[i], sep=""))
  
  indiana_temp = subset(temp,A3b == 15)
  
  time <- ymd_hms(indiana_temp$StartDatetime)
  day_df = data.frame("date" = date(time), "weight" = indiana_temp$weight)
  
  fever <- function(x) {any(as.numeric(unlist(strsplit(x, split = ",")))==1)}
  cough <- function(x) {any(as.numeric(unlist(strsplit(x, split = ",")))==2)}
  shortness <- function(x) {any(as.numeric(unlist(strsplit(x, split = ",")))==3)}
  diffbreath <- function(x) {any(as.numeric(unlist(strsplit(x, split = ",")))==4)}
  
  day_df$fever = unlist(lapply(indiana_temp$B2,fever))
  day_df$cough = unlist(lapply(indiana_temp$B2,cough))
  day_df$shortness = unlist(lapply(indiana_temp$B2,shortness))
  day_df$diffbreath = unlist(lapply(indiana_temp$B2,diffbreath))
  day_df$symptoms = (day_df$fever & day_df$cough) | day_df$shortness | day_df$diffbreath
  
  day_df$gottested = as.numeric(is.element(indiana_temp$B5, c(1,2,3)))
  day_df$postest =  as.numeric(is.element(indiana_temp$B5, c(1)))
  day_df$negtest =  as.numeric(is.element(indiana_temp$B5, c(2)))
  
  day_df$gender = indiana_temp$D1
  day_df$age = indiana_temp$D2
  
  if(file.exists(file_name)){
    write.table(file = file_name, x = day_df, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
  } else{
    write.table(file = file_name, x = day_df, row.names = FALSE, sep = ",")
  }
  
}
