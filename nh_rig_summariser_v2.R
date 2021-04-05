##
# Script to load rig csv to create a more detailed summary of the mining.

## Things to note:
# NiceHash has a point of data per whole 5th minute of active mining; 
# see time and HMS variable.

### Constants
nh_payout_lim = 10^-5
hour_constant_diff = 2 #Change depending on the time zone

### Libraries
# Nice to have, and some are used
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

#####
## Load csv(s) --- only for one at this point.

path_to_nicehash_rig_csv = ".../my_nicehash/excavator_2021-04-05.csv"

rig_csv = read.csv(path_to_nicehash_rig_csv)

#####
## Check csv(s)

names(rig_csv)
dim(rig_csv)

head(rig_csv)
tail(rig_csv)

#####
## Create (plausibly) usable derivatives

mod_rig_csv = rig_csv

#Quick look at data
head(mod_rig_csv)
tail(mod_rig_csv)

### Basic index 
# (wrt. assumed chronological time in the loaded csv;
# does not contain information about time gaps where mining has stopped)
mod_rig_csv$index = dim(mod_rig_csv)[1]:1


### Profitability per MH (relative to accepted speed)
mod_rig_csv$prof_MH = mod_rig_csv$profitability/mod_rig_csv$speed_accepted


### Time and date management
mod_rig_csv$time_posix = as.POSIXct(mod_rig_csv$time,format = "%Y-%m-%d %H:%M:%S") #Ensure data type.

mod_rig_csv$time_posix = mod_rig_csv$time_posix + hours(hour_constant_diff) #Fix time difference.

#Then derive measurements from fixed time.
mod_rig_csv$YMD = format(mod_rig_csv$time_posix, format = "%Y/%m/%d")
mod_rig_csv$HMS = format(mod_rig_csv$time_posix, format = "%H:%M:%S")
mod_rig_csv$Hour = format(mod_rig_csv$time_posix, format = "%H")


#Hour per weekday (7 categories) - i.e. which weekday are more profitable than others.
mod_rig_csv$Weekday = as.factor(weekdays(mod_rig_csv$time_posix))

mod_rig_csv$Weekday_fixed <- #Removing Norwegian names set by R base "weekdays" function.
  recode_factor(mod_rig_csv$Weekday,
                'mandag' = "1. Monday",
                'tirsdag' = "2. Tuesday",
                'onsdag' = "3. Wednesday",
                'torsdag' = "4. Thursday",
                'fredag' = "5. Friday",
                'lørdag' = "6. Saturday",
                'søndag' = "7. Sunday")

#Hour per month - i.e. which month are more profitable than others.
mod_rig_csv$MonthHour = format(mod_rig_csv$time_posix, format = "%m_%H")


#####
## Examine properties

### Line-plot of profitability with basic index (Not interesting)
plot(mod_rig_csv$index,mod_rig_csv$profitability,type = "l")

p_profitability <- ggplot(data = mod_rig_csv,aes(index,profitability)) +
  geom_line() +
  scale_y_continuous(name = "24h Profitability", 
                     breaks = seq(0,15*nh_payout_lim,2.5*nh_payout_lim),limits = c(0,15*nh_payout_lim))

p_profitability


### Line-plot of profitability per MH with basic index (Interesting)
plot(mod_rig_csv$index,mod_rig_csv$prof_MH,type = "l")

p_prof_MH <- ggplot(data = mod_rig_csv,aes(index,prof_MH)) +
  geom_line() +
  scale_y_continuous(name = "24h Profitability per MH", 
                     breaks = seq(0,1.5*nh_payout_lim,2*nh_payout_lim/10),limits = c(0,1.2*nh_payout_lim))

p_prof_MH


### Profitability per MH per hour (Interesting)
p_prof_MH_hour <- ggplot(data = mod_rig_csv,aes(x = Hour,y = prof_MH)) +
  geom_boxplot() + 
  scale_y_continuous(name = "24h Profitability per MH", 
                     breaks = seq(0,1.5*nh_payout_lim,2*nh_payout_lim/10),limits = c(0,1.2*nh_payout_lim)) +
  scale_x_discrete(name = "Hour of day")

p_prof_MH_hour


### Profitability per MH per weekday (Interesting)
p_prof_MH_wkd <- ggplot(data = mod_rig_csv,aes(x = Weekday_fixed,y = prof_MH)) +
  geom_boxplot() + 
  scale_y_continuous(name = "24h Profitability per MH", 
                     breaks = seq(0,1.5*nh_payout_lim,2*nh_payout_lim/10),limits = c(0,nh_payout_lim)) +
  scale_x_discrete(name = "Weekday")

p_prof_MH_wkd

### Line-plot of unpaid amount (Not interesting...)
plot(mod_rig_csv$index,mod_rig_csv$unpaid_total_amount,type = "l")


##### 
## Testing

### Compare weekdays 24h profitability per MH with each other
compare_x_fun = function(df_1, df_2, n_times = 5000){
  #Distribution invariant testing
  n_1 = length(df_1)
  
  n_2 = length(df_2)
  
  if(!(n_1 > 2 & n_2 > 2)){
    #Want it to be larger, as sample behaves another way if scalar is provided.
    return(-1)
  }
  
  x_1 = sample(df_1,size = n_times, replace = TRUE)
  x_2 = sample(df_2,size = n_times, replace = TRUE)
  
  B = sum(x_1 > x_2,na.rm = TRUE)
  
  p_B = B/n_times
  
  return(p_B)
}

compare_c_fun = function(df,
                         label_c = "prof_MH",
                         label_in = "Weekday_fixed",
                         n_times = 5000){
  
  vector_lbl = df[,label_in]
  uniq_lbl = sort(unique(vector_lbl),decreasing = FALSE)
  
  n_lbl = length(uniq_lbl)
  
  output_matrix = matrix(0,nrow = n_lbl,ncol = n_lbl)
  
  rownames(output_matrix) <- uniq_lbl
  colnames(output_matrix) <- uniq_lbl
  
  for(i in 1:n_lbl){
    for(j in 1:n_lbl){
      print(paste("Current:", i,"and", j,sep = " "))
      
      if(i == j){
        next()
        #Skip diagonal.
      }
      
      i_lbl = uniq_lbl[i]
      j_lbl = uniq_lbl[j]
      
      vector_1 = df[(vector_lbl == i_lbl),label_c]
      vector_2 = df[(vector_lbl == j_lbl),label_c]
      
      p_12_ij = compare_x_fun(df_1 = vector_1,df_2 = vector_2, n_times = n_times)
      
      output_matrix[i,j] = p_12_ij
    }
  }
  
  return(output_matrix)
}

out_matrix_weekdays = compare_c_fun(df = mod_rig_csv,n_times = 100000)

print(out_matrix_weekdays,digits = 3,quote = TRUE)


### Compare hours of the day with each other???














