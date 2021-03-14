
# Set working directory and load libraries --------------------------------

#set working directory
setwd("path to your own directory")

#load libraries / require and library does the same thing
require(tidyverse)


# Import data -------------------------------------------------------------

#Import file names from working directory
#This code gets all the CSV file names and save them as temp
temp=list.files(pattern = "*.csv")

#Using file names, import all csv files and save them as objects with names iqual to file names

datalist=lapply(setNames(temp,make.names(gsub("*.csv$","",temp))),read.csv)

#Now we have a list of 50 objects with data for one stip.
#Bind all the objects in the list in one big data frame.
#.id creates an extra column with the object name.
#In our case this create a column called "id" with file names.
#File names contain info about exp.
#tibble creates smarter dataframes with categories etc.

df = as_tibble(bind_rows(datalist, .id = "id"))

#Take a look to how the data frame looks like
head(df)


# Clean data and column names ---------------------------------------------

#Separate file names into indivudual columns with a more tidy information and sae it as df2, data is in df, column 
#has id , it is gonna sepratate it to treatment etc...
df2 = separate(data = df, col = id, 
               into = c("treatment", "remove", "hours", "repetition", "strip_no"), 
               sep = "_" ) #remove columns with useless data.

#Take a look to the tidied data frame df2
head(df2)

#Give better column names to df2 and save it as df3
df3 = rename(df2, value = Value, x = X, y = Y) 

#Take a look to df3
head(df3)

#extract the data from a column using dollar sign eg. df3$repetition
#Remove .jpg text from tray position

df3$repetition = gsub(".jpg$", "", df3$repetition)

#remove h from hours and coerce to numeric
df3$hours = as.numeric(gsub("h$", "", df3$hours))

#Final data frame
head(df3)


# Process data ------------------------------------------------------------

# Summarising by portion from A-D # percentages 15, 40, 67.5, >67.5 
df4 = df3 %>%
  group_by(treatment,hours,repetition,strip_no) %>% 
  mutate(x_min = min(x)) %>% #finds minimum value and creates a column with that
  mutate(x_norm = x - x_min) %>% #normalise x
  mutate(x_max = max(x)) %>%
  mutate(x_perc = x_norm / x_max * 100) %>%
  mutate(portion = ifelse(x_perc <=15, "A", 
                          ifelse(x_perc >15 & x_perc <=40, "B",
                                 ifelse(x_perc >40 & x_perc <= 67.5, "C","D")))) %>%
  group_by(treatment,hours,repetition,strip_no,portion) %>%
  summarise(portion_mean = mean(value), portion_sd = sd(value))

#re-order treatment
glimpse(df4)
df4$treatment = factor(df4$treatment,levels = c("CmXyn10", "NpXyn11A", "TfXyn11A","buffer"),
                       labels = c("CmXyn10B","NpXyn11A", "TfXyn11A","buffer"))


# Plot results ------------------------------------------------------------

#If we want to add hours to the image
df4$hours = factor(df4$hours,levels = c("3", "6"),
                   labels = c("3H","6H"))
df4 %>%
  ggplot(aes(x = portion, y = portion_mean, color = treatment, fill = treatment))+
  scale_fill_manual(values = c("#FFFFFF","#FF0000","#0000FF","#000000"))+
  scale_color_manual(values = c("#FFFFFF","#FF0000","#0000FF","#000000"))+
  geom_jitter(width = 0.1, alpha = 0.2)+
  facet_wrap(~hours)+
  stat_summary(fun.data = mean_se, geom = "pointrange", shape = 21, color = "black", size = 0.6)+
  theme_bw()+
  labs(y="portion mean",color="treatment",fill="treatment")


#To omit 20H from the plot
df4 %>%
  filter(hours !=20) %>% ggplot(aes(x = portion, y = portion_mean, color = treatment, fill = treatment))+
  #set to scale to 120 to 180
  scale_y_continuous(breaks = seq(120,180,20),limits = c(120,180))+
  scale_fill_manual(values = c("#FF0080","#00FFFF","#A65628","#000000"))+
  scale_color_manual(values = c("#FF0080","#00FFFF","#A65628","#000000"))+
  geom_jitter(width = 0.1, alpha = 0.2)+
  facet_wrap(~hours)+
  stat_summary(fun.data = mean_se, geom = "pointrange", shape = 21, color = "black", size = 0.6)+
  theme_bw()+
  labs(title="NpXyn11A",y="portion mean",color="pre-treatment",fill="pre-treatment")
