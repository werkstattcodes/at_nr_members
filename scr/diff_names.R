library(tidyverse)

#read file
df_nr <- readr::read_csv(file="https://raw.githubusercontent.com/werkstattcodes/at_nr_members/master/data/at_nr_since_1918.csv")

#split name versions
df_diff <- df_nr %>% 
  filter(str_detect(name_raw, "siehe" )) %>% 
  separate(col=name_raw, into=c("name_v1", "name_v2"), sep=" siehe ") %>% 
  mutate(across(.cols=contains("_v"), .fns = list(split=~str_remove_all(.,",") %>% str_split(., " "))))

#function to contrast different name versions
fn_diff_set <- function(set1, set2) {
  
  setdiff(union(set1, set2), intersect(set1, set2))
  
}

#apply function
df_diff <- df_diff %>% 
  mutate(diff=map2(name_v1_split, name_v2_split, fn_diff_set) %>% 
           map_chr(., ~paste(., collapse=", "))) 

#search pattern
family_name_pattern <- df_nr %>% 
  filter(str_detect(name_raw, "siehe")) %>% 
  distinct(name_family) %>%  #pattern only for family names; not Josef/Sepp; Hannes/Johannes
  pull() %>% 
  paste(., collapse="|")


#all mps whose family names changed  
df_names <- df_diff %>% 
  filter(str_detect(diff, family_name_pattern)) #those where only family name changed; title change irrelevant
  
#male MPs with change in family name  
library(gender)
df_male <- df_names %>%   
  mutate(gender_df=map(str_extract(name_first, regex("^\\S*")),  #only first part if double first name
                    possibly(gender, 
                             otherwise="missing"))) %>% 
  unnest_wider(gender_df, names_repair = "universal") %>% 
  select(-name, -contains("year")) %>% 
  filter(gender=="male") 

glimpse(df_male)
  


# difference in titles ----------------------------------------------------

title_pattern <- df_nr %>% 
  distinct(title) %>% 
  pull() %>% 
  paste(., collapse="|") #vector /w OR separator

#all mps whose titles changed 
df_title <- df_diff %>% 
  filter(!str_detect(diff, title_pattern))
df_title  
