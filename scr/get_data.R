library(tidyverse)
library(rvest)
library(xml2)

nr_link_rss <- "https://www.parlament.gv.at/WWER/NR/ABG/filter.psp?view=RSS&jsMode=&xdocumentUri=&filterJq=&view=&GP=ALLE&R_WF=FR&FR=ALLE&W=W&M=M&listeId=4&FBEZ=FW_004"

xml_nr <- xml2::read_xml(nr_link_rss)

df_nr <- xml_nr %>% 
  xml2::xml_find_all("//description") %>% 
  rvest::html_text() %>% 
  tibble::enframe(name="id", value="raw") %>% 
  filter(!str_detect(raw, "Parlamentarier")) %>% 
  mutate(raw=str_squish(raw) %>% str_trim(., side=c("both"))) %>% 
  separate(raw, 
           sep="<br />", 
           into=c("name", "fraction", "period", "state"), 
           remove=T)


df_nr <- df_nr %>% 
  mutate(name=str_remove(name, "Name:"),
         fraction=str_remove(fraction, "Fraktion:"),
         period=str_remove(period, "Gesetzgebungsperioden:"),
         state=str_remove(state, "Bundesland:")) %>% 
  mutate(across(.cols=where(is.character), str_trim))

df_nr <- df_nr %>% 
  mutate(name_dupes=case_when(str_detect(name, "siehe") ~ str_remove(name, regex("siehe.*")),
                              TRUE ~ name)) %>% 
  mutate(name_only=str_remove(name_dupes, ",.*")) %>% 
  mutate(name_family=str_extract(name_only, regex("^\\S*"))) %>% 
  mutate(name_first=str_remove(name_only, name_family) %>% str_trim) %>% 
  mutate(title=str_extract(name_dupes, regex("(?<=,).*$")) %>% str_trim) %>% 
  select(name_raw=name, name_family, name_first, title, fraction, period, state)


readr::write_excel_csv2(df_nr, file=here::here("data", "at_nr_since_1918.csv"))
  
head(df_nr)

