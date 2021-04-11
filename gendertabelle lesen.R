# Gendertabellen lesen
library(dplyr)
library(rvest)

## NICHT weiter verfolgt aufgrund Lizensierung nach CC BY NC
# # scraping https://www.gendern.de/#Gender-Woerterbuch
# url = "https://www.gendern.de/#Gender-Woerterbuch"
# table = url %>%
#   read_html() %>%
#   html_nodes(xpath = '//*[@id="tablepress-4"]') %>% 
#   html_table() 
# gender_woerterbuch = table[[1]] %>% 
#   as.data.frame(.) %>% 
#   select(1:3) %>% 
#   setNames(c("text","vorschlag","stammform")) %>% 
#   mutate(stammform = ifelse(stammform == "",NA,stammform))
# glimpse(gender_woerterbuch)
# gender_woerterbuch = gender_woerterbuch %>% 
#   tidyr::unite("vorschlag",vorschlag:stammform,sep = "; ",na.rm = T) %>% 
#   tidyr::separate_rows(.,text,sep = ", ") %>% 
#   tidyr::separate_rows(.,vorschlag,sep = ";") %>% 
#   mutate(
#     text = trimws(text,which = "both"),
#     vorschlag = trimws(vorschlag,which = "both"),
#   ) %>% 
#   distinct()
# 

## NICHT weiter verfolgt aufgrund Lizensierung nach CC BY NC
# # geschicktgendern.de
# filename = "H:/R-Stuff/genderAddin/data/geschicktgendern.xlsx"
# 
# geschicktgendern = openxlsx::read.xlsx(
#   xlsxFile = filename,
#   sheet = 1,
#   startRow = 4,
#   colNames = F,
#   cols = c(1:3)
#   ) %>%
#   filter(!grepl("<",X2)) %>%
#   filter(!grepl("<",X3)) %>%
#   mutate(
#     X2 = gsub("\\s*\\([^\\)]+\\)","",X2),
#     X2 = gsub("\\s*\\[[^\\)]+\\]","",X2)
#     ) %>% 
#   select(-X1) %>% 
#   tidyr::separate(X3,paste0("variante_",c(1:10)),fill = "right",extra = "merge",sep = ";") %>%
#   tidyr::pivot_longer(.,cols = starts_with("variante_")) %>%
#   select(-name) %>% 
#   filter(!is.na(value)) %>% 
#   mutate(
#     X2 = stringr::str_replace_all(X2,"\\.\\.\\.",""),
#     value = stringr::str_squish(value),
#     ) %>% 
#   select(
#     text = X2,
#     vorschlag = value
#   )

# wien worttabelle
# "https://www.data.gv.at/katalog/dataset/15d6ede8-f128-4fcd-aa3a-4479e828f477/resource/804f6db1-add7-4480-b4d0-e52e61c48534/download/worttabelle.csv"
filename = "H:/R-Stuff/genderAddin/data/worttabelle.csv"
worttabelle = read.csv(
  filename,
  encoding = "UTF-8",
  na.strings = c("",NA," ")
) %>% 
  select(2:3) %>% 
  setNames(
    c(
      "text",
      "vorschlag"
    )
  ) %>% 
  filter(!is.na(vorschlag),!is.na(text))

data = tibble(text = "",vorschlag = "",.rows = 0) %>% 
  dplyr::bind_rows(worttabelle) %>% 
  #geschicktgendern %>% 
 # dplyr::bind_rows(gender_woerterbuch) %>% 
  mutate(
    text = stringr::str_replace_all(text,"\\.\\.\\.",""),
    text = stringr::str_replace_all(text,"…",""),
    vorschlag = stringr::str_squish(vorschlag),
    text = stringr::str_squish(text)
  ) %>%
  mutate(word_len = nchar(text)) %>% 
  distinct() %>% 
  arrange(word_len,text,vorschlag) %>% 
  select(word_len,text,vorschlag) %>% 
  filter(stringr::str_count(text," ") < 1)

# daten sichern
write.table(
  data,
  file = "H:/R-Stuff/genderAddin/daten.csv",
  col.names = FALSE,
  fileEncoding = "UTF-8",
  quote = FALSE,
  sep = ",",
  row.names = FALSE
  )

# Statistics
# einzigartige Keywords
data %>% 
  summarize(n = n_distinct(text))

# quantile der voschläge pro wort
data %>% 
  group_by(text) %>% 
  tally() %>% 
  summarize(quantile(n))

# mittelwert der vorschläge pro wort
data %>% 
  group_by(text) %>% 
  tally() %>% 
  summarize(mean(n))


