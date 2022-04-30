
# Setting-up --------------------------------------------------------------

packages = c("devtools",
             "usethis",
             "here",
             "readr",
             "readxl",
             "tidyverse",
             "tidylog",
             "lubridate",
             "ggplot2",
             "tidylog",
             "ggplotgui",
             "ggthemes",
             "arsenal")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

getwd()
rm(list=ls())

getwd()
setwd("C:/Users/akihi/Downloads/RDD-ITS")

## dataset1

filelist <- list.files("input/result_1",
                       pattern = "\\csv$")
df1 <- map_df(filelist, ~{
  path <- file.path("input/result_1", .)
  df <- read_csv(path, locale = locale(encoding = "SHIFT-JIS"))
  df <- df %>% mutate_all(.funs = as.character)
})

df1 %>% colnames()
df1 <- df1 %>% 
  select(1:21) 

df1 %>% glimpse()

df1 <- df1 %>% 
  rename(id = "患者ID",
         date = "依頼日",
         age = "年齢",
         sex = "性別",
         setting = "入外",
         sc_2 = "結果1",
         hc_229E = "結果2",
         hc_NKU1 = "結果3",
         hc_NL63 = "結果4",
         hc_oc43 = "結果5",
         inf_A = "結果6") %>% 
  mutate(sc_2 = if_else(sc_2 == "検出せず", 0, 1),
         hc_229E = if_else(hc_229E == "検出せず", 0, 1),
         hc_NKU1 = if_else(hc_NKU1 == "検出せず", 0, 1),
         hc_NL63 = if_else(hc_NL63 == "検出せず", 0, 1),
         hc_oc43 = if_else(hc_oc43 == "検出せず", 0, 1),
         inf_A = if_else(inf_A == "検出せず", 0, 1),
         sex = if_else(sex == "女性", 0, 1),
         setting = if_else(setting == "外来", 1, 0)) %>% 
  distinct(id, date, .keep_all=TRUE)

df1 %>% colnames()
df1 <- df1 %>% 
  select(2,4,5,6,7,11,13,15,17,19,21) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate(id = as.numeric(id),
         date = ymd(date)) 

df1 %>% glimpse()

df <- df1 %>% 
  select(id, date) %>% 
  mutate(id = as.numeric(id),
         date = ymd(date))

## dataset2

filelist <- list.files("input/result_2",
                       pattern = "\\csv$")
df2 <- map_df(filelist, ~{
  path <- file.path("input/result_2", .)
  df <- read_csv(path, locale = locale(encoding = "SHIFT-JIS"))
  df <- df %>% mutate_all(.funs = as.character)
})

df2 %>% colnames()
df2 %>% glimpse()

df2 <- df2 %>% 
  select(1:29)

df2 <- df2 %>% 
  rename(id = "患者ID",
         date = "依頼日",
         age = "年齢",
         sex = "性別",
         setting = "入外",
         inf_B = "結果1",
         para_1 = "結果2",
         para_2 = "結果3",
         para_3 = "結果4",
         para_4 = "結果5",
         adeno = "結果6",
         hmp = "結果7",
         rhv = "結果8",
         rsv = "結果9",
         pts = "結果10") %>% 
  mutate(inf_B = if_else(inf_B == "検出せず", 0, 1),
         para_1 = if_else(para_1 == "検出せず", 0, 1),
         para_2 = if_else(para_2 == "検出せず", 0, 1),
         para_3 = if_else(para_3 == "検出せず", 0, 1),
         para_4 = if_else(para_4 == "検出せず", 0, 1),
         adeno = if_else(adeno == "検出せず", 0, 1),
         hmp = if_else(hmp == "検出せず", 0, 1),
         rhv = if_else(rhv == "検出せず", 0, 1),
         rsv = if_else(rsv == "検出せず", 0, 1),
         pts = if_else(pts == "検出せず", 0, 1),
         sex = if_else(sex == "女性", 0, 1),
         setting = if_else(setting == "外来", 1, 0)) %>% 
  distinct(id, date, .keep_all=TRUE)

df2 %>% colnames()
df2 <- df2 %>% 
  select(2,4,5,6,7,11,13,15,17,19,21,23,25,27,29) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate(id = as.numeric(id),
         date = ymd(date))

df2 %>% glimpse()

## dataset3

filelist <- list.files("input/result_3",
                       pattern = "\\csv$")
df3 <- map_df(filelist, ~{
  path <- file.path("input/result_3", .)
  df <- read_csv(path, locale = locale(encoding = "SHIFT-JIS"))
  df <- df %>% mutate_all(.funs = as.character)
})

df3 %>% colnames()
df3 <- df3 %>% 
  select(1:29)

df3 %>% colnames()
df3 %>% glimpse()

df3 <- df3 %>% 
  select(1:29)

df3 <- df3 %>% 
  rename(id = "患者ID",
         date = "依頼日",
         age = "年齢",
         sex = "性別",
         setting = "入外",
         clam = "結果1",
         myco = "結果2",
         para_pts = "結果3") %>% 
  mutate(clam = if_else(clam == "検出せず", 0, 1),
         myco = if_else(myco == "検出せず", 0, 1),
         para_pts = if_else(para_pts == "検出せず", 0, 1),
         sex = if_else(sex == "女性", 0, 1),
         setting = if_else(setting == "外来", 1, 0)) %>% 
  distinct(id, date, .keep_all=TRUE)

df3 %>% glimpse()
df3 %>% colnames()
df3 <- df3 %>% 
  select(2,4,5,6,7,11,13,15) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate(id = as.numeric(id),
         date = ymd(date))

## combine three datasets

df <- left_join(df, df1, by = c("id","date")) %>% 
  select(-sex, -age, -setting)
df <- left_join(df, df2, by = c("id","date")) %>% 
  select(-sex, -age, -setting)
df <- left_join(df, df3, by = c("id","date"))

df %>% glimpse()
df %>% write.csv("output/cleaned_data.csv")
