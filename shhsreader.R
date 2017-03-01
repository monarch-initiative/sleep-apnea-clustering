library(tidyverse)

setwd("~/Documents/MSthesis/datasets")
shhs2<-read_csv("shhs2-dataset-0.11.0.csv")
shhs1<-read_csv("shhs1-dataset-0.11.0.csv")
datadict<-read_csv("shhs-data-dictionary-0.11.0-variables.csv")

shhsWhole<-left_join(shhs1, shhs2, by="obf_pptid")

Admin<-datadict %>%
  filter(str_detect(folder, "Administrative"))

shhs2 %>%
  select(sh322a,sh323a)

ids<-Admin %>%
  select(id)
shhs2 %>%
  select(shhs2_hi, shhs2_ms)

ids.df<-as.data.frame(ids)

shhs2 %>%
  dplyr::select(starts_with("shhs2"))

interim.data<-read_csv("shhs-interim-followup-dataset-0.11.0.csv")
interim.data %>%
  dplyr::select(accidents)


cols_admin<-Admin %>% select (id) %>% collect %>% .[["id"]]
shhsWhole %>%
  dplyr::select(one_of(my_cols[2:25]))

#alternative way
iris2 %>% select(Species) %>% unlist(use.names = FALSE)

#another alternative
pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}

pull (Admin, id)
library(stringr)
datadict %>%
  filter(str_detect(folder, "Heart Rate"))

percent3<-datadict %>%
  filter(str_detect(display_name, "3%"))
percent3ColNames<-percent3 %>% select(id) %>% unlist(use.names = FALSE)
shhs2%>%select (-(one_of(percent3ColNames)))

shhs2 %>% select(-ecgdate)

colsToLower <- function(df) {
  names(df) <- tolower(names(df))
  df
}

mtcars %>% upcase %>% select(MPG)

assertthat::has_name(shhs1, "shhs1_bp")
colnames(shhs1)

filter(Admin, assertthat::has_name(shhs1, cols_admin))
cols_admin.shhs1<-Admin %>%
  filter(assertthat::has_name(shhs1, cols_admin)) %>%
  select (id) %>% 
  collect %>% 
  .[["id"]]

shhs1.noadmin<-shhs1 %>%
  select(-one_of(cols_admin.shhs1))

sq<-datadict %>%
  filter(str_detect(folder, "Signal Quality"))

miscToRemove<-datadict %>%
  filter(id, dplyr::starts_with("pm"))

drop.cols<-c("pm217", "pm227", "ecgdate")
shhs2 %>% select(-one_of(drop.cols))
shhs2.noadmin<-select(shhs2, -pm217, -pm227, -ecgdate)

shhs1.noadmin<-select(shhs1, -ecgdate)

cols_sq<-sq %>% select(id) %>% unlist(use.names = FALSE)

assertthat::has_name(shhs1, cols_sq)

#load function
#look at data dict for rows to remove
#verify cols are in there
#remove cols from tibble
#what to do with categorical variables
#find distance measure

ggplot(data=shhs2.pruned, aes(x=pptid, y=rdi0ps))+
  geom_point(alpha=.4, size=1, color="#880011")

library(cluster)

shhs2.pruned %>%
  select(matches('^pm212.|pm220..'))

shhs1.pruned %>%
  select(-matches('^dias.20'))

psg_hr<-pullTerms(datadict, shhs1.pruned, "Heart Rate")
psg_hr<-c(psg_hr, "obf_pptid") #add in id column
cols.psg_hr<-subsetCols(shhs1.pruned, psg_hr)
glimpse(cols.psg_hr)

avg_psg_hr<-pullTerms(datadict, cols.psg_hr, "display_name", "Average")

#cols.id<-shhs1.pruned%>%select(obf_pptid)
#left_join(cols.id, cols.psg_hr, by='obf_pptid')

head(cars)
ggplot(data=cols.psg_hr, aes(x=obf_pptid, y=savbnbh))+
  geom_point(alpha=.4, size=1, color="#880011")

catibble<-datadict %>% 
  filter(str_detect(folder, "Heart Rate")) %>%
  filter(str_detect(display_name, "Average"))

psg_hr<-pullTerms(catibble, shhs1.pruned)
psg_hr<-c(psg_hr, "obf_pptid") #add in id column

clusters_psg_hr.pam<-pam(na.omit(cols.psg_hr %>% select(-obf_pptid)), 3, FALSE, "euclidean")
clusters_psg_hr.hclust<-hclust(na.omit(cols.psg_hr))

