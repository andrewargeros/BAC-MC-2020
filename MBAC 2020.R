rm(list=ls())
library(readxl)

mbac.list = list()
sheets = excel_sheets("./MBAC 2020/PHASE-ONE_NYC-housing-data.xlsx")
sheets.cm = janitor::make_clean_names(sheets, case = "snake")
sheets = cbind(sheets, sheets.cm) %>% as_tibble()

for (i in sheets$sheets[2:nrow(sheets)]){
  shname = as.character(sheets[which(sheets$sheets==i), "sheets.cm"])
   mbac.temp = read_excel("./MBAC 2020/PHASE-ONE_NYC-housing-data.xlsx", 
                         sheet = i) %>% 
    janitor::clean_names(case = "snake") %>%
    select(-short_name, -long_name) %>% 
    janitor::remove_empty("cols") %>% 
    pivot_longer(2:ncol(.), 
                 values_to = shname) %>% 
    rename("year" = name)
 mbac.temp$year = str_extract_all(mbac.temp$year, "\\d{4}") %>% 
   as.integer()
  mbac.list[[i]] = mbac.temp
}

list.sba = list()
list.cd = list()
for (i in mbac.list){
  df.temp = i
  j = names(df.temp[,3])
   if (names(df.temp[,1])=="sub_borough_area"){
     list.sba[[j]] = df.temp
   }else{
     list.cd[[j]] = df.temp
   }
}

mbac.sb = plyr::join_all(list.sba,
                         by = c("sub_borough_area", "year"),
                         type = "full")
mbac.sb$sub_borough_area = as.factor(mbac.sb$sub_borough_area)
mbac.cd = plyr::join_all(list.cd,
                         by = c("community_district", "year"),
                         type = "full")
mbac.cd$community_district = as.factor(mbac.cd$community_district)

ggplot(mbac.sb, aes(x = year, y = poverty_rate,
                    group = sub_borough_area, fill = sub_borough_area))+
  geom_line()+
  facet_wrap(~sub_borough_area)

mbac_sb_2019 = mbac.sb %>% 
  filter(year == 2018) %>% 
  arrange(desc(housing_units), homeowner_income)

summary(mbac_sb_2019$homeowner_income)
summary(mbac_sb_2019$renter_income)