library(readxl)
library(tidyverse)
library(lubridate)
invisible(Sys.setlocale("LC_TIME", "English"))

cases_2425 <- read_excel("D:/OUCRU/sero_measles_nd2/data/Measles2018-2019-2024-2025_17.06.25.xlsx", 
                                                  sheet = "2024-2025")
cases_18 <- read_excel("D:/OUCRU/sero_measles_nd2/data/Measles2018-2019-2024-2025_17.06.25.xlsx", 
                         sheet = "2018")

cases_19 <- read_excel("D:/OUCRU/sero_measles_nd2/data/Measles2018-2019-2024-2025_17.06.25.xlsx", 
                       sheet = "2019")

sero <-  read_excel("D:/OUCRU/sero_measles_nd2/data/20240811_measles_titer_hcdc_with_meta.xlsx")

vaxreg_hcmc_measles <- readRDS("D:/OUCRU/sero_measles_nd2/data/vaxreg_hcmc_measles.rds")

census2019 <- readRDS("D:/OUCRU/hfmd/data/census2019.rds") %>% 
  filter(province == "Thành phố Hồ Chí Minh") %>% 
  mutate(age2 = word(age,1))

## map 
library(sf)
library(janitor)
library(stringr)
library(magrittr)
library(stringi)
library(paletteer)

map_path <- "D:/OUCRU/HCDC/project phân tích sero quận huyện/"
vn_qh <- st_read(dsn = file.path(map_path, "gadm41_VNM.gpkg"), layer = "ADM_ADM_2")

vn_qh1 <- vn_qh %>%
  clean_names() %>%     ## cho thành chữ thường
  filter(
    str_detect(
      name_1,
      "Hồ Chí Minh"
    )
  )
qhtp <- vn_qh1[-c(14,21),]

qhtp$geom[qhtp$varname_2 == "Thu Duc"] <- vn_qh1[c("21","24","14"),] %>%
  st_union()

qhtp <- qhtp %>% st_cast("MULTIPOLYGON")

qhtp$varname_2 <- stri_trans_general(qhtp$varname_2, "latin-ascii") %>%
  tolower() %>%
  str_remove("district") %>%
  trimws(which = "both")

qhtp$nl_name_2 <- c("BC","BTân","BT","CG","CC","GV",
                    "HM","NB","PN","1","10","11","12",
                    "3","4","5","6","7","8","TB",
                    "TP","TĐ")

### incidence analysis

incidence_hcm <- cases_2425 %>% 
  filter(province == "TP.HCM") %>%
  distinct(.keep_all = TRUE)

incidence_hcm <- incidence_hcm %>% mutate(
  district2 = district %>% 
    str_replace_all(
      c("Quận 2" = "Thủ Đức",
        "Quận 9" = "Thủ Đức")) %>% 
    str_remove("Quận|Huyện") %>%
    trimws(which = "both") %>% 
    stri_trans_general("latin-ascii") %>% 
    tolower() ,
  agegr2 = factor(agegr,
                  levels = c("< 6th","6 - 9th","9th -12th","1 - 5T","6-10T","> 10T"),
                  labels = c("< 6m","6-9m","9-12m","1-5y","6-10y","> 10y")),
  adm_month = month(admission),
  adm_year = year(admission),
  adm_month_cut = cut(adm_month,
                      breaks = c(0.5,3,6,9,12),
                      labels = c("Jan-Mar","Apr-June","Jul-Sep","Oct-Dec")),
  adm_month_year_cut = str_c(adm_month_cut,adm_year,sep = " ") %>% 
    factor(levels = c("Apr-June 2024","Jul-Sep 2024","Oct-Dec 2024",
                      "Jan-Mar 2025","Apr-June 2025"))
) 

tdnd2 <- data.frame(long = 106.7025,
                    lat  = 10.7808) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)


incidene_36912 <- incidence_hcm %>% 
  group_by(adm_month_year_cut,district2) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(adm_month_year_cut) %>% 
  group_modify(~.x %>% mutate(prevalence = n/sum(n)) %>% 
               left_join(qhtp, ., by = join_by(varname_2 == district2))) %>% 
  ggplot() +
  geom_sf(aes(fill = prevalence,geometry = geom),show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    labels = scales::label_percent(),
                                    na.value="white",
                                    name = "Prevalence")+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)+
  facet_wrap(~factor(adm_month_year_cut,
                     levels = c("Apr-June 2024","Jul-Sep 2024","Oct-Dec 2024",
                                "Jan-Mar 2025","Apr-June 2025")),ncol = 5) +
  labs(title = "Case prevalence in 2024-2025 outbreak")+
  theme_void()+
  theme(legend.position = "bottom")

incidence_hcm %>% 
  group_by(adm_month_year_cut,district2) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(district2) %>% 
  left_join(.,hcm_pop_19, by = join_by(district2 == district)) %>% 
  mutate(prevalence_10000 = (n/pop)*10000) %>% 
  ungroup() %>% 
  group_by(adm_month_year_cut) %>% 
  group_modify(~.x %>% mutate(prevalence = n/sum(n)) %>% 
                 left_join(qhtp, ., by = join_by(varname_2 == district2))) %>% 
  ggplot() +
  geom_sf(aes(fill = prevalence_10000,geometry = geom),show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    labels = scales::label_percent(),
                                    na.value="white",
                                    name = "Prevalence per 10.000 children")+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)+
  facet_wrap(~factor(adm_month_year_cut,
                     levels = c("Apr-June 2024","Jul-Sep 2024","Oct-Dec 2024",
                                "Jan-Mar 2025","Apr-June 2025")),ncol = 3) +
  labs(title = "Case prevalence in 2024-2025 outbreak")+
  theme_void()+
  theme(legend.position = "bottom")

## sero

sero_nd2 <- sero %>% filter(hospital == "Bv Nhi Dong 2") %>% 
  select(pos,district,age,age_1y,age_5y,sampling_period)

sero_nd2$district2 <- sero_nd2$district %>% 
  stri_trans_general("latin-ascii") %>% 
  str_remove("Tp|^0") %>%
  trimws(which = "both") %>% 
  tolower() 

sero_nd2$sampling_period <- sero_nd2$sampling_period %>% as.Date() 

sero_nd2 %>% 
mutate(
  district2 = district %>% 
    stri_trans_general("latin-ascii") %>% 
    str_remove("Tp|^0") %>%
    trimws(which = "both") %>% 
    tolower() ,
  adm_month = month(sampling_period),
  adm_year = year(sampling_period)
) 

sero_negative_district <- sero_nd2 %>% 
  group_by(sampling_period,district2) %>% 
  count(pos) %>% 
  pivot_wider(names_from = pos, 
              values_from = n,
              names_prefix = "pos_") %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(total = pos_0+pos_1,
         sp = pos_1/total,
         sneg = 1 - sp)  %>% 
  group_by(sampling_period) %>% 
  group_modify(~.x %>% left_join(qhtp, ., by = join_by(varname_2 == district2))) %>% 
  ungroup() %>% 
  ggplot() +
  geom_sf(aes(fill = sneg,geometry = geom),show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    labels = scales::label_percent(),
                                    na.value="white",
                                    name = "Sero-negative")+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)+
  facet_wrap(~factor(sampling_period,
                     labels = c("Sep 2022","Dec 2022","Mar 2023",
                                "June 2023","Oct 2023","Dec 2023")),
             ncol = 6) +
  labs(title = "Sero-negative from Sep 2022 - Dec 2023")+
  theme_void()+
  theme(legend.position = "bottom")

sero_negative_district/
  incidene_36912

### cases 18 analysis

cases_hcm_18 <- cases_18 %>% filter(province == "TP. H.C.M")

cases_hcm_18 <- cases_hcm_18 %>% mutate(
  district2 = district %>% 
    str_replace_all(
      c("Quận 2" = "Thủ Đức",
        "Quận 9" = "Thủ Đức")) %>% 
    str_remove("Quận|Huyện") %>%
    trimws(which = "both") %>% 
    stri_trans_general("latin-ascii") %>% 
    tolower() ,
  agegr2 = factor(agegr,
                  levels = c("<6 THANG","6-9 THANG","9Thang -1 Tuoi",
                             "Từ 1-5 tuổi","Từ 5-10 tuổi","Trên 10 tuổi"),
                  labels = c("< 6m","6-9m","9-12m","1-5y","6-10y","> 10y")),
  adm_month = month(admission),
  adm_year = year(admission),
  adm_month_cut = cut(adm_month,
                      breaks = c(0.5,3,6,9,12),
                      labels = c("Jan-Mar","Apr-June","Jul-Sep","Oct-Dec")),
  adm_month_year_cut = str_c(adm_month_cut,adm_year,sep = " ")
) 

prevalence_2018 <- cases_hcm_18 %>% 
  group_by(adm_month_year_cut,district2) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(adm_month_year_cut) %>% 
  group_modify(~.x %>% mutate(prevalence = n/sum(n)) %>% 
                 left_join(qhtp, ., by = join_by(varname_2 == district2))) %>% 
  ggplot() +
  geom_sf(aes(fill = prevalence,geometry = geom),show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    labels = scales::label_percent(),
                                    na.value="white",
                                    name = "Prevalence")+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)+
  facet_wrap(~adm_month_year_cut) +
  labs(title = "Case prevalence in 2018 outbreak")+
  theme_void()+
  theme(legend.position = "bottom")

###

cases_hcm_19 <- cases_19 %>% filter(province == "TP. H.C.M")

cases_hcm_19 <- cases_hcm_19 %>% mutate(
  district2 = district %>% 
    str_replace_all(
      c("Quận 2" = "Thủ Đức",
        "Quận 9" = "Thủ Đức")) %>% 
    str_remove("Quận|Huyện") %>%
    trimws(which = "both") %>% 
    stri_trans_general("latin-ascii") %>% 
    tolower() ,
  agegr2 = factor(agegr,
                  levels = c("<6 THANG","6-9 THANG","9Thang -1 Tuoi",
                             "Từ 1-5 tuổi","Từ 5-10 tuổi","Trên 10 tuổi"),
                  labels = c("< 6m","6-9m","9-12m","1-5y","6-10y","> 10y")),
  adm_month = month(admission),
  adm_year = year(admission),
  adm_month_cut = cut(adm_month,
                      breaks = c(0.5,3,6,9,12),
                      labels = c("Jan-Mar","Apr-June","Jul-Sep","Oct-Dec")),
  adm_month_year_cut = str_c(adm_month_cut,adm_year,sep = " ")
) 

prevalence_2019 <- cases_hcm_19 %>% 
  group_by(adm_month_year_cut,district2) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(adm_month_year_cut) %>% 
  group_modify(~.x %>% mutate(prevalence = n/sum(n)) %>% 
                 left_join(qhtp, ., by = join_by(varname_2 == district2))) %>% 
  ggplot() +
  geom_sf(aes(fill = prevalence,geometry = geom),show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    labels = scales::label_percent(),
                                    na.value="white",
                                    name = "Prevalence")+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)+
  facet_wrap(~adm_month_year_cut) +
  labs(title = "Case prevalence in 2018 outbreak")+
  theme_void()+
  theme(legend.position = "bottom")

prevalence_2019

cases_hcm_18 <- cases_18 %>% filter(province == "TP. H.C.M") %>% 
  select(id,gender,dob,agegr,district,admission)

cases_hcm_19 <- cases_19 %>% filter(province == "TP. H.C.M")%>% 
  select(id,gender,dob,agegr,district,admission)

cases_hcm_1819 <- rbind(cases_hcm_18,cases_hcm_19) %>% 
  distinct(id,dob, .keep_all = TRUE)  %>% mutate(
    district2 = district %>% 
      str_replace_all(
        c("Quận 2" = "Thủ Đức",
          "Quận 9" = "Thủ Đức")) %>% 
      str_remove("Quận|Huyện") %>%
      trimws(which = "both") %>% 
      stri_trans_general("latin-ascii") %>% 
      tolower() ,
    agegr2 = factor(agegr,
                    levels = c("<6 THANG","6-9 THANG","9Thang -1 Tuoi",
                               "Từ 1-5 tuổi","Từ 5-10 tuổi","Trên 10 tuổi"),
                    labels = c("< 6m","6-9m","9-12m","1-5y","6-10y","> 10y")),
    adm_month = month(admission),
    adm_year = year(admission),
    adm_month_cut = cut(adm_month,
                        breaks = c(0.5,3,6,9,12),
                        labels = c("Jan-Mar","Apr-June","Jul-Sep","Oct-Dec")),
    adm_month_year_cut = str_c(adm_month_cut,adm_year,sep = " ")
  ) 


cases_hcm_1819 %>% 
  group_by(adm_month_year_cut,district2) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(adm_month_year_cut) %>% 
  group_modify(~.x %>% mutate(prevalence = n/sum(n)) %>% 
                 left_join(qhtp, ., by = join_by(varname_2 == district2))) %>%
  ungroup() %>% 
  ggplot() +
  geom_sf(aes(fill = prevalence,geometry = geom),show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    labels = scales::label_percent(),
                                    na.value="white",
                                    name = "Prevalence")+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)+
  facet_wrap(~factor(adm_month_year_cut,
                     levels = c("Apr-June 2018","Jul-Sep 2018","Oct-Dec 2018",
                                "Jan-Mar 2019","Apr-June 2019","Jul-Sep 2019",
                                "Oct-Dec 2019")),
             ncol = 4) +
  labs(title = "Case prevalence in 2018-2019 outbreak")+
  theme_void()+
  theme(legend.position = "bottom")

## hospital catchment using cummulative cases rate

hcm_pop_19 <- census2019 %>% mutate(
  district = district %>% 
    str_replace_all(
      c("Quận 2" = "Thủ Đức",
        "Quận 9" = "Thủ Đức")) %>% 
    str_remove("Quận|Huyện") %>%
    trimws(which = "both") %>% 
    stri_trans_general("latin-ascii") %>% 
    tolower()) %>% 
  group_by(district) %>% 
  summarise(pop = sum(n))


ch2_obs_1819 <- cases_hcm_1819 %>% nrow()

ch2_cum_case_rate <- ch2_obs_1819/sum(hcm_pop_19$pop)

catchment_ch2 <- cases_hcm_1819 %>% 
  group_by(district2) %>% 
  count() %>% 
  left_join(hcm_pop_19,., by = join_by(district == district2)) %>% 
  replace(is.na(.), 0) %>% 
  mutate(expected_cases = pop*ch2_cum_case_rate,
         cum_case_ratio = n/expected_cases,
         lwr = pois.exact(n)$lower/expected_cases,
         upr = pois.exact(n)$upper/expected_cases,
         cut = cut(upr,
                   breaks = c(0,1,2,3,10),
                   labels = c("< 1",">= 1",">= 2",">= 3"),
                   right = F))

catchment_ch2 %>% 
  left_join(qhtp, ., by = join_by(varname_2 == district))  %>% 
  ggplot() +
  geom_sf(aes(fill = cut,geometry = geom),show.legend = T)+
  scale_fill_manual(
    values = c(
      "< 1" = "#FFFFFFFF",
      ">= 1" = "#6BAED6FF",
      ">= 2" = "#2171B5FF",
      ">= 3" = "#08306BFF"
               ),
    name = "95% CI Upper bound of \n cumulative case ratio of each districts"
  )+
  # scale_fill_paletteer_d("colorBlindness::LightBlue2DarkBlue7Steps")+
  # paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
  #                                   labels = scales::label_percent(),
  #                                   na.value="white",
  #                                   name = "Prevalence")+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=2,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)+
  theme_void()

## select cases and sero from CH2 catchment area 

district_consider <- catchment_ch2_1819 %>% 
  filter(cut != "< 1") %>% 
  pull(district) %>% as.character()

incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  group_by(district2,agegr2) %>% 
  count() %>% 
  ggplot(aes(x = agegr2,y = n))+
  geom_col()+
  facet_wrap(~district2)

case_agegr_cm <- incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  group_by(adm_month_year_cut,agegr2) %>% 
  count() %>% 
  ggplot(aes(x = agegr2,y = n))+
  geom_col()+
  facet_wrap(~adm_month_year_cut,ncol = 5)

sp_nd2_cm <- sero_nd2 %>% 
  filter(district2 %in% district_consider) %>% 
  ggplot(aes(x = age,y = pos))+
  geom_smooth(fill = "blue",alpha = 1/10,
              method = mgcv::gam,formula = y ~ s(x, bs = "bs"),
              method.args = list(method = "REML",link = "logit",
                                 family = "binomial"))+
  facet_wrap(~adm_month_year_cut,ncol = 6)

sp_nd2_cm/
  case_agegr_cm


incidence_hcm %>% 
  group_by(agegr2,vax) %>% 
  count() %>% 
  ggplot(aes(x = factor(vax,levels = c("0","1",">=2")),
             y = n))+
  geom_col()+
  facet_wrap(~agegr2)


incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  mutate(adm_week = as.Date(floor_date(admission, "week"))) %>% 
  group_by(district2,adm_week) %>% count() %>% 
  ggplot(aes(x = adm_week,y = n))+
  geom_col()+
  scale_y_continuous(breaks = seq(0,15,by = 5),
                     limits = c(0,15))+
  facet_wrap(~district2)


incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  mutate(
    agegr3 = case_when(
      agegr2 %in% c("< 6m","6-9m","9-12m") ~ "< 1y",
      !(agegr2 %in% c("< 6m","6-9m","9-12m")) ~ agegr2),
    agegr3 = factor(agegr3, levels = c(c("< 1y","1-5y","6-10y","> 10y"))),
    adm_week = as.Date(floor_date(admission, "week"))
    ) %>% 
  group_by(agegr3,adm_week) %>% count() %>%  
  ggplot(aes(x = adm_week,y = n))+
  geom_col()+
  scale_y_continuous(breaks = seq(0,25,by = 5),
                     limits = c(0,25))+
  facet_wrap(~agegr3,ncol = 4)


sero_nd2 %>% 
  filter(district2 %in% district_consider) %>% 
  group_by(adm_month_year_cut,district2) %>% 
  count(pos) %>% 
  pivot_wider(names_from = pos, 
              values_from = n,
              names_prefix = "pos_") %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(total = pos_0+pos_1,
         sp = pos_1/total,
         sneg = 1 - sp) %>% 
  group_by(adm_month_year_cut) %>% 
  group_modify(~.x %>% left_join(qhtp, ., by = join_by(varname_2 == district2))) %>% 
  ungroup() %>% 
  ggplot() +
  geom_sf(aes(fill = sneg,geometry = geom),show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    labels = scales::label_percent(),
                                    na.value="white",
                                    name = "Sero-negative")+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)+
  facet_wrap(~adm_month_year_cut)

sero_nd2 %>% 
  filter(district2 %in% district_consider) %>% 
  group_by(district2) %>% 
  count(pos) %>% 
  pivot_wider(names_from = pos, 
              values_from = n,
              names_prefix = "pos_") %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(total = pos_0+pos_1,
         sp = pos_1/total,
         sneg = 1 - sp) %>% 
  left_join(qhtp, ., by = join_by(varname_2 == district2)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_sf(aes(fill = sneg,geometry = geom),show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    labels = scales::label_percent(),
                                    na.value="white",
                                    name = "Sero-negative")+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)+
  theme_void()


incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  group_by(district2) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(district2) %>% 
  left_join(.,hcm_pop_19, by = join_by(district2 == district)) %>% 
  mutate(prevalence_10000 = (n/pop)*10000) %>% 
  ungroup() %>% 
  left_join(hcm_pop_19, ., by = join_by(district == district2)) %>%
  left_join(qhtp, ., by = join_by(varname_2 == district)) %>% 
  ggplot() +
  geom_sf(aes(fill = prevalence*10000,geometry = geom),show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    # labels = scales::label_percent(),
                                    na.value="white",
                                    name = "Prevalence per 10.000")+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)

sero_cm <- sero_nd2 %>% 
  filter(district2 %in% district_consider) %>% 
  group_by(district2) %>% 
  count(pos) %>% 
  pivot_wider(names_from = pos, 
              values_from = n,
              names_prefix = "pos_") %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(total = pos_0+pos_1,
         sp = pos_1/total,
         sneg = 1 - sp)

sero_cm_23  <- sero_nd2 %>% 
  filter(district2 %in% district_consider & adm_year == 2023) %>% 
  group_by(district2) %>% 
  count(pos) %>% 
  pivot_wider(names_from = pos, 
              values_from = n,
              names_prefix = "pos_") %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(total = pos_0+pos_1,
         sp = pos_1/total,
         sneg = 1 - sp)


cases_cm <- incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  group_by(district2) %>% 
  count() %>% 
  left_join(hcm_pop_19, ., by = join_by(district == district2)) %>%
  na.omit(n) %>%
  mutate(prevalence_10000 = (n/pop)*10000)

left_join(cases_cm,sero_cm,by = join_by(district == district2)) %>% 
  ggplot(aes(x = sneg,y = prevalence*10000, label=district))+
  geom_point()+
  geom_text(hjust=0, vjust=0)


cor_matrix <- left_join(cases_cm,sero_cm,by = join_by(district == district2)) %>% 
  select(prevalence,sneg) 

cor.test(cor_matrix$prevalence,cor_matrix$sneg,method = "pearson") 

library(ggstatsplot)

left_join(cases_cm,sero_cm_23,by = join_by(district == district2)) %>% 
ggscatterstats(
  x = sneg,
  y = prevalence_10000,
  bf.message = FALSE,
  marginal = FALSE, 
  label.var = district,
  xlab = "Sero-negative in 2023",
  ylab = "Prevalence per 10000 people"
)

## sero-negative and prevalence in each age group

incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  select(agegr2) %>% unique()


pop_dis_age_cm <- census2019 %>% mutate(
  district = district %>% 
    str_replace_all(
      c("Quận 2" = "Thủ Đức",
        "Quận 9" = "Thủ Đức")) %>% 
    str_remove("Quận|Huyện") %>%
    trimws(which = "both") %>% 
    stri_trans_general("latin-ascii") %>% 
    tolower(),
  agegr = cut(as.numeric(age2), 
              breaks = c(0,1,5,10,100),
              labels = c("< 1y","1-5y","6-10y","> 10y"))
  ) %>% 
  group_by(district,agegr) %>% 
  summarise(pop = sum(n),.groups = "drop")

pre_dis_agr <- incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  mutate(
    agegr3 = case_when(
      agegr2 %in% c("< 6m","6-9m","9-12m") ~ "< 1y",
      !(agegr2 %in% c("< 6m","6-9m","9-12m")) ~ agegr2),
    agegr3 = factor(agegr3, levels = c(c("< 1y","1-5y","6-10y","> 10y")))) %>% 
  group_by(district2,agegr3) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(.,pop_dis_age_cm,by = join_by(district2 == district,
                                          agegr3 == agegr)) %>% 
  mutate(pre_10000 = (n/pop)*10000)  

plot_dis_age_pre <- function(data,agegr){
  df_plot <- data %>% filter(agegr3 == agegr)
  
  df_plot %>% 
  left_join(qhtp, ., by = join_by(varname_2 == district2)) %>% 
  ggplot() +
  geom_sf(aes(fill = pre_10000,geometry = geom),show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    # labels = scales::label_percent(),
                                    na.value="white",
                                    name = "Prevalence per 10.000 people") +
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)+
  labs(tag = agegr) +
  theme_void()+
  theme(legend.position = "top")
}


plot_dis_age_pre(data = pre_dis_agr,agegr = "< 1y")
plot_dis_age_pre(data = pre_dis_agr,agegr = "1-5y")
plot_dis_age_pre(data = pre_dis_agr,agegr = "6-10y")
plot_dis_age_pre(data = pre_dis_agr,agegr = "> 10y")  

pre_dis_agr

sneg_dis_agr <- sero_nd2 %>% 
  filter(district2 %in% district_consider) %>% 
  mutate(age_gr = cut(as.numeric(age), 
                      breaks = c(0,1,5,10,100),
                      labels = c("< 1y","1-5y","6-10y","> 10y"))
         ) %>% 
  group_by(district2,age_gr) %>% 
  count(pos) %>% 
  pivot_wider(names_from = pos, 
              values_from = n,
              names_prefix = "pos_") %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(total = pos_0+pos_1,
         sp = pos_1/total,
         sneg = 1 - sp)


plot_dis_age_sneg <- function(data,agegr){
  
  data %>% filter(age_gr == agegr) %>% 
    left_join(qhtp, ., by = join_by(varname_2 == district2)) %>% 
    ggplot() +
    geom_sf(aes(fill = sneg,geometry = geom),show.legend = T)+
    paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                      labels = scales::label_percent(),
                                      na.value="white",
                                      name = "Sero negative") +
    geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
    geom_sf(data = tdnd2, shape = 17,
            color = "yellow", size = 1)+
    theme_void()+
    theme(legend.position = "top")
}

plot_dis_age_sneg(data = sneg_dis_agr ,agegr = "< 1y")
plot_dis_age_sneg(data = sneg_dis_agr,agegr = "1-5y")
plot_dis_age_sneg(data = sneg_dis_agr,agegr = "6-10y")
plot_dis_age_sneg(data = sneg_dis_agr,agegr = "> 10y") 


sneg_dis_agr

left_join(pre_dis_agr,sneg_dis_agr,by = join_by(district2,
                                                agegr3 == age_gr)) %>% 
  replace(is.na(.), 0) %>% 
  filter(agegr3 == "6-10y") %>% 
  ggscatterstats(
    # data = cor_matrix,
    x = sneg,
    y = pre_10000,
    bf.message = FALSE,
    marginal = FALSE, 
    label.var = district2,
    xlab = "Sero-negative",
    ylab = "Prevalence per 10.000 people"
  )

## Rt per district and clustering

incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  mutate(adm_week = as.Date(floor_date(admission, "week"))) %>% 
  group_by(district2,adm_week) %>% count() %>% 
  ggplot(aes(x = adm_week,y = n))+
  geom_col()+
  scale_y_continuous(breaks = seq(0,15,by = 5),
                     limits = c(0,15),
                     name = "Incidence")+
  labs(x = "Admission week")+
  facet_wrap(~district2)+
  theme_bw()

library(EpiEstim)

inci_dis_cm <- incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  mutate(adm_week = as.Date(floor_date(admission, "week"))) %>% 
  group_by(district2,adm_week) %>% 
  count() %>% 
  ungroup() %>% 
  set_colnames(c("district","dates","I")) 

inci_dis_cm <- incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  mutate(adm_week = as.Date(floor_date(admission, "week"))) %>%
  group_by(district2,adm_week) %>%
  count() %>% 
  ungroup() %>% 
  group_by(district2) %>%
  complete(
    adm_week = seq.Date(as.Date("2024-04-28"), as.Date("2025-03-30"), by = "week"),
    fill = list(n = 0)
  ) %>% 
  ungroup() %>% 
  set_colnames(c("district","dates","I"))

inci_dis_cm %>% 
  ggplot(aes(x = adm_week,y = n))+
  geom_col()+
  scale_y_continuous(breaks = seq(0,15,by = 5),
                     limits = c(0,15),
                     name = "Incidence")+
  labs(x = "Admission week")+
  facet_wrap(~district2)+
  theme_bw()

inci_dis_cm %>% 
  group_by(district) %>% 
  group_modify(~.x %>% pull(I) %>% 
wallinga_teunis(method="parametric_si",
                config = list(
                  t_start = seq(2,49-6),
                  t_end = seq(8,49),
                  mean_si = 14.5,
                  std_si = 3.25,
                  n_sim = 100)
                ) %>% select(R) %>% data.frame()
)

output <- list()

for (i in 1:length(district_consider)){
  incid <- inci_dis_cm %>% filter(district == district_consider[i])
  incid2 <- inci_dis_cm %>% filter(district == district_consider[i] & month(dates) >= 9)  
  t_start <- seq(2, nrow(incid2)-1)
  t_end <- t_start + 1
  model <-  incid2 %>%  pull(I) %>% 
    estimate_R(method="parametric_si",
                    config = list(
                      t_start = t_start,
                      t_end = t_end,
                      mean_si = 14.5,
                      std_si = 3.25)
    ) 
  
  output[[district_consider[i]]] <- data.frame(date = incid$dates,
                                               I = incid$I,
                                               week = 1:nrow(incid)) %>%
    left_join(.,model$R, by = join_by(week == t_start))
   
}

output %>% bind_rows(.id = "district") %>% 
  ggplot(aes(x = date))+
  geom_col(aes(y = I))+
  geom_line(aes(y = `Mean(R)`))+
  facet_wrap(~district)


inci_dis_cm <- incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  # mutate(adm_week = as.Date(floor_date(admission, "week"))) %>%
  group_by(district2,admission) %>%
  count() %>% 
  ungroup() %>% 
  group_by(district2) %>%
  complete(
    admission = seq.Date(as.Date("2024-05-04"), as.Date("2025-03-30"), by = "day"),
    fill = list(n = 0)
  ) %>% 
  ungroup() %>% 
  set_colnames(c("district","dates","I"))

inci_dis_cm %>% 
  ggplot(aes(x = dates,y = I))+
  geom_col()+
  # scale_y_continuous(breaks = seq(0,15,by = 5),
  #                    limits = c(0,15),
  #                    name = "Incidence")+
  labs(x = "Admission week")+
  facet_wrap(~district)+
  theme_bw()
library(EpiEstim)

inci_dis_cm$I %>% aggregate_inc(dt = 7L)

mean_si <- 14.5
std_si = 3.25
method <- "parametric_si"
config <- make_config(list(mean_si = mean_si,
                           std_si = std_si))

output <- estimate_R(incid = inci_dis_cm %>% filter(district == "1") %>% pull(I),
           dt = 7L,
           dt_out = 7L,
           recon_opt = "naive",
           iter = 10L,
           tol = 1e-6,
           grid = list(precision = 0.001, min = -1, max = 1),
           config = config,
           method = method)

plot(output)

output$R

## multicohort model

centroids <- st_centroid(qhtp)

district_xy <- centroids %>%
  mutate(
    lon = st_coordinates(centroids)[,1],
    lat = st_coordinates(centroids)[,2]
  ) %>%
  select(district = varname_2, lon, lat) %>% 
  as.data.frame() %>% 
  select(-geom)



sero_nd2_cm_23 <- sero_nd2 %>% 
  filter(district2 %in% district_consider & samp_year == 2023) %>% 
  group_by(district2,age_1y) %>% 
  count(pos) %>% 
  pivot_wider(names_from = pos, 
              values_from = n,
              names_prefix = "pos_") %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(total = pos_0+pos_1) %>%
  group_by(district2) %>%
  left_join(.,district_xy, by = join_by(district2 == district))

attach(sero_nd2_cm_23)

y = cbind(pos_1,total)
a = age_1y
lat
lon
library(mgcv)

model <- gam(y~s(a,bs = "cr") + te(lat,lon,bs = "tp"),family=binomial(link="cloglog"))

model2 <- gam(y~s(a,bs = "cr"),family=binomial(link="cloglog"))

model$aic
model2$aic


new_df <- district_xy %>% 
  filter(district %in% district_consider) %>% 
  crossing(a = seq(0, 15, length.out = 512)) %>% 
  select(-district)

sp_dis_cm <- predict(model,new_df,type = "response") %>% 
  tibble(new_df,
         sero = .) %>% 
  left_join(.,district_xy, by = join_by(lat,lon))

# install.packages("plotly")
# library(plotly)

dis_lvls_sp_based <- reorder(sp_dis_cm$district,sp_dis_cm$sero) %>% levels()

sp_dis_cm %>% 
  ggplot(aes(x = a,y = sero))+
  geom_line()+
  scale_y_continuous(name = "Seroprevalence",
                     limits = c(0,1))+
  scale_color_discrete(name = "Districts")+
  facet_wrap(~factor(district,levels = dis_lvls_sp_based))+
  theme_minimal()


sp_dis_cm %>% 
  ggplot(aes(x = a,y = sero,color = factor(district,levels = dis_lvls_sp_based)))+
  geom_line()+
  scale_y_continuous(name = "Seroprevalence",
                     limits = c(0.25,.5))+
  scale_color_discrete(name = "Districts")+
  theme_minimal()

## vaccine coverage
vax_cov_age_dis <- vaxreg_hcmc_measles %>% 
  mutate(age_at_2023 = interval(dob, as.Date("2023-12-31")) / years(1),
         age_round_23 = round(age_at_2023),
         district2 = district %>% 
           trimws(which = "both") %>% 
           stri_trans_general("latin-ascii") %>% 
           tolower()) %>% 
  group_by(district2,age_round_23) %>% 
  count(is_m2) %>% 
  pivot_wider(names_from = is_m2, 
              values_from = n,
              names_prefix = "m2_") %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(m2_covr = m2_1/(m2_0+m2_1))

vax_cov_age_dis %>% View()

new_df2 <- district_xy %>% 
  filter(district %in% district_consider) %>% 
  crossing(a = seq(0, 8)) %>% 
  select(-district)

sp_dis_cm2 <- predict(model,new_df2,type = "response") %>% 
  tibble(new_df2,
         sero = .) %>% 
  left_join(.,district_xy, by = join_by(lat,lon))

data_vaccov_sero <- left_join(sp_dis_cm2,vax_cov_age_dis,by = join_by(district == district2,
                                                  a == age_round_23)) %>% 
  select(-c(m2_0,m2_1))

attach(data_vaccov_sero)
y = cbind(data_vaccov_sero$sero,data_vaccov_sero$m2_covr)
a =   data_vaccov_sero$a
gam(y~te(a))

vaxreg_hcmc_measles$district %>% unique()

district_xy %>% 
  filter(district %in% district_consider) %>% 
  arrange(lon)

dis_lvls_sp_based

ggplotly(sp_dis_plot)


sp_dis_cm %>% 
  mutate(se_neg = 1- sero) %>% 
  group_by(district) %>% 
  summarise(mean_sp = mean(se_neg)) %>% 
  left_join(qhtp, ., by = join_by(varname_2 == district)) %>% 
  ggplot() +
  geom_sf(aes(fill = mean_sp,geometry = geom),show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    labels = scales::label_percent(),
                                    na.value="white",
                                    name = "Mean seroprevalence")+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)



sp_dis_cm %>% 
  mutate(se_neg = 1- sero) %>% 
  group_by(district) %>% 
  summarise(mean_sn = mean(se_neg)) %>% 
  left_join(.,cases_cm, by = join_by(district)) %>% 
  ggplot(aes(y = mean_sn, x = prevalence))+
  geom_point()
library(ggstatsplot)

sp_dis_cm %>% 
  mutate(se_neg = 1- sero) %>% 
  group_by(district) %>% 
  summarise(mean_sn = mean(se_neg)) %>% 
  left_join(.,cases_cm, by = join_by(district))  %>% 
  ggscatterstats(
    # data = cor_matrix,
    x = mean_sn,
    y = prevalence,
    bf.message = FALSE,
    marginal = FALSE, 
    label.var = district,
    xlab = "Sero-negative in 2023",
    ylab = "Prevalence per 10000 people"
  )


pre_dis_agr %>% 
  group_by(agegr3) %>% 
  ggplot(aes(x = factor(district2,levels = dis_lvls_sp_based), 
             y = pre_10000))+
  geom_col()+
  facet_wrap(~agegr3,ncol = 4)+
  labs(x = "Districts", y = "Prevalence per 10.000 people")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5))


pre_dis_agr %>% 
  group_by(agegr3) %>% 
  ggplot(aes(x = factor(district2,levels = dis_lvls_sp_based), 
             y = n))+
  geom_col()+
  facet_wrap(~agegr3,ncol = 4)+
  labs(x = "Districts", y = "Incidence")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5))

## vaccine coverage in 2023

vax_cov_age_dis

vaxreg_hcmc_measles %>% 
  mutate(age_at_2023 = interval(dob, as.Date("2023-12-31")) / years(1),
         age_at_m2 = interval(dob, date_m2) / years(1),
         age_round_23 = round(age_at_2023),
         district2 = district %>% 
           trimws(which = "both") %>% 
           stri_trans_general("latin-ascii") %>% 
           tolower()
         ) %>% 
  filter(district2 %in% district_consider)

vax_cov_age_dis %>% 
  filter(district2 %in% district_consider) %>% 
  filter(age_round_23 != 0) %>%
  mutate(age_from_m2 = age_round_23-1.5,
         adjust_cvr = exp(-0.008*(age_from_m2))*0.977*m2_covr) %>% 
  ggplot(aes(x = factor(district2,levels = dis_lvls_sp_based),
             y = 1 - adjust_cvr))+
  geom_col()+
  facet_wrap(~age_round_23, ncol = 4)+
  scale_y_continuous(limits = c(0,1),
                     labels = scales::label_percent())+
  labs(x = "Districts", y = "The proportion of susceptible until the end of 2023")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5))

  
hcm_pop_19_chil <- census2019 %>% mutate(
  district = district %>% 
    str_replace_all(
      c("Quận 2" = "Thủ Đức",
        "Quận 9" = "Thủ Đức")) %>% 
    str_remove("Quận|Huyện") %>%
    trimws(which = "both") %>% 
    stri_trans_general("latin-ascii") %>% 
    tolower()) %>% 
  filter(age2 <= 15) %>% 
  group_by(district) %>% 
  summarise(pop = sum(n))
  