meases_1812_3hos$nam_nv %>% unique()
meases_1812_3hos$hospital %>% unique()
measles_1825_eoc$`Năm NV` %>% unique()

## 18-19
n_hcdc_1819 <- measles_1825_eoc %>% 
  clean_names() %>%   
  mutate(dob = as.Date(ngay_sinh,format = "%d/%m/%Y"),
         admission = as.Date(ngay_nv,format = "%d/%m/%Y"),
         age = interval(dob, admission) / years(1)) %>% 
  filter(nam_nv %in% c(2018,2019) & age <= 15) %>% 
  nrow()

n_ch1_1819 <- meases_1812_3hos %>%
  mutate(dob = as.Date(ngay_sinh,format = "%d/%m/%Y"),
         age = interval(dob, admission) / years(1)) %>% 
  filter(hospital == "BV Nhi đồng 1" &
           nam_nv %in% c(2018,2019) &
           age <= 15) %>% 
  nrow()

n_cch_1819 <- meases_1812_3hos %>%
  mutate(dob = as.Date(ngay_sinh,format = "%d/%m/%Y"),
         age = interval(dob, admission) / years(1)) %>% 
  filter(hospital == "BV Nhi đồng \nThành phố" &
           nam_nv %in% c(2018,2019) &
           age <= 15) %>% 
  nrow()


n_ch2_1819 <- meases_1812_3hos %>%
  mutate(dob = as.Date(ngay_sinh,format = "%d/%m/%Y"),
         age = interval(dob, admission) / years(1)) %>% 
  filter(hospital == "BV Nhi đồng 2" &
           nam_nv %in% c(2018,2019) &
           age <= 15) %>% 
  nrow()

(n_ch1_1819)/n_hcdc_1819*100
(n_ch2_1819)/n_hcdc_1819*100
(n_cch_1819)/n_hcdc_1819*100
(n_ch2_1819+n_ch1_1819+n_cch_1819)/n_hcdc_1819*100


#### 24-25

n_hcdc_2425 <- measles_1825_eoc %>% 
  clean_names() %>%   
  mutate(dob = as.Date(ngay_sinh,format = "%d/%m/%Y"),
         admission = as.Date(ngay_nv,format = "%d/%m/%Y"),
         age = interval(dob, admission) / years(1)) %>% 
  filter(nam_nv %in% c(2024,2025) & age <= 15) %>% 
  nrow()

n_ch1_2425 <- meases_1812_3hos %>%
  mutate(dob = as.Date(ngay_sinh,format = "%d/%m/%Y"),
         age = interval(dob, admission) / years(1)) %>% 
  filter(hospital == "BV Nhi đồng 1" &
           nam_nv %in% c(2024,2025) &
           age <= 15) %>% 
  nrow()

n_cch_2425 <- meases_1812_3hos %>%
  mutate(dob = as.Date(ngay_sinh,format = "%d/%m/%Y"),
         age = interval(dob, admission) / years(1)) %>% 
  filter(hospital == "BV Nhi đồng \nThành phố" &
           nam_nv %in% c(2024,2025) &
           age <= 15) %>% 
  nrow()


n_ch2_2425 <- meases_1812_3hos %>%
  mutate(dob = as.Date(ngay_sinh,format = "%d/%m/%Y"),
         age = interval(dob, admission) / years(1)) %>% 
  filter(hospital == "BV Nhi đồng 2" &
           nam_nv %in% c(2024,2025) &
           age <= 15) %>% 
  nrow()

(n_ch1_2425)/n_hcdc_2425*100
(n_ch2_2425)/n_hcdc_2425*100
(n_cch_2425)/n_hcdc_2425*100
(n_ch2_2425+n_ch1_2425+n_cch_2425)/n_hcdc_2425*100
