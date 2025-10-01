meases_1812_3hos <- measles_1825_eoc %>% 
  clean_names() %>% 
  filter(ten_bv %in% c("Bệnh viện Nhi đồng 1",
                       "Bệnh viện Nhi Đồng 1",
                       "BV Nhi Đồng 1",
                       "Bệnh viện Nhi đồng thành phố",
                       "Bệnh viện Nhi đồng 2",
                       "Bệnh viện Nhi Đồng 2")) %>% 
  mutate(
    district = quan_huyen %>% 
      str_replace_all(
        c("Quận 2" = "Thủ Đức",
          "Quận 9" = "Thủ Đức")) %>% 
      str_remove("Quận|Huyện|Thành phố|QUẬN") %>%
      trimws(which = "both") %>% 
      stri_trans_general("latin-ascii") %>% 
      tolower(),
    admission = as.Date(ngay_nv,format = "%d/%m/%Y"),
    hospital = ten_bv %>% 
      str_replace_all(
        c("BV Nhi Đồng 1" = "Bệnh viện Nhi Đồng 1")) %>%
      trimws(which = "both") %>% 
      stri_trans_general("latin-ascii") %>% 
      tolower() %>% 
      factor(levels = c("benh vien nhi dong 1",
                        "benh vien nhi dong 2",
                        "benh vien nhi dong thanh pho"),
             labels = c("Children's Hospital 1",
                        "Children's Hospital 2",
                        "City Children's Hospital")),
    out_year = case_when(
      nam_nv %in% c(2018,2019) ~ "2018-2019",
      nam_nv %in% c(2024,2025) ~ "2024-2025"
    ),
    dob = as.Date(ngay_sinh,format="%d/%m/%Y"),
    age2 = interval(dob, admission) / years(1),
    age_gr = cut(age2,
                 breaks = c(0,5,10,15,100),
                 labels = c("0-5","5-10","10-15",">15"))
  ) %>% 
  filter(district %in% qhtp$varname_2) %>%
  distinct(.keep_all = TRUE)

epicurve <- meases_1812_3hos %>% 
  group_by(admission) %>% 
  count() %>% 
  filter(month(admission) >= 4 & year(admission) >=2024) %>% 
  ggplot(aes(x = admission,y = n))+
  geom_col()+
  scale_y_continuous(name = "Cases per day")+
  scale_x_date(name = "Day",breaks = "1 month",date_labels = "%b %Y")+
  geom_vline(xintercept = as.Date("2024-04-30"))+
  theme_minimal()


sero_ch1a <- sero_ch1 %>% 
  select(commune,district,doc,moc,yoc,dob,mob,yob,pos,age) %>% 
  na.omit(district) %>% 
  mutate(district = district %>%
           trimws(which = "both") %>% 
           stri_trans_general("latin-ascii") %>% 
           tolower(),
         col_time = make_date(yoc,moc,doc),
         hospital = rep("CH1"),
         dob = make_date(yob,mob,dob))

sero_ch2_cch <- sero  %>% 
  select(pos,commune,district,age,age_1y,age_5y,sampling_period,hospital,doc,dob) %>% 
  mutate(
    district = district %>% 
      stri_trans_general("latin-ascii") %>% 
      str_remove("Tp|^0") %>%
      trimws(which = "both") %>% 
      tolower() ,
    samp_month = month(sampling_period),
    samp_year = year(sampling_period),
    col_time = doc
  )


sero_3bv <- rbind(sero_ch1a[,c("age","commune","district","pos","col_time","dob","hospital")],
      sero_ch2_cch[,c("age","commune","district","pos","col_time","dob","hospital")]) %>% 
  mutate(age2 = round(age)) %>% 
  left_join(.,district_xy, by = join_by(district)) %>% 
  mutate(yoc = year(col_time),
         moc = month(col_time),
         doc = day(col_time),
         age_at_apr24 = interval(dob, as.Date("2024-04-30")) / years(1),
         age424 = round(age_at_apr24))
  


## 09/2022 - 4/2024

### 1st assumption: life long immunity
library(mgcv)


age_profile <- function(data, age_values = seq(0, 16, le = 512), ci = .95) {
  model <- data %>% 
    group_by(age424) %>%
    count(pos) %>% 
    pivot_wider(names_from = pos,
                values_from = n,
                names_prefix = "pos_") %>% 
    replace(is.na(.), 0) %>% 
    ungroup() %>% 
    mutate(total = pos_0+pos_1,
           sp = pos_1/total,
           sneg = 1 - sp) %>% 
    gam(cbind(pos_1,total)~s(age424,bs = "cr"),
        family = binomial(link = "cloglog"),
        data = .)
  
  link_inv <- family(model)$linkinv
  df <- nrow(data) - length(coef(model))
  p <- (1 - ci) / 2
  
  model |> 
    predict(list(age424 = age_values), se.fit = TRUE) %>%
    c(list(age = age_values), .) |> 
    as_tibble() |> 
    mutate(lwr = link_inv(fit + qt(    p, df) * se.fit),
           upr = link_inv(fit + qt(1 - p, df) * se.fit),
           fit = link_inv(fit)) |> 
    select(- se.fit)
}

age_profile(sero_3bv) %>% 
  mutate(sus = 1-fit,
         lwr_s = 1-lwr,
         upr_s = 1-upr) %>%  
  ggplot(aes(x = age,y = sus))+
  geom_line()+
  geom_ribbon(aes(ymin = lwr_s,ymax = upr_s),fill = "blue",alpha = 0.4)+
  scale_y_continuous(limits = c(0,1),
                     name = "Proportion of susceptibility")+
  scale_x_continuous(limits = c(0,16),
                     breaks = seq(0,16,by=2),
                     name = "Age (years)")+
  theme_bw()

age_profile_apr24|epicurve

## spatial cordinate

centroids <- st_centroid(qhtp)

district_xy <- centroids %>%
  mutate(
    lon = st_coordinates(centroids)[,1],
    lat = st_coordinates(centroids)[,2]
  ) %>%
  select(district = varname_2, lon, lat) %>% 
  as.data.frame() %>% 
  select(-geom)

commune_xy %>% 
  mutate(district = district %>%
           str_replace_all(
             c("Quận 2" = "Thủ Đức",
               "Quận 9" = "Thủ Đức")) %>%
           str_remove("Quận|Huyện") %>%
           trimws(which = "both") %>%
           stri_trans_general("latin-ascii") %>%
           tolower(),
         commune = commune %>% 
           str_remove("ward") %>% 
           trimws(which = "both"))


## contact matrix
library(contactdata)
countries <- c("Vietnam")
contact_data <- contact_df_countries(countries,
                                     location = "all",
                                     geographic_setting = c("urban"),
                                     data_source = c("2020"))

ggplot(contact_data, aes(x = age_from, y = age_to, fill = contact)) +
  geom_tile() +
  facet_wrap(~country) +
  coord_equal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_viridis_c()

## age 4/2024

model2 <- sero_3bv %>% 
  # group_by(district,age2) %>%
  group_by(age2) %>%
  count(pos) %>% 
  pivot_wider(names_from = pos,
              values_from = n,
              names_prefix = "pos_") %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(total = pos_0+pos_1,
         sp = pos_1/total,
         sneg = 1 - sp) %>% 
  # left_join(.,district_xy, by = join_by(district)) %>%
  gam(cbind(pos_1,total)~s(age2,bs="cr",k = 15),
      family = binomial(link = "cloglog"),
      data = .)

plot(model2, ylim = c(0, 1))

predict(model2,
        list(age2 = seq(0, 15, length.out = 512)),
        type = "response") %>% 
  as.tibble() %>% 
  mutate(age = seq(0, 15, length.out = 512)) %>% 
  ggplot(aes(x = age, y = value))+
  geom_line()+
  scale_y_continuous(limits = c(0,1))
  
model2 %>% str()

model2$coefficients

new_df2 <- district_xy %>% 
  # filter(district %in% district_consider) %>% 
  crossing(age2 = seq(0, 15, length.out = 512)) %>% 
  select(-district)

sp_dis_cm2 <- predict(model2,new_df2,type = "response") %>% 
  tibble(new_df2,
         sero = .) %>% 
  left_join(.,district_xy, by = join_by(lat,lon))


dis_lvls_sp_based <- reorder(sp_dis_cm2$district,sp_dis_cm2$sero) %>% levels()

sp_dis_cm2 %>% 
  ggplot(aes(x = age2,
             y = 1-sero,
             color = factor(district,levels = dis_lvls_sp_based)))+
  geom_line()+
  scale_y_continuous(name = "Susceptibility",limits = c(0,1))+
  scale_color_discrete(name = "Districts",
                       guide = guide_legend(reverse = TRUE))+
  labs(x = "Age")+
  theme_minimal()


## linelisting fit

model <- gam(pos~s(age,bs="tp")+s(lon,lat,bs = "tp",k=10),
    family = binomial(link = "cloglog"),
    data = sero_3bv)

new_df <- district_xy %>% 
  filter(district %in% district_consider) %>% 
  crossing(age = seq(0, 15, length.out = 512)) %>% 
  select(-district)


sp_dis_cm <- predict(model,new_df,type = "response") %>% 
  tibble(new_df,
         sero = .) %>% 
  left_join(.,district_xy, by = join_by(lat,lon))

sp_dis_cm %>% 
  ggplot(aes(x = age,
             y = sero,
             color = factor(district,levels = dis_lvls_sp_based)))+
  geom_line()+
  scale_y_continuous(name = "Seronegative in 2023",limits = c(0,1))+
  scale_color_discrete(name = "Districts",
                       guide = guide_legend(reverse = TRUE))+
  labs(x = "Age")+
  theme_minimal()


attach(sero_3bv)
y = cbind(pos_1,total)
a = age2
library(mgcv)

model <- gam(y~s(a,bs = "cr") + te(lat,lon,bs = "tp"),
             family=binomial(link="cloglog"))

new_df <- district_xy %>% 
  filter(district %in% district_consider) %>% 
  crossing(a = seq(0, 15, length.out = 512)) %>% 
  select(-district)

sp_dis_cm <- predict(model,new_df,type = "response") %>% 
  tibble(new_df,
         sero = .,
         sneg = 1 - sero) %>% 
  left_join(.,district_xy, by = join_by(lat,lon))




