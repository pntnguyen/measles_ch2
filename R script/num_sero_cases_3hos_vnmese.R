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
             labels = c("BV Nhi đồng 1",
                        "BV Nhi đồng 2",
                        "BV Nhi đồng \nThành phố")),
    out_year = case_when(
      nam_nv %in% c(2018,2019) ~ "2018-2019",
      nam_nv %in% c(2024,2025) ~ "2024-2025"
    )
  ) %>% 
  filter(district %in% qhtp$varname_2) %>%
  distinct(.keep_all = TRUE)

## dec 2022 - Apr 2024
num_sero_ch1 <- sero_ch1 %>% 
  select(district,doc,moc,yoc) %>% 
  na.omit(district) %>% 
  mutate(district = district %>%
           trimws(which = "both") %>% 
           stri_trans_general("latin-ascii") %>% 
           tolower()) %>% 
  group_by(district) %>% 
  count() %>% 
  mutate(hospital = rep("nd1"),
         col = rep("Dec 2022 - Apr 2024"))%>% 
  ungroup()

num_sero_ch2_cch <- sero  %>% 
  select(pos,district,age,age_1y,age_5y,sampling_period,hospital) %>% 
  mutate(
    district = district %>% 
      stri_trans_general("latin-ascii") %>% 
      str_remove("Tp|^0") %>%
      trimws(which = "both") %>% 
      tolower() ,
    samp_month = month(sampling_period),
    samp_year = year(sampling_period)
  ) %>% 
  group_by(hospital,district) %>% 
  count() %>% 
  mutate(col = rep("Dec 2022 - Dec 2023")) %>% 
  ungroup()

text_size <- 20

num_cases_3hos <- meases_1812_3hos %>% 
  filter(!is.na(out_year)) %>% 
  group_by(hospital,district,out_year) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(hospital,out_year) %>% 
  group_modify(~.x %>% left_join(qhtp, ., by = join_by(varname_2 == district))) %>% 
  ungroup() %>% 
  ggplot() +
  geom_sf(aes(fill = n,geometry = geom),
          show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    na.value="white",
                                    name = "Số ca bệnh nhập viện")+
  # geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  # geom_sf(data = tdnd2, shape = 17,
  #         color = "yellow", size = 1)+
  facet_grid(hospital ~ out_year,
             switch = "y") +
  theme_void()+
  theme(legend.position = "bottom",
        legend.key.width =  unit(1.5, "cm"),
        legend.title = element_text(size = text_size),
        legend.text = element_text(size = text_size),
        strip.text.x = element_text(size = text_size),
        strip.text.y = element_text(size = text_size))


num_sero_3hos_p <- rbind(num_sero_ch1,num_sero_ch2_cch) %>% 
  group_by(hospital) %>% 
  group_modify(~.x %>% left_join(qhtp, ., by = join_by(varname_2 == district))) %>% 
  ungroup() %>% 
  ggplot() +
  geom_sf(aes(fill = n,geometry = geom),
          show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    na.value="white",
                                    name = "Số mẫu máu \nthu thập")+
  # geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  # geom_sf(data = tdnd2, shape = 17,
  #         color = "yellow", size = 1)+
  facet_wrap(~factor(hospital,
                     levels = c("nd1","Bv Nhi Dong 2","Bv Nhi Dong Tp"),
                     labels = c("12/2022-4/2024",
                                "12/2022-12/2023",
                                " 12/2022-12/2023")),
             ncol = 1) +
  theme_void()+
  theme(legend.title = element_text(size = text_size),
        legend.text = element_text(size = text_size),
        strip.text.x = element_text(size = text_size-3.5),
        strip.text.y = element_text(size = text_size))



num_cases_3hos|num_sero_3hos_p

ggsave("./plot/fig1.svg",
       width = 12,height = 8,dpi = 500)
