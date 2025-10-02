# Correlation between sero-negative and prevalence in CH2 catchment area

Now we only analyse districts in CH2 catchment area

```{r}
#| fig-width: 8
#| fig-height: 6 
#| out-width: "100%"

district_consider <- catchment_ch2_1819 %>% 
  filter(cut != "< 1") %>% 
  pull(district) %>% as.character()

sero_neg_cm <- sero_nd2 %>% 
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
  theme_void()+
  theme(legend.position = "top",
        legend.key.width =  unit(1, "cm"))

prevalence_cm <- incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  group_by(district2) %>% 
  count() %>% 
  left_join(hcm_pop_19, ., by = join_by(district == district2)) %>%
  na.omit(n) %>%
  mutate(prevalence = n/pop) %>%
  left_join(qhtp, ., by = join_by(varname_2 == district)) %>% 
  ggplot() +
  geom_sf(aes(fill = prevalence*10000,geometry = geom),show.legend = T)+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    # labels = scales::label_percent(),
                                    na.value="white",
                                    name = "Prevalence per 10.000 people")+
  geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
  geom_sf(data = tdnd2, shape = 17,
          color = "yellow", size = 1)+
  theme_void()+
  theme(legend.position = "top")

sero_neg_cm | prevalence_cm
```

## Correlation test {.unnumbered}

### All sero-sample {.unnumbered}

I plotted prevalence and sero-negativity to see whether there is a linear relationship between these variables

```{r}
#| fig-width: 8
#| fig-height: 5 
#| out-width: "100%"

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

cases_cm <- incidence_hcm %>% 
  filter(district2 %in% district_consider) %>% 
  group_by(district2) %>% 
  count() %>% 
  left_join(hcm_pop_19, ., by = join_by(district == district2)) %>%
  na.omit(n) %>%
  mutate(prevalence_10000 = (n/pop)*10000)

left_join(cases_cm,sero_cm,by = join_by(district == district2)) %>% 
  ggscatterstats(
    # data = cor_matrix,
    x = sneg,
    y = prevalence_10000,
    bf.message = FALSE,
    marginal = FALSE, 
    label.var = district,
    xlab = "Sero-negative before 2024",
    ylab = "Prevalence per 10000 people"
  )
```

::: callout-note
The plot shows a linear trend but no significant correlation between sero-negativity and prevalence across districts in the CH2 catchment area.
:::
  
  ### Sero-sample in 2023 {.unnumbered}
  
  ```{r}
#| fig-width: 8
#| fig-height: 5 
#| out-width: "100%"

sero_cm_23  <- sero_nd2 %>% 
  filter(district2 %in% district_consider & samp_year == 2023) %>% 
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
```

## Age group {.unnumbered}

```{r}
## district population per age group
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


## prevalence per 10000 of districts
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

## sero negative per district

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


## plot function

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
    # labs(tag = agegr) +
    theme_void()+
    theme(legend.position = "top")
}

plot_dis_age_sneg <- function(data,agegr){
  
  data %>% filter(age_gr == agegr) %>% 
    left_join(qhtp, ., by = join_by(varname_2 == district2)) %>% 
    ggplot() +
    geom_sf(aes(fill = sneg,geometry = geom),show.legend = T)+
    paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                      labels = scales::label_percent(),
                                      na.value="white",
                                      name = "Sero negative \n before 2024") +
    geom_sf_text(aes(label = nl_name_2,geometry = geom),size=1.5,color = "black")+
    geom_sf(data = tdnd2, shape = 17,
            color = "yellow", size = 1)+
    theme_void()+
    theme(legend.position = "top",
          legend.key.width =  unit(1, "cm"))
}

```

::: {.panel-tabset .nav-pills}
## \< 1 year old

```{r}
plot_dis_age_pre(data = pre_dis_agr,agegr = "< 1y") | plot_dis_age_sneg(data = sneg_dis_agr ,agegr = "< 1y")

## correlation plot
left_join(pre_dis_agr,sneg_dis_agr,by = join_by(district2,
                                                agegr3 == age_gr)) %>% 
  replace(is.na(.), 0) %>% 
  filter(agegr3 == "< 1y") %>% 
  ggscatterstats(
    # data = cor_matrix,
    x = sneg,
    y = pre_10000,
    bf.message = FALSE,
    marginal = FALSE, 
    label.var = district2,
    xlab = "Sero-negative before 2024",
    ylab = "Prevalence per 10.000 people"
  )
```

## 1 - 5 years old

```{r}
plot_dis_age_pre(data = pre_dis_agr,agegr = "1-5y") | plot_dis_age_sneg(data = sneg_dis_agr,agegr = "1-5y")

## correlation plot
left_join(pre_dis_agr,sneg_dis_agr,by = join_by(district2,
                                                agegr3 == age_gr)) %>% 
  replace(is.na(.), 0) %>% 
  filter(agegr3 == "1-5y") %>% 
  ggscatterstats(
    # data = cor_matrix,
    x = sneg,
    y = pre_10000,
    bf.message = FALSE,
    marginal = FALSE, 
    label.var = district2,
    xlab = "Sero-negative before 2024",
    ylab = "Prevalence per 10.000 people"
  )
```

## 6 - 10 years old

```{r}
plot_dis_age_pre(data = pre_dis_agr,agegr = "6-10y") | plot_dis_age_sneg(data = sneg_dis_agr,agegr = "6-10y")

## correlation plot
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
    xlab = "Sero-negative before 2024",
    ylab = "Prevalence per 10.000 people"
  )
```

## \> 10 years old

```{r}
plot_dis_age_pre(data = pre_dis_agr,agegr = "> 10y") | plot_dis_age_sneg(data = sneg_dis_agr,agegr = "> 10y") 

## correlation plot
left_join(pre_dis_agr,sneg_dis_agr,by = join_by(district2,
                                                agegr3 == age_gr)) %>% 
  replace(is.na(.), 0) %>% 
  filter(agegr3 == "> 10y") %>% 
  ggscatterstats(
    # data = cor_matrix,
    x = sneg,
    y = pre_10000,
    bf.message = FALSE,
    marginal = FALSE, 
    label.var = district2,
    xlab = "Sero-negative before 2024",
    ylab = "Prevalence per 10.000 people"
  )
```
:::
  
  # Age-dependent susceptibility profiles in 2023
  
  ## Serological data
  
  In this section, I followed the [@hens2015] article. Using GAM model fitted to seroprevalence data of districts in CH2 catchment area with cloglog link function. The smooth function $s_1$ was one-dimensional cubic splines, and $te$ referring to tensor product thin-plate regression splines allowing for differential smoothing along the two dimensions.

$$
  seropositive \sim s_1(age) + te(x,y)
$$ With x, y are spatial coordinates.

```{r}
## extract x and y coordinates

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
```

:::: {.panel-tabset .nav-pills}
## Model

```{r}
library(mgcv)
library(plotly)

attach(sero_nd2_cm_23)
y = cbind(pos_1,total)
a = age_1y

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

dis_lvls_sp_based <- reorder(sp_dis_cm$district,sp_dis_cm$sneg) %>% levels()

sp_dis_cm %>% 
  ggplot(aes(x = a,
             y = sneg,
             color = factor(district,levels = dis_lvls_sp_based)))+
  geom_line()+
  scale_y_continuous(name = "Seronegative in 2023")+
  scale_color_discrete(name = "Districts",
                       guide = guide_legend(reverse = TRUE))+
  labs(x = "Age")+
  theme_minimal()
```

::: callout-note
The order of legend is similiar with the color line
:::
  
  ## Data
  
  ```{r}
sero_nd2_cm_23 %>% head(20)
```
::::
  
  ## Vaccine coverage data
  
  Follow [@hens2015], I use the model to estimate age-dependent susceptibility profiles in 2023.

$$
  1-s_{(a)} = e^{-\gamma_2 \times (a-{age\ at \ dose \ 2})} \times \rho v_2
$$
  
  $\rho$ is the seroconversion rate = 0.977 (95% CI: 0.959 − 0.990), $\gamma_2$ is the decay rates of vaccine-induced immunity related to dose 2, age at dose 2 is 18 month = 1.5 years. $v_2$ is the measles dose 2 vaccine coverage of districts

```{r}
#| fig-width: 10
#| fig-height: 5 
#| out-width: "100%"

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

vax_cov_age_dis %>% 
  filter(district2 %in% district_consider) %>% 
  filter(age_round_23 != 0) %>%
  mutate(age_from_m2 = age_round_23-1.5,
         adjust_cvr = exp(-0.008*(age_from_m2))*0.977*m2_covr) %>% 
  ggplot(aes(x = factor(district2,levels = dis_lvls_sp_based),
             y = 1 - adjust_cvr))+
  geom_col()+
  facet_wrap(~age_round_23, ncol = 4,
             labeller = labeller(age_round_23 = function(x) paste0(x, " year-old")))+
  scale_y_continuous(limits = c(0,1),
                     labels = scales::label_percent())+
  labs(x = "Districts", y = "The proportion of susceptible until the end of 2023")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5))
```

::: callout-note
The order of districts in x axis (left to right) follow the increase of seronegative in the plot above
:::
  
  ## Prevalence per 10000 people of each districts
  
  Let see the age distribution of measles prevalence in CH2 cathment

```{r}
#| fig-width: 10
#| fig-height: 5 
#| out-width: "100%"

pre_dis_agr %>% 
  group_by(agegr3) %>% 
  ggplot(aes(x = factor(district2,levels = dis_lvls_sp_based), 
             y = pre_10000))+
  geom_col()+
  facet_wrap(~agegr3,ncol = 4)+
  labs(x = "Districts", y = "Prevalence per 10.000 people")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5))
```

::: callout-note
The order of districts in x axis (left to right) follow the increase of seronegative in the plot above
:::
  