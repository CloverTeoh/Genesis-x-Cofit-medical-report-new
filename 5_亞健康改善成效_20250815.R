library(pacman)
pacman::p_load(tidyr, googleVis, magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, extraInserts, ggvenn, ggplot2, plotly, knitr, kableExtra, openxlsx, lubridate, cowplot, ggpubr, webshot)

setwd("/Users/clover_teoh/Documents/cofit/clean_data")
getwd()

raw_data <- read.csv("final_analysis_ready_jason_20250710.csv")
bp_data <- read_excel("血壓前後測_20250710.xlsx")
clinic_list <- read_excel("客戶_診所_20250711.xlsx")
food_light_data <- read.csv("final_analysis_ready_foodlights_data.csv")

setwd("/Users/clover_teoh/Documents/cofit/gitbook")
getwd()

############################# data cleaning ##################
bp_data <- bp_data %>%
  select(-started_at, -finished_at)

food_light_data <- food_light_data %>%
  select(-started_at, -adjusted_end)

raw_data <- raw_data %>%
  select(-mean_calories, -mean_carbohydrate, -mean_protein, -mean_fat)

raw_data <- raw_data %>%
  left_join(bp_data, by = c("client_id", "class_id"))

raw_data <- raw_data %>%
  left_join(food_light_data, by = c("client_id", "class_id"))


nrow(raw_data)

raw_data <- raw_data %>%
  mutate(
    birthday = case_when(
      client_id == 659338 ~ "1971-07-07",
      client_id == 681404 ~ "1958-03-26",
      TRUE ~ birthday  # 其他人保持原樣
    ),
    birthday = ymd(birthday)  # 轉換為 Date 格式
  )

raw_data <- raw_data %>%
  mutate(
    age = floor(time_length(interval(ymd(birthday), ymd(started_at)), "years"))
  )


raw_data <- raw_data %>% 
  rename(finished_at = adjusted_end,
         systolic_blood_pressure_baseline = systolic_blood_pressure_w0,
         systolic_blood_pressure_endpoint = systolic_blood_pressure_w8,
         diastolic_blood_pressure_baseline = diastolic_blood_pressure_w0,
         diastolic_blood_pressure_endpoint = diastolic_blood_pressure_w8)



## define disease #######
## define Obesity by BMI
raw_data$bmi_gp_base <- raw_data$bmi_baseline %>% cut(c(0,18.5,24,27, 30, 35 ,100), c("Underweight", "Normal", "Overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)
raw_data$bmi_gp_end <- raw_data$bmi_endpoint %>% cut(c(0,18.5,24,27, 30, 35 ,100), c("Underweight", "Normal", "Overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)


# define DM base
raw_data <- raw_data %>%
  mutate(DM_base = case_when(
    hba1c_baseline >= 6.5 |
      glucose_ac_baseline >= 126 |
      glucose_pc_2hr_baseline >= 200 ~ "DM",
    
    hba1c_baseline >= 5.7 & hba1c_baseline < 6.5 |
      glucose_ac_baseline >= 100 & glucose_ac_baseline < 126 |
      glucose_pc_2hr_baseline >= 140 & glucose_pc_2hr_baseline < 200 ~ "Pre-DM",
    
    !is.na(hba1c_baseline) &
      !is.na(glucose_ac_baseline) ~ "Normal",
    
    TRUE ~ NA_character_
  ))



## define DM end
raw_data <- raw_data %>%
  mutate(DM_end = case_when(
    hba1c_endpoint >= 6.5 |
      glucose_ac_endpoint >= 126 |
      glucose_pc_2hr_endpoint >= 200 ~ "DM",
    
    hba1c_endpoint >= 5.7 & hba1c_endpoint < 6.5 |
      glucose_ac_endpoint >= 100 & glucose_ac_endpoint < 126 |
      glucose_pc_2hr_endpoint >= 140 & glucose_pc_2hr_endpoint < 200 ~ "Pre-DM",
    
    !is.na(hba1c_endpoint) &
      !is.na(glucose_ac_endpoint) ~ "Normal",
    
    TRUE ~ NA_character_
  ))




#define_metabolic syndrome
raw_data <- raw_data %>%
  mutate(
    met_abdominal_base = ifelse((gender == "male" & waist_circumference_baseline  >= 90) |
                                  (gender == "female" & waist_circumference_baseline >= 80), 1, 0),
    
    met_tg_base = ifelse(triglyceride_baseline >= 150, 1, 0),
    
    met_hdl_base = ifelse((gender == "male" & high_density_lipoprotein_baseline < 40) |
                            (gender == "female" & high_density_lipoprotein_baseline < 50), 1, 0),
    
    met_bp_base = ifelse(systolic_blood_pressure_baseline >= 130 | diastolic_blood_pressure_baseline >= 85, 1, 0),
    
    met_fbg_base = ifelse(glucose_ac_baseline >= 100, 1, 0)
  ) %>%
  rowwise() %>%
  mutate(
    met_sum_base = sum(c_across(c(met_abdominal_base, met_tg_base, met_hdl_base, met_bp_base, met_fbg_base)), na.rm = TRUE),
    met_n_base = sum(!is.na(c_across(c(met_abdominal_base, met_tg_base, met_hdl_base, met_bp_base, met_fbg_base)))),
    metax_base = case_when(
      met_sum_base >= 3 ~ "Metabolic Syndrome",
      met_n_base < 5 ~ NA_character_,
      TRUE ~ "Normal"
    )
  ) %>%
  ungroup()


raw_data <- raw_data %>%
  mutate(
    met_abdominal_end = ifelse((gender == "male" & waist_circumference_endpoint  >= 90) |
                                 (gender == "female" & waist_circumference_endpoint >= 80), 1, 0),
    
    met_tg_end = ifelse(triglyceride_endpoint >= 150, 1, 0),
    
    met_hdl_end = ifelse((gender == "male" & high_density_lipoprotein_endpoint < 40) |
                           (gender == "female" & high_density_lipoprotein_endpoint < 50), 1, 0),
    
    met_bp_end = ifelse(systolic_blood_pressure_endpoint >= 130 | diastolic_blood_pressure_endpoint >= 85, 1, 0),
    
    met_fbg_end = ifelse(glucose_ac_endpoint >= 100, 1, 0)
  ) %>%
  rowwise() %>%
  mutate(
    met_sum_end = sum(c_across(c(met_abdominal_end, met_tg_end, met_hdl_end, met_bp_end, met_fbg_end)), na.rm = TRUE),
    met_n_end = sum(!is.na(c_across(c(met_abdominal_end, met_tg_end, met_hdl_end, met_bp_end, met_fbg_end)))),
    metax_end = case_when(
      met_sum_end >= 3 ~ "Metabolic Syndrome",
      met_n_end < 5 ~ NA_character_,
      TRUE ~ "Normal"
    )
  ) %>%
  ungroup()



## define_hyperlipidemia_base
raw_data <- raw_data %>%
  mutate(HLP_base = case_when(
    total_cholesterol_baseline >= 200 |
      low_density_lipoprotein_cholesterol_baseline >= 130 |
      (gender == "male" & high_density_lipoprotein_baseline < 40) |
      (gender == "female" & high_density_lipoprotein_baseline < 50) |
      triglyceride_baseline >= 150 ~ "HLP",
    
    total_cholesterol_baseline < 200 &
      low_density_lipoprotein_cholesterol_baseline < 130 &
      ((gender == "male" & high_density_lipoprotein_baseline >= 40) |
         (gender == "female" & high_density_lipoprotein_baseline >= 50)) &
      triglyceride_baseline < 150 ~ "Normal",
    
    is.na(total_cholesterol_baseline) & is.na(low_density_lipoprotein_cholesterol_baseline) &
      is.na(high_density_lipoprotein_baseline) & is.na(triglyceride_baseline) ~ NA_character_,
    
    TRUE ~ NA_character_
  ))


## define_hyperlipidemia_end
raw_data <- raw_data %>%
  mutate(HLP_end = case_when(
    total_cholesterol_endpoint >= 200 |
      low_density_lipoprotein_cholesterol_endpoint >= 130 |
      (gender == "male" & high_density_lipoprotein_endpoint < 40) |
      (gender == "female" & high_density_lipoprotein_endpoint < 50) |
      triglyceride_endpoint >= 150 ~ "HLP",
    
    total_cholesterol_endpoint < 200 &
      low_density_lipoprotein_cholesterol_endpoint < 130 &
      ((gender == "male" & high_density_lipoprotein_endpoint >= 40) |
         (gender == "female" & high_density_lipoprotein_endpoint >= 50)) &
      triglyceride_endpoint < 150 ~ "Normal",
    
    is.na(total_cholesterol_endpoint) & is.na(low_density_lipoprotein_cholesterol_endpoint) &
      is.na(high_density_lipoprotein_endpoint) & is.na(triglyceride_endpoint) ~ NA_character_,
    
    TRUE ~ NA_character_
  ))



## define_hypertension_base
raw_data <- raw_data %>%
  mutate(htn_base = case_when(
    systolic_blood_pressure_baseline >= 140 | diastolic_blood_pressure_baseline >= 90 ~ "HTN (Stage 2)",
    
    systolic_blood_pressure_baseline >= 130 & systolic_blood_pressure_baseline < 140 |
      diastolic_blood_pressure_baseline >= 80 & diastolic_blood_pressure_baseline < 90 ~ "HTN (Stage 1)",
    
    systolic_blood_pressure_baseline >= 120 & systolic_blood_pressure_baseline < 130 &
      diastolic_blood_pressure_baseline < 80 ~ "Elevated",
    
    systolic_blood_pressure_baseline < 120 & diastolic_blood_pressure_baseline < 80 ~ "Normal",
    
    is.na(systolic_blood_pressure_baseline) & is.na(diastolic_blood_pressure_baseline) ~ NA_character_,
    
    TRUE ~ NA_character_
  ))


## define_hypertension_end
raw_data <- raw_data %>%
  mutate(htn_end = case_when(
    systolic_blood_pressure_endpoint >= 140 | diastolic_blood_pressure_endpoint >= 90 ~ "HTN (Stage 2)",
    
    systolic_blood_pressure_endpoint >= 130 & systolic_blood_pressure_endpoint < 140 |
      diastolic_blood_pressure_endpoint >= 80 & diastolic_blood_pressure_endpoint < 90 ~ "HTN (Stage 1)",
    
    systolic_blood_pressure_endpoint >= 120 & systolic_blood_pressure_endpoint < 130 &
      diastolic_blood_pressure_endpoint < 80 ~ "Elevated",
    
    systolic_blood_pressure_endpoint < 120 & diastolic_blood_pressure_endpoint < 80 ~ "Normal",
    
    is.na(systolic_blood_pressure_endpoint) & is.na(diastolic_blood_pressure_endpoint) ~ NA_character_,
    
    TRUE ~ NA_character_
  ))





################# 根據次數分組，拆分dataset ######################
stat_table <- raw_data

duplicated_clients <- stat_table %>%
  group_by(client_id) %>%
  filter(n() > 1) %>%  # 出現超過一次
  ungroup()

stat_table %>%
  count(client_id) %>%             # 計算每個 client_id 出現幾次
  count(n, name = "num_clients")   # 再計算每個出現次數（n）各有幾個 client_id



# 確保日期格式正確並進行操作
filtered_sorted_data <- stat_table %>%
  mutate(started_at = as.Date(started_at)) %>%
  group_by(client_id) %>%
  filter(n() >= 3) %>%                # 篩選出現次數 >= 3 的人
  arrange(client_id, started_at) %>% # 每個人內部依 started_at 排序
  ungroup()

top3_started <- stat_table %>%
  mutate(started_at = as.Date(started_at)) %>%
  group_by(client_id) %>%
  filter(n() >= 3) %>%                     # 只保留至少出現 3 次的人
  arrange(started_at, .by_group = TRUE) %>%
  slice_head(n = 3) %>%                    # 每個人抓前三筆（已排序）
  ungroup()

top3_started <- stat_table %>%
  mutate(started_at = as.Date(started_at)) %>%
  group_by(client_id) %>%
  filter(n() >= 3) %>%                     # 只保留至少出現 3 次的人
  arrange(started_at, .by_group = TRUE) %>%
  slice_head(n = 3) %>%                    # 每個人抓前三筆（已排序）
  mutate(month_tag = case_when(            # 加上時間標籤
    row_number() == 1 ~ "1st treatment",
    row_number() == 2 ~ "2nd treatment",
    row_number() == 3 ~ "3rd treatment"
  )) %>%
  ungroup()


gap_check_top3 <- top3_started %>%
  mutate(
    started_at = as.Date(started_at),
    finished_at = as.Date(finished_at)  
  ) %>%
  group_by(client_id) %>%
  arrange(started_at, .by_group = TRUE) %>%
  mutate(
    prev_finished = lag(finished_at),
    gap_days = as.numeric(started_at - prev_finished)
  ) %>%
  summarise(
    has_gap_over_60days = any(gap_days > 60, na.rm = TRUE),
    .groups = "drop"
  )


gap_check_top3 %>% count(has_gap_over_60days)


top3_started_disease <- top3_started %>%
  select(client_id, started_at, finished_at, month_tag, 
         bmi_gp_base, bmi_gp_end,
         DM_base, DM_end,
         metax_base, metax_end,
         htn_base, htn_end,
         HLP_base, HLP_end)

top3_started_with_gap_flag <- top3_started_disease %>%
  left_join(gap_check_top3, by = "client_id")

stat_table_60d <- top3_started_with_gap_flag %>%
  filter(has_gap_over_60days == FALSE)


nrow(stat_table_60d)/3



################## plot BMI #############
# Overall N=191
# 篩出 1st treatment/2nd treatment/3rd treatment 都有非 NA 的 bmi_gp_end，且 1st treatment 有非 NA 的 bmi_gp_base
valid_clients <- stat_table_60d %>%
  filter(month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")) %>%
  group_by(client_id) %>%
  summarise(
    has_base = any(!is.na(bmi_gp_base) & month_tag == "1st treatment"),
    n_end = sum(!is.na(bmi_gp_end) & month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")),
    .groups = "drop"
  ) %>%
  filter(has_base == TRUE, n_end == 3) %>%
  pull(client_id)

# baseline（只保留 1st treatment 的 bmi_gp_base）
baseline_data <- stat_table_60d %>%
  filter(client_id %in% valid_clients, month_tag == "1st treatment") %>%
  select(client_id, bmi_gp = bmi_gp_base) %>%
  mutate(time_point = "baseline")

# followup（1st treatment/2nd treatment/3rd treatment 的 bmi_gp_end）
followup_data <- stat_table_60d %>%
  filter(client_id %in% valid_clients, month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")) %>%
  select(client_id, time_point = month_tag, bmi_gp = bmi_gp_end)

# 合併 baseline + followup
combined_data <- bind_rows(baseline_data, followup_data) %>%
  filter(!is.na(bmi_gp))  # 再次保險移除 NA 類別



#  計算各時間點 × 分組的人數與百分比
plot_data <- combined_data %>%
  group_by(time_point, bmi_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

plot_data <- plot_data %>%
  mutate(time_point = factor(time_point, levels = c("3rd treatment", "2nd treatment", "1st treatment", "baseline")))


## plot overall 
p_bmi_overall <- ggplot(plot_data, aes(y = time_point, x = percent, fill = bmi_gp,
                                               text = paste0("分組：", bmi_gp, "\nn：", n, "\n百分比：", round(percent, 1), "%"))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Underweight" = "#B8DBB3",
    "Normal" = "#77AC30",
    "Overweight" = "#FED976",
    "Mild obesity" = "#EDB120",
    "Moderate obesity" = "#D95319",
    "Morbid obesity" = "#a50f15"
  )) +
  labs(
    title = "BMI Categories Count Changes Over 3 Treatments"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_blank(),    
    axis.title.y = element_blank(),
    legend.title = element_blank()
  ) 

ggplotly(p_bmi_overall, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
p_bmi <- ggplotly(p_bmi_overall, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
htmlwidgets::saveWidget(p_bmi, "p_bmi.html")





# 依照baseline狀態去分群看變化
# 建立 baseline group
baseline_info_60d <- stat_table_60d %>%
  filter(client_id %in% valid_clients, month_tag == "1st treatment") %>%
  select(client_id, baseline_group = bmi_gp_base)


# 建立 transition data（只保留完整三期資料的人）
transition_data_60d <- stat_table_60d %>%
  filter(month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment"),
         client_id %in% valid_clients) %>%
  left_join(baseline_info_60d, by = "client_id") %>%
  select(client_id, baseline_group, time_point = month_tag, bmi_gp = bmi_gp_end)


plot_transition_data_60d <- transition_data_60d %>%
  filter(!is.na(baseline_group), !is.na(bmi_gp)) %>%
  group_by(baseline_group, time_point, bmi_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(baseline_group, time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    time_point = factor(time_point, levels = c("3rd treatment", "2nd treatment", "1st treatment")),
    baseline_group = factor(baseline_group,
                            levels = c("Underweight", "Normal", "Overweight",
                                       "Mild obesity", "Moderate obesity", "Morbid obesity"))
  )



# 畫橫向 facet 堆疊圖
p_bmi_baseline_group <- ggplot(plot_transition_data_60d, aes(
  y = time_point, x = percent, fill = bmi_gp,
  text = paste0("分組：", bmi_gp, "\nn：", n, "\n百分比：", round(percent, 1), "%")
)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Underweight" = "#8CBF87",
    "Normal" = "#77AC30",
    "Overweight" = "#FED976",
    "Mild obesity" = "#EDB120",
    "Moderate obesity" = "#D95319",
    "Morbid obesity" = "#a50f15"
  )) +
  labs(
    title = "BMI Categories Count Changes Over 3 Treatments by Initial Category",
  ) +
  facet_wrap(~ baseline_group, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_blank(),    
    axis.title.y = element_blank(),
    legend.title = element_blank()
  ) 

ggplotly(p_bmi_baseline_group, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
p_bmi_baseline_gp <- ggplotly(p_bmi_baseline_group, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
htmlwidgets::saveWidget(p_bmi_baseline_gp, "p_bmi_baseline_gp.html")




################## plot DM #############
# Overall
# 篩出 1st treatment/2nd treatment/3rd treatment 都有非 NA 的 DM_end，且 1st treatment 有非 NA 的 DM_base
DM_valid_clients_60d <- stat_table_60d %>%
  filter(month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")) %>%
  group_by(client_id) %>%
  summarise(
    has_base = any(!is.na(DM_base) & month_tag == "1st treatment"),
    n_end = sum(!is.na(DM_end) & month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")),
    .groups = "drop"
  ) %>%
  filter(has_base == TRUE, n_end == 3) %>%
  pull(client_id)

# baseline（只保留 1st treatment 的 base）
DM_baseline_data_60d <- stat_table_60d %>%
  filter(client_id %in% DM_valid_clients_60d, month_tag == "1st treatment") %>%
  select(client_id, DM_gp = DM_base) %>%
  mutate(time_point = "baseline")

# followup（1st treatment, 2nd treatment, 3rd treatment 的 end）
DM_followup_data_60d <- stat_table_60d %>%
  filter(client_id %in% DM_valid_clients_60d, month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")) %>%
  select(client_id, time_point = month_tag, DM_gp = DM_end)

# 合併 baseline + followup
DM_combined_data_60d <- bind_rows(DM_baseline_data_60d, DM_followup_data_60d) %>%
  filter(!is.na(DM_gp))  # 再次保險移除 NA 類別



#  計算各時間點 × 分組的人數與百分比
plot_DM_data_60d <- DM_combined_data_60d %>%
  group_by(time_point, DM_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

plot_DM_data_60d <- plot_DM_data_60d %>%
  mutate(time_point = factor(time_point, levels = c("3rd treatment", "2nd treatment", "1st treatment", "baseline")),
         DM_gp = factor(DM_gp, levels = c("Normal", "Pre-DM", "DM")))


## plot overall 
p_DM_overall_60d <- ggplot(plot_DM_data_60d, aes(y = time_point, x = percent, fill = DM_gp,
                                                 text = paste0("分組：", DM_gp, "\nn：", n, "\n百分比：", round(percent, 1), "%"))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Normal" = "#77AC30",
    "Pre-DM" = "#EDB120",
    "DM" = "#a50f15"
  )) +
  labs(
    title = "Diabetes Counts Changes Over 3 Treatments"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_blank(),    
    axis.title.y = element_blank(),
    legend.title = element_blank()
  ) 

ggplotly(p_DM_overall_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
p_DM <- ggplotly(p_DM_overall_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
htmlwidgets::saveWidget(p_DM, "p_DM.html")



# plot baseline group
# 建立 baseline group
DM_baseline_info_60d <- stat_table_60d %>%
  filter(client_id %in% DM_valid_clients_60d, month_tag == "1st treatment") %>%
  select(client_id, baseline_group = DM_base)

# 建立 transition data（只保留完整三期資料的人）
DM_transition_data_60d <- stat_table_60d %>%
  filter(month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment"),
         client_id %in% DM_valid_clients_60d) %>%
  left_join(DM_baseline_info_60d, by = "client_id") %>%
  select(client_id, baseline_group, time_point = month_tag, DM_gp = DM_end)


DM_plot_transition_data_60d <- DM_transition_data_60d %>%
  filter(!is.na(baseline_group), !is.na(DM_gp)) %>%
  group_by(baseline_group, time_point, DM_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(baseline_group, time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    time_point = factor(time_point, levels = c("3rd treatment", "2nd treatment", "1st treatment")),
    baseline_group = factor(baseline_group,
                            levels = c("Normal", "Pre-DM", "DM")),
    DM_gp = factor(DM_gp, levels = c("Normal", "Pre-DM", "DM")))
  


# 畫橫向 facet 堆疊圖
p_DM_baseline_group <- ggplot(DM_plot_transition_data_60d, aes(
  y = time_point, x = percent, fill = DM_gp,
  text = paste0("分組：", DM_gp, "\n百分比：", round(percent, 1), "%\nn：", n)
)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Normal" = "#77AC30",
    "Pre-DM" = "#EDB120",
    "DM" = "#a50f15"
  )) +
  labs(
    title = "Diabetes Count Changes Over 3 Treatments by Initial Status"
  ) +
  facet_wrap(~ baseline_group, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_blank(),    
    axis.title.y = element_blank(),
    legend.title = element_blank()
  ) 

ggplotly(p_DM_baseline_group, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
p_DM_baseline_gp <- ggplotly(p_DM_baseline_group, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
htmlwidgets::saveWidget(p_DM_baseline_gp, "p_DM_baseline_gp.html")




################## plot Metabolic Syndrome #############
# Overall N = 74
# 篩出 1st treatment/2nd treatment/3rd treatment 都有非 NA 的 end，且 1st treatment 有非 NA 的 base
metax_valid_clients_60d <- stat_table_60d %>%
  filter(month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")) %>%
  group_by(client_id) %>%
  summarise(
    has_base = any(!is.na(metax_base) & month_tag == "1st treatment"),
    n_end = sum(!is.na(metax_end) & month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")),
    .groups = "drop"
  ) %>%
  filter(has_base == TRUE, n_end == 3) %>%
  pull(client_id)

# baseline（只保留 1st treatment 的 base）
metax_baseline_data_60d <- stat_table_60d %>%
  filter(client_id %in% metax_valid_clients_60d, month_tag == "1st treatment") %>%
  select(client_id, metax_gp = metax_base) %>%
  mutate(time_point = "baseline")

# followup（1st treatment, 2nd treatment, 3rd treatment 的 end）
metax_followup_data_60d <- stat_table_60d %>%
  filter(client_id %in% metax_valid_clients_60d, month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")) %>%
  select(client_id, time_point = month_tag, metax_gp = metax_end)

# 合併 baseline + followup
metax_combined_data_60d <- bind_rows(metax_baseline_data_60d, metax_followup_data_60d) %>%
  filter(!is.na(metax_gp))  # 再次保險移除 NA 類別



#  計算各時間點 × 分組的人數與百分比
plot_metax_data_60d <- metax_combined_data_60d %>%
  group_by(time_point, metax_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

plot_metax_data_60d <- plot_metax_data_60d %>%
  mutate(time_point = factor(time_point, levels = c("3rd treatment", "2nd treatment", "1st treatment", "baseline")),
         metax_gp = factor(metax_gp, levels = c("Normal", "Metabolic Syndrome")))


## plot overall 
p_metax_overall_60d <- ggplot(plot_metax_data_60d, aes(y = time_point, x = percent, fill = metax_gp,
                                                       text = paste0("分組：", metax_gp, "\nn：", n, "\n百分比：", round(percent, 1), "%"))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Normal" = "#77AC30",
    "Metabolic Syndrome" = "#a50f15"
  )) +
  labs(
    title = "Diabetes Count Changes Over 3 Treatments by Initial Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_blank(),    
    axis.title.y = element_blank(),
    legend.title = element_blank()
  ) 

ggplotly(p_metax_overall_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
p_metax <- ggplotly(p_metax_overall_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
htmlwidgets::saveWidget(p_metax, "p_metax.html")




# 建立 baseline group
metax_baseline_info_60d <- stat_table_60d %>%
  filter(client_id %in% metax_valid_clients_60d, month_tag == "1st treatment") %>%
  select(client_id, baseline_group = metax_base)

# 建立 transition data（只保留完整三期資料的人）
metax_transition_data_60d <- stat_table_60d %>%
  filter(month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment"),
         client_id %in% metax_valid_clients_60d) %>%
  left_join(metax_baseline_info_60d, by = "client_id") %>%
  select(client_id, baseline_group, time_point = month_tag, metax_gp = metax_end)


metax_plot_transition_data_60d <- metax_transition_data_60d %>%
  filter(!is.na(baseline_group), !is.na(metax_gp)) %>%
  group_by(baseline_group, time_point, metax_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(baseline_group, time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    time_point = factor(time_point, levels = c("3rd treatment", "2nd treatment", "1st treatment")),
    baseline_group = factor(baseline_group,
                            levels = c("Normal", "Metabolic Syndrome")),
    metax_gp = factor(metax_gp, levels = c("Normal", "Metabolic Syndrome")))



# 畫橫向 facet 堆疊圖
p_metax_group_60d <- ggplot(metax_plot_transition_data_60d, aes(
  y = time_point, x = percent, fill = metax_gp,
  text = paste0("分組：", metax_gp, "\n百分比：", round(percent, 1), "%\nn：", n)
)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Normal" = "#77AC30",
    "Metabolic Syndrome" = "#a50f15"
  )) +
  labs(
    title = "Metabolic Syndrome Count Changes Over 3 Treatments by Initial Status"
  ) +
  facet_wrap(~ baseline_group, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_blank(),    
    axis.title.y = element_blank(),
    legend.title = element_blank()
  ) 

ggplotly(p_metax_group_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
p_metax_baseline_gp <- ggplotly(p_metax_group_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
htmlwidgets::saveWidget(p_metax_baseline_gp, "p_metax_baseline_gp.html")




################## plot Hypertension #############
# overall N=62
# 篩出 1st treatment/2nd treatment/3rd treatment 都有非 NA 的 end，且 1st treatment 有非 NA 的 base
htn_valid_clients_60d <- stat_table_60d %>%
  filter(month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")) %>%
  group_by(client_id) %>%
  summarise(
    has_base = any(!is.na(htn_base) & month_tag == "1st treatment"),
    n_end = sum(!is.na(htn_end) & month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")),
    .groups = "drop"
  ) %>%
  filter(has_base == TRUE, n_end == 3) %>%
  pull(client_id)

# baseline（只保留 1st treatment 的 base）
htn_baseline_data_60d <- stat_table_60d %>%
  filter(client_id %in% htn_valid_clients_60d, month_tag == "1st treatment") %>%
  select(client_id, htn_gp = htn_base) %>%
  mutate(time_point = "baseline")

# followup（1st treatment, 2nd treatment, 3rd treatment 的 end）
htn_followup_data_60d <- stat_table_60d %>%
  filter(client_id %in% htn_valid_clients_60d, month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")) %>%
  select(client_id, time_point = month_tag, htn_gp = htn_end)

# 合併 baseline + followup
htn_combined_data_60d <- bind_rows(htn_baseline_data_60d, htn_followup_data_60d) %>%
  filter(!is.na(htn_gp))  # 再次保險移除 NA 類別



#  計算各時間點 × 分組的人數與百分比
plot_htn_data_60d <- htn_combined_data_60d %>%
  group_by(time_point, htn_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

plot_htn_data_60d <- plot_htn_data_60d %>%
  mutate(time_point = factor(time_point, levels = c("3rd treatment", "2nd treatment", "1st treatment", "baseline")),
         htn_gp = factor(htn_gp, levels = c("Normal", "Elevated", "HTN (Stage 1)", "HTN (Stage 2)")))


## plot overall 
p_htn_overall_60d <- ggplot(plot_htn_data_60d, aes(y = time_point, x = percent, fill = htn_gp,
                                                   text = paste0("分組：", htn_gp, "\nn：", n, "\n百分比：", round(percent, 1), "%"))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Normal" = "#77AC30",
    "Elevated" = "#EDB120",
    "HTN (Stage 1)" = "#D95319",
    "HTN (Stage 2)" = "#a50f15"
  )) +
  labs(
    title = "Hypertension Count Changes Over 3 Treatments"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_blank(),    
    axis.title.y = element_blank(),
    legend.title = element_blank()
  ) 

ggplotly(p_htn_overall_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
p_htn <- ggplotly(p_htn_overall_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
htmlwidgets::saveWidget(p_htn, "p_htn.html")



# 建立 baseline group
htn_baseline_info_60d <- stat_table_60d %>%
  filter(client_id %in% htn_valid_clients_60d, month_tag == "1st treatment") %>%
  select(client_id, baseline_group = htn_base)

# 建立 transition data（只保留完整三期資料的人）
htn_transition_data_60d <- stat_table_60d %>%
  filter(month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment"),
         client_id %in% htn_valid_clients_60d) %>%
  left_join(htn_baseline_info_60d, by = "client_id") %>%
  select(client_id, baseline_group, time_point = month_tag, htn_gp = htn_end)


htn_plot_transition_data_60d <- htn_transition_data_60d %>%
  filter(!is.na(baseline_group), !is.na(htn_gp)) %>%
  group_by(baseline_group, time_point, htn_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(baseline_group, time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    time_point = factor(time_point, levels = c("3rd treatment", "2nd treatment", "1st treatment")),
    baseline_group = factor(baseline_group,
                            levels = c("Normal", "Elevated", "HTN (Stage 1)", "HTN (Stage 2)")),
    htn_gp = factor(htn_gp,levels = c("Normal", "Elevated", "HTN (Stage 1)", "HTN (Stage 2)")))  # 固定順序



# 畫橫向 facet 堆疊圖
p_htn_group_60d <- ggplot(htn_plot_transition_data_60d, aes(
  y = time_point, x = percent, fill = htn_gp,
  text = paste0("分組：", htn_gp, "\n百分比：", round(percent, 1), "%\nn：", n)
)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Normal" = "#77AC30",
    "Elevated" = "#EDB120",
    "HTN (Stage 1)" = "#D95319",
    "HTN (Stage 2)" = "#a50f15"
  )) +
  labs(
    title = "Hypertension Count Changes Over 3 Treatments by Initial Status"
  ) +
  facet_wrap(~ baseline_group, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_blank(),    
    axis.title.y = element_blank(),
    legend.title = element_blank()
  ) 

ggplotly(p_htn_group_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
p_htn_baseline_gp <- ggplotly(p_htn_group_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
htmlwidgets::saveWidget(p_htn_baseline_gp, "p_htn_baseline_gp.html")






################## plot Hyperlipidemia #############
#Overall N=236
# 篩出 1st treatment/2nd treatment/3rd treatment 都有非 NA 的 end，且 1st treatment 有非 NA 的 base
HLP_valid_clients_60d <- stat_table_60d %>%
  filter(month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")) %>%
  group_by(client_id) %>%
  summarise(
    has_base = any(!is.na(HLP_base) & month_tag == "1st treatment"),
    n_end = sum(!is.na(HLP_end) & month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")),
    .groups = "drop"
  ) %>%
  filter(has_base == TRUE, n_end == 3) %>%
  pull(client_id)

# baseline（只保留 1st treatment 的 base）
HLP_baseline_data_60d <- stat_table_60d %>%
  filter(client_id %in% HLP_valid_clients_60d, month_tag == "1st treatment") %>%
  select(client_id, HLP_gp = HLP_base) %>%
  mutate(time_point = "baseline")

# followup（1st treatment, 2nd treatment, 3rd treatment 的 end）
HLP_followup_data_60d <- stat_table_60d %>%
  filter(client_id %in% HLP_valid_clients_60d, month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment")) %>%
  select(client_id, time_point = month_tag, HLP_gp = HLP_end)

# 合併 baseline + followup
HLP_combined_data_60d <- bind_rows(HLP_baseline_data_60d, HLP_followup_data_60d) %>%
  filter(!is.na(HLP_gp))  # 再次保險移除 NA 類別



#  計算各時間點 × 分組的人數與百分比
plot_HLP_data_60d <- HLP_combined_data_60d %>%
  group_by(time_point, HLP_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

plot_HLP_data_60d <- plot_HLP_data_60d %>%
  mutate(time_point = factor(time_point, levels = c("3rd treatment", "2nd treatment", "1st treatment", "baseline")),
         HLP_gp = factor(HLP_gp, levels = c("Normal", "HLP")))


## plot overall 
p_HLP_overall_60d <- ggplot(plot_HLP_data_60d, aes(y = time_point, x = percent, fill = HLP_gp,
                                                   text = paste0("分組：", HLP_gp, "\nn：", n, "\n百分比：", round(percent, 1), "%"))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Normal" = "#77AC30",
    "HLP" = "#a50f15"
  )) +
  labs(
    title = "Hyperlipidemia Count Changes Over 3 Treatments"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_blank(),    
    axis.title.y = element_blank(),
    legend.title = element_blank()
  ) 

ggplotly(p_HLP_overall_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
p_hlp <- ggplotly(p_HLP_overall_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
htmlwidgets::saveWidget(p_hlp, "p_hlp.html")



# 建立 baseline group
HLP_baseline_info_60d <- stat_table_60d %>%
  filter(client_id %in% HLP_valid_clients_60d, month_tag == "1st treatment") %>%
  select(client_id, baseline_group = HLP_base)

# 建立 transition data（只保留完整三期資料的人）
HLP_transition_data_60d <- stat_table_60d %>%
  filter(month_tag %in% c("1st treatment", "2nd treatment", "3rd treatment"),
         client_id %in% HLP_valid_clients_60d) %>%
  left_join(HLP_baseline_info_60d, by = "client_id") %>%
  select(client_id, baseline_group, time_point = month_tag, HLP_gp = HLP_end)


HLP_plot_transition_data_60d <- HLP_transition_data_60d %>%
  filter(!is.na(baseline_group), !is.na(HLP_gp)) %>%
  group_by(baseline_group, time_point, HLP_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(baseline_group, time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    time_point = factor(time_point, levels = c("3rd treatment", "2nd treatment", "1st treatment")),
    baseline_group = factor(baseline_group,
                            levels = c("Normal", "HLP"))
  )


# 畫橫向 facet 堆疊圖
p_HLP_group_60d <- ggplot(HLP_plot_transition_data_60d, aes(
  y = time_point, x = percent, fill = HLP_gp,
  text = paste0("分組：", HLP_gp, "\n百分比：", round(percent, 1), "%\nn：", n)
)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Normal" = "#77AC30",
    "HLP" = "#a50f15"
  )) +
  labs(
    title = "Hyperlipidemia Count Changes Over 3 Treatments by Initial Status"
  ) +
  facet_wrap(~ baseline_group, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_blank(),    
    axis.title.y = element_blank(),
    legend.title = element_blank()
  ) 

ggplotly(p_HLP_group_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
p_hlp_baseline_gp <- ggplotly(p_HLP_group_60d, tooltip = "text") %>% layout(legend = list(title = list(text = "")))
htmlwidgets::saveWidget(p_hlp_baseline_gp, "p_hlp_baseline_gp.html")


