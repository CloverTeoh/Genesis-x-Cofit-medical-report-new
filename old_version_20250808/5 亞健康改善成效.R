library(pacman)
pacman::p_load(tidyr, googleVis, magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, extraInserts, ggvenn, ggplot2, plotly, knitr, kableExtra, openxlsx, lubridate, cowplot, ggpubr, webshot)

# data_cleaning
## dataset_input
setwd("/Users/clover/Documents/cofit/gitbook")
getwd()
stat_table <- read_excel("client_intervention_data_OGIRT_classified_20250529.xlsx")

## define disease #######
## define Obesity by BMI
stat_table$bmi_gp_base <- stat_table$bmi_w0 %>% cut(c(0,18.5,24,27, 30, 35 ,100), c("Underweight", "Normal", "Overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)
stat_table$bmi_gp_end <- stat_table$bmi_w8 %>% cut(c(0,18.5,24,27, 30, 35 ,100), c("Underweight", "Normal", "Overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)


## define DM base
stat_table <- stat_table %>%
    mutate(DM_base = case_when(
      hba1c_base >= 6.5 |
        glucose_ac_base >= 126 |
        glucose_pc_2hr_base >= 200 ~ "DM",
      
      hba1c_base >= 5.7 & hba1c_base < 6.5 |
        glucose_ac_base >= 100 & glucose_ac_base < 126 |
        glucose_pc_2hr_base >= 140 & glucose_pc_2hr_base < 200 ~ "Pre-DM",
      
      !is.na(hba1c_base) |
        !is.na(glucose_ac_base) |
        !is.na(glucose_pc_2hr_base) ~ "Normal",
      
      TRUE ~ NA_character_
    ))

## define DM end
stat_table <- stat_table %>%
  mutate(DM_end = case_when(
    hba1c_end >= 6.5 |
      glucose_ac_end >= 126 |
      glucose_pc_2hr_end >= 200 ~ "DM",
    
    hba1c_end >= 5.7 & hba1c_end < 6.5 |
      glucose_ac_end >= 100 & glucose_ac_end < 126 |
      glucose_pc_2hr_end >= 140 & glucose_pc_2hr_end < 200 ~ "Pre-DM",
    
    !is.na(hba1c_end) |
      !is.na(glucose_ac_end) |
      !is.na(glucose_pc_2hr_end) ~ "Normal",
    
    TRUE ~ NA_character_
  ))




## define_metabolic syndrome_base
stat_table <- stat_table %>% 
  mutate(
    met_abdominal_base = ifelse((sex == "male" & waist_circumference_w0  >= 90) |
                             (sex == "female" & waist_circumference_w0 >= 80), 1, 0),
    
    met_tg_base = ifelse(triglyceride_base >= 150, 1, 0),
    
    met_hdl_base = ifelse((sex == "male" & high_density_lipoprotein_base < 40) |
                       (sex == "female" & high_density_lipoprotein_base < 50), 1, 0),
    
    met_bp_base = ifelse(systolic_blood_pressure_base >= 130 | diastolic_blood_pressure_base >= 85, 1, 0),
    
    met_fbg_base = ifelse(glucose_ac_base >= 100, 1, 0)
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


## define_metabolic syndrome_end
stat_table <- stat_table %>% 
  mutate(
    met_abdominal_end = ifelse((sex == "male" & waist_circumference_w8  >= 90) |
                                  (sex == "female" & waist_circumference_w8 >= 80), 1, 0),
    
    met_tg_end = ifelse(triglyceride_end >= 150, 1, 0),
    
    met_hdl_end = ifelse((sex == "male" & high_density_lipoprotein_end < 40) |
                            (sex == "female" & high_density_lipoprotein_end < 50), 1, 0),
    
    met_bp_end = ifelse(systolic_blood_pressure_end >= 130 | diastolic_blood_pressure_end >= 85, 1, 0),
    
    met_fbg_end = ifelse(glucose_ac_end >= 100, 1, 0)
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



## define_hypertension_base
stat_table <- stat_table %>%
  mutate(htn_base = case_when(
    systolic_blood_pressure_base >= 140 | diastolic_blood_pressure_base >= 90 ~ "HTN (Stage 2)",
    
    systolic_blood_pressure_base >= 130 & systolic_blood_pressure_base < 140 |
      diastolic_blood_pressure_base >= 80 & diastolic_blood_pressure_base < 90 ~ "HTN (Stage 1)",
    
    systolic_blood_pressure_base >= 120 & systolic_blood_pressure_base < 130 &
      diastolic_blood_pressure_base < 80 ~ "Elevated",
    
    systolic_blood_pressure_base < 120 & diastolic_blood_pressure_base < 80 ~ "Normal",
    
    is.na(systolic_blood_pressure_base) & is.na(diastolic_blood_pressure_base) ~ NA_character_,
    
    TRUE ~ NA_character_
  ))


## define_hypertension_end
stat_table <- stat_table %>%
  mutate(htn_end = case_when(
    systolic_blood_pressure_end >= 140 | diastolic_blood_pressure_end >= 90 ~ "HTN (Stage 2)",
    
    systolic_blood_pressure_end >= 130 & systolic_blood_pressure_end < 140 |
      diastolic_blood_pressure_end >= 80 & diastolic_blood_pressure_end < 90 ~ "HTN (Stage 1)",
    
    systolic_blood_pressure_end >= 120 & systolic_blood_pressure_end < 130 &
      diastolic_blood_pressure_end < 80 ~ "Elevated",
    
    systolic_blood_pressure_end < 120 & diastolic_blood_pressure_end < 80 ~ "Normal",
    
    is.na(systolic_blood_pressure_end) & is.na(diastolic_blood_pressure_end) ~ NA_character_,
    
    TRUE ~ NA_character_
  ))




## define_hyperlipidemia_base
stat_table <- stat_table %>%
  mutate(HLP_base = case_when(
    total_cholesterol_base >= 200 |
      low_density_lipoprotein_cholesterol_base >= 130 |
      (sex == "male" & high_density_lipoprotein_base < 40) |
      (sex == "female" & high_density_lipoprotein_base < 50) |
      triglyceride_base >= 150 ~ "HLP",
    
    total_cholesterol_base < 200 &
      low_density_lipoprotein_cholesterol_base < 130 &
      ((sex == "male" & high_density_lipoprotein_base >= 40) |
         (sex == "female" & high_density_lipoprotein_base >= 50)) &
      triglyceride_base < 150 ~ "Normal",
    
    is.na(total_cholesterol_base) & is.na(low_density_lipoprotein_cholesterol_base) &
      is.na(high_density_lipoprotein_base) & is.na(triglyceride_base) ~ NA_character_,
    
    TRUE ~ NA_character_
  ))


## define_hyperlipidemia_end
stat_table <- stat_table %>%
  mutate(HLP_end = case_when(
    total_cholesterol_end >= 200 |
      low_density_lipoprotein_cholesterol_end >= 130 |
      (sex == "male" & high_density_lipoprotein_end < 40) |
      (sex == "female" & high_density_lipoprotein_end < 50) |
      triglyceride_end >= 150 ~ "HLP",
    
    total_cholesterol_end < 200 &
      low_density_lipoprotein_cholesterol_end < 130 &
      ((sex == "male" & high_density_lipoprotein_end >= 40) |
         (sex == "female" & high_density_lipoprotein_end >= 50)) &
      triglyceride_end < 150 ~ "Normal",
    
    is.na(total_cholesterol_end) & is.na(low_density_lipoprotein_cholesterol_end) &
      is.na(high_density_lipoprotein_end) & is.na(triglyceride_end) ~ NA_character_,
    
    TRUE ~ NA_character_
  ))



################# 根據次數分組，拆分dataset ######################

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
    row_number() == 1 ~ "2mo",
    row_number() == 2 ~ "4mo",
    row_number() == 3 ~ "6mo"
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
    has_gap_over_7days = any(gap_days > 7, na.rm = TRUE),
    has_gap_over_14days = any(gap_days > 14, na.rm = TRUE),
    has_gap_over_30days = any(gap_days > 30, na.rm = TRUE),
    has_gap_over_60days = any(gap_days > 60, na.rm = TRUE),
    .groups = "drop"
  )

gap_check_top3 %>% count(has_gap_over_7days)
gap_check_top3 %>% count(has_gap_over_14days)
gap_check_top3 %>% count(has_gap_over_30days)
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

stat_table_14d <- top3_started_with_gap_flag %>%
  filter(has_gap_over_14days == FALSE)

stat_table_30d <- top3_started_with_gap_flag %>%
  filter(has_gap_over_30days == FALSE)

stat_table_60d <- top3_started_with_gap_flag %>%
  filter(has_gap_over_60days == FALSE)

nrow(stat_table_14d)/3
nrow(stat_table_30d)/3
nrow(stat_table_60d)/3










################## plot BMI for 14 days #############

# 篩出 2mo/4mo/6mo 都有非 NA 的 bmi_gp_end，且 2mo 有非 NA 的 bmi_gp_base
valid_clients_14d <- stat_table_14d %>%
  filter(month_tag %in% c("2mo", "4mo", "6mo")) %>%
  group_by(client_id) %>%
  summarise(
    has_base = any(!is.na(bmi_gp_base) & month_tag == "2mo"),
    n_end = sum(!is.na(bmi_gp_end) & month_tag %in% c("2mo", "4mo", "6mo")),
    .groups = "drop"
  ) %>%
  filter(has_base == TRUE, n_end == 3) %>%
  pull(client_id)

# baseline（只保留 2mo 的 bmi_gp_base）
baseline_data_14d <- stat_table_14d %>%
  filter(client_id %in% valid_clients_14d, month_tag == "2mo") %>%
  select(client_id, bmi_gp = bmi_gp_base) %>%
  mutate(time_point = "baseline")

# followup（2mo, 4mo, 6mo 的 bmi_gp_end）
followup_data_14d <- stat_table_14d %>%
  filter(client_id %in% valid_clients_14d, month_tag %in% c("2mo", "4mo", "6mo")) %>%
  select(client_id, time_point = month_tag, bmi_gp = bmi_gp_end)

# 合併 baseline + followup
combined_data_14d <- bind_rows(baseline_data_14d, followup_data_14d) %>%
  filter(!is.na(bmi_gp))  # 再次保險移除 NA 類別



#  計算各時間點 × 分組的人數與百分比
plot_data_14d <- combined_data_14d %>%
  group_by(time_point, bmi_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

plot_data_14d <- plot_data_14d %>%
  mutate(time_point = factor(time_point, levels = c("6mo", "4mo", "2mo", "baseline")))


## plot overall 
p_bmi_overall_14d <- ggplot(plot_data_14d, aes(y = time_point, x = percent, fill = bmi_gp,
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
    title = "6個月減重成效——肥胖",
    x = "百分比", y = "時間點",
    fill = "分組"
  ) +
  theme_minimal(base_size = 14)

ggplotly(p_bmi_overall_14d, tooltip = "text")





# 建立 baseline group
baseline_info_14d <- stat_table_14d %>%
  filter(client_id %in% valid_clients_14d, month_tag == "2mo") %>%
  select(client_id, baseline_group = bmi_gp_base)



# 建立 transition data（只保留完整三期資料的人）
transition_data_14d <- stat_table_14d %>%
  filter(month_tag %in% c("2mo", "4mo", "6mo"),
         client_id %in% valid_clients_14d) %>%
  left_join(baseline_info_14d, by = "client_id") %>%
  select(client_id, baseline_group, time_point = month_tag, bmi_gp = bmi_gp_end)


plot_transition_data_14d <- transition_data_14d %>%
  filter(!is.na(baseline_group), !is.na(bmi_gp)) %>%
  group_by(baseline_group, time_point, bmi_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(baseline_group, time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    time_point = factor(time_point, levels = c("6mo", "4mo", "2mo")),
    baseline_group = factor(baseline_group,
                            levels = c("Underweight", "Normal", "Overweight",
                                       "Mild obesity", "Moderate obesity", "Morbid obesity"))
  )


# 畫橫向 facet 堆疊圖
p_bmi_group_14d <- ggplot(plot_transition_data_14d, aes(
  y = time_point, x = percent, fill = bmi_gp,
  text = paste0("分組：", bmi_gp, "\n百分比：", round(percent, 1), "%\nn：", n)
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
    title = "6個月減重成效——肥胖（依 Baseline 分群）",
    x = "百分比", y = "時間點",
    fill = "分組"
  ) +
  facet_wrap(~ baseline_group, ncol = 2) +
  theme_minimal(base_size = 14)

ggplotly(p_bmi_group_14d, tooltip = "text")





################## plot DM for 14 days #############


# 篩出 2mo/4mo/6mo 都有非 NA 的 DM_end，且 2mo 有非 NA 的 DM_base
DM_valid_clients_14d <- stat_table_14d %>%
  filter(month_tag %in% c("2mo", "4mo", "6mo")) %>%
  group_by(client_id) %>%
  summarise(
    has_base = any(!is.na(DM_base) & month_tag == "2mo"),
    n_end = sum(!is.na(DM_end) & month_tag %in% c("2mo", "4mo", "6mo")),
    .groups = "drop"
  ) %>%
  filter(has_base == TRUE, n_end == 3) %>%
  pull(client_id)

# baseline（只保留 2mo 的 base）
DM_baseline_data_14d <- stat_table_14d %>%
  filter(client_id %in% DM_valid_clients_14d, month_tag == "2mo") %>%
  select(client_id, DM_gp = DM_base) %>%
  mutate(time_point = "baseline")

# followup（2mo, 4mo, 6mo 的 end）
DM_followup_data_14d <- stat_table_14d %>%
  filter(client_id %in% DM_valid_clients_14d, month_tag %in% c("2mo", "4mo", "6mo")) %>%
  select(client_id, time_point = month_tag, DM_gp = DM_end)

# 合併 baseline + followup
DM_combined_data_14d <- bind_rows(DM_baseline_data_14d, DM_followup_data_14d) %>%
  filter(!is.na(DM_gp))  # 再次保險移除 NA 類別



#  計算各時間點 × 分組的人數與百分比
plot_DM_data_14d <- DM_combined_data_14d %>%
  group_by(time_point, DM_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

plot_DM_data_14d <- plot_DM_data_14d %>%
  mutate(time_point = factor(time_point, levels = c("6mo", "4mo", "2mo", "baseline")),
                             DM_gp = factor(DM_gp, levels = c("Normal", "Pre-DM", "DM")))


## plot overall 
p_DM_overall_14d <- ggplot(plot_DM_data_14d, aes(y = time_point, x = percent, fill = DM_gp,
                                               text = paste0("分組：", DM_gp, "\nn：", n, "\n百分比：", round(percent, 1), "%"))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Normal" = "#77AC30",
    "Pre-DM" = "#EDB120",
    "DM" = "#a50f15"
  )) +
  labs(
    title = "6個月減重成效——糖尿病",
    x = "百分比", y = "時間點",
    fill = "分組"
  ) +
  theme_minimal(base_size = 14)

ggplotly(p_DM_overall_14d, tooltip = "text")




# 建立 baseline group
DM_baseline_info_14d <- stat_table_14d %>%
  filter(client_id %in% DM_valid_clients_14d, month_tag == "2mo") %>%
  select(client_id, baseline_group = DM_base)

# 建立 transition data（只保留完整三期資料的人）
DM_transition_data_14d <- stat_table_14d %>%
  filter(month_tag %in% c("2mo", "4mo", "6mo"),
         client_id %in% DM_valid_clients_14d) %>%
  left_join(DM_baseline_info_14d, by = "client_id") %>%
  select(client_id, baseline_group, time_point = month_tag, DM_gp = DM_end)


DM_plot_transition_data_14d <- DM_transition_data_14d %>%
  filter(!is.na(baseline_group), !is.na(DM_gp)) %>%
  group_by(baseline_group, time_point, DM_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(baseline_group, time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    time_point = factor(time_point, levels = c("6mo", "4mo", "2mo")),
    baseline_group = factor(baseline_group,
                            levels = c("Normal", "Pre-DM", "DM"))
  )


# 畫橫向 facet 堆疊圖
p_DM_group_14d <- ggplot(DM_plot_transition_data_14d, aes(
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
    title = "6個月減重成效——糖尿病（依 Baseline 分群）",
    x = "百分比", y = "時間點",
    fill = "分組"
  ) +
  facet_wrap(~ baseline_group, ncol = 2) +
  theme_minimal(base_size = 14)

ggplotly(p_DM_group_14d, tooltip = "text")




################## plot Metabolic Syndrome for 14 days #############


# 篩出 2mo/4mo/6mo 都有非 NA 的 end，且 2mo 有非 NA 的 base
metax_valid_clients_14d <- stat_table_14d %>%
  filter(month_tag %in% c("2mo", "4mo", "6mo")) %>%
  group_by(client_id) %>%
  summarise(
    has_base = any(!is.na(metax_base) & month_tag == "2mo"),
    n_end = sum(!is.na(metax_end) & month_tag %in% c("2mo", "4mo", "6mo")),
    .groups = "drop"
  ) %>%
  filter(has_base == TRUE, n_end == 3) %>%
  pull(client_id)

# baseline（只保留 2mo 的 base）
metax_baseline_data_14d <- stat_table_14d %>%
  filter(client_id %in% metax_valid_clients_14d, month_tag == "2mo") %>%
  select(client_id, metax_gp = metax_base) %>%
  mutate(time_point = "baseline")

# followup（2mo, 4mo, 6mo 的 end）
metax_followup_data_14d <- stat_table_14d %>%
  filter(client_id %in% metax_valid_clients_14d, month_tag %in% c("2mo", "4mo", "6mo")) %>%
  select(client_id, time_point = month_tag, metax_gp = metax_end)

# 合併 baseline + followup
metax_combined_data_14d <- bind_rows(metax_baseline_data_14d, metax_followup_data_14d) %>%
  filter(!is.na(metax_gp))  # 再次保險移除 NA 類別



#  計算各時間點 × 分組的人數與百分比
plot_metax_data_14d <- metax_combined_data_14d %>%
  group_by(time_point, metax_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

plot_metax_data_14d <- plot_metax_data_14d %>%
  mutate(time_point = factor(time_point, levels = c("6mo", "4mo", "2mo", "baseline")),
         metax_gp = factor(metax_gp, levels = c("Normal", "Metabolic Syndrome")))


## plot overall 
p_metax_overall_14d <- ggplot(plot_metax_data_14d, aes(y = time_point, x = percent, fill = metax_gp,
                                                 text = paste0("分組：", metax_gp, "\nn：", n, "\n百分比：", round(percent, 1), "%"))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Normal" = "#77AC30",
    "Metabolic Syndrome" = "#a50f15"
  )) +
  labs(
    title = "6個月減重成效——代謝症候群",
    x = "百分比", y = "時間點",
    fill = "分組"
  ) +
  theme_minimal(base_size = 14)

ggplotly(p_metax_overall_14d, tooltip = "text")




# 建立 baseline group
metax_baseline_info_14d <- stat_table_14d %>%
  filter(client_id %in% metax_valid_clients_14d, month_tag == "2mo") %>%
  select(client_id, baseline_group = metax_base)

# 建立 transition data（只保留完整三期資料的人）
metax_transition_data_14d <- stat_table_14d %>%
  filter(month_tag %in% c("2mo", "4mo", "6mo"),
         client_id %in% metax_valid_clients_14d) %>%
  left_join(metax_baseline_info_14d, by = "client_id") %>%
  select(client_id, baseline_group, time_point = month_tag, metax_gp = metax_end)


metax_plot_transition_data_14d <- metax_transition_data_14d %>%
  filter(!is.na(baseline_group), !is.na(metax_gp)) %>%
  group_by(baseline_group, time_point, metax_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(baseline_group, time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    time_point = factor(time_point, levels = c("6mo", "4mo", "2mo")),
    baseline_group = factor(baseline_group,
                            levels = c("Normal", "Metabolic Syndrome"))
  )


# 畫橫向 facet 堆疊圖
p_metax_group_14d <- ggplot(metax_plot_transition_data_14d, aes(
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
    title = "6個月減重成效——代謝症候群（依 Baseline 分群）",
    x = "百分比", y = "時間點",
    fill = "分組"
  ) +
  facet_wrap(~ baseline_group, ncol = 2) +
  theme_minimal(base_size = 14)

ggplotly(p_metax_group_14d, tooltip = "text")





################## plot Hypertension for 14 days #############


# 篩出 2mo/4mo/6mo 都有非 NA 的 end，且 2mo 有非 NA 的 base
htn_valid_clients_14d <- stat_table_14d %>%
  filter(month_tag %in% c("2mo", "4mo", "6mo")) %>%
  group_by(client_id) %>%
  summarise(
    has_base = any(!is.na(htn_base) & month_tag == "2mo"),
    n_end = sum(!is.na(htn_end) & month_tag %in% c("2mo", "4mo", "6mo")),
    .groups = "drop"
  ) %>%
  filter(has_base == TRUE, n_end == 3) %>%
  pull(client_id)

# baseline（只保留 2mo 的 base）
htn_baseline_data_14d <- stat_table_14d %>%
  filter(client_id %in% htn_valid_clients_14d, month_tag == "2mo") %>%
  select(client_id, htn_gp = htn_base) %>%
  mutate(time_point = "baseline")

# followup（2mo, 4mo, 6mo 的 end）
htn_followup_data_14d <- stat_table_14d %>%
  filter(client_id %in% htn_valid_clients_14d, month_tag %in% c("2mo", "4mo", "6mo")) %>%
  select(client_id, time_point = month_tag, htn_gp = htn_end)

# 合併 baseline + followup
htn_combined_data_14d <- bind_rows(htn_baseline_data_14d, htn_followup_data_14d) %>%
  filter(!is.na(htn_gp))  # 再次保險移除 NA 類別



#  計算各時間點 × 分組的人數與百分比
plot_htn_data_14d <- htn_combined_data_14d %>%
  group_by(time_point, htn_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

plot_htn_data_14d <- plot_htn_data_14d %>%
  mutate(time_point = factor(time_point, levels = c("6mo", "4mo", "2mo", "baseline")),
         htn_gp = factor(htn_gp, levels = c("Normal", "Elevated", "HTN (Stage 1)", "HTN (Stage 2)")))


## plot overall 
p_htn_overall_14d <- ggplot(plot_htn_data_14d, aes(y = time_point, x = percent, fill = htn_gp,
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
    title = "6個月減重成效——高血壓",
    x = "百分比", y = "時間點",
    fill = "分組"
  ) +
  theme_minimal(base_size = 14)

ggplotly(p_htn_overall_14d, tooltip = "text")




# 建立 baseline group
htn_baseline_info_14d <- stat_table_14d %>%
  filter(client_id %in% htn_valid_clients_14d, month_tag == "2mo") %>%
  select(client_id, baseline_group = htn_base)

# 建立 transition data（只保留完整三期資料的人）
htn_transition_data_14d <- stat_table_14d %>%
  filter(month_tag %in% c("2mo", "4mo", "6mo"),
         client_id %in% htn_valid_clients_14d) %>%
  left_join(htn_baseline_info_14d, by = "client_id") %>%
  select(client_id, baseline_group, time_point = month_tag, htn_gp = htn_end)


htn_plot_transition_data_14d <- htn_transition_data_14d %>%
  filter(!is.na(baseline_group), !is.na(htn_gp)) %>%
  group_by(baseline_group, time_point, htn_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(baseline_group, time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    time_point = factor(time_point, levels = c("6mo", "4mo", "2mo")),
    baseline_group = factor(baseline_group,
                            levels = c("Normal", "Elevated", "HTN (Stage 1)", "HTN (Stage 2)"))
  )


# 畫橫向 facet 堆疊圖
p_htn_group_14d <- ggplot(htn_plot_transition_data_14d, aes(
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
    title = "6個月減重成效——高血壓（依 Baseline 分群）",
    x = "百分比", y = "時間點",
    fill = "分組"
  ) +
  facet_wrap(~ baseline_group, ncol = 2) +
  theme_minimal(base_size = 14)

ggplotly(p_htn_group_14d, tooltip = "text")







################## plot Hyperlipidemia for 14 days #############


# 篩出 2mo/4mo/6mo 都有非 NA 的 end，且 2mo 有非 NA 的 base
HLP_valid_clients_14d <- stat_table_14d %>%
  filter(month_tag %in% c("2mo", "4mo", "6mo")) %>%
  group_by(client_id) %>%
  summarise(
    has_base = any(!is.na(HLP_base) & month_tag == "2mo"),
    n_end = sum(!is.na(HLP_end) & month_tag %in% c("2mo", "4mo", "6mo")),
    .groups = "drop"
  ) %>%
  filter(has_base == TRUE, n_end == 3) %>%
  pull(client_id)

# baseline（只保留 2mo 的 base）
HLP_baseline_data_14d <- stat_table_14d %>%
  filter(client_id %in% HLP_valid_clients_14d, month_tag == "2mo") %>%
  select(client_id, HLP_gp = HLP_base) %>%
  mutate(time_point = "baseline")

# followup（2mo, 4mo, 6mo 的 end）
HLP_followup_data_14d <- stat_table_14d %>%
  filter(client_id %in% HLP_valid_clients_14d, month_tag %in% c("2mo", "4mo", "6mo")) %>%
  select(client_id, time_point = month_tag, HLP_gp = HLP_end)

# 合併 baseline + followup
HLP_combined_data_14d <- bind_rows(HLP_baseline_data_14d, HLP_followup_data_14d) %>%
  filter(!is.na(HLP_gp))  # 再次保險移除 NA 類別



#  計算各時間點 × 分組的人數與百分比
plot_HLP_data_14d <- HLP_combined_data_14d %>%
  group_by(time_point, HLP_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

plot_HLP_data_14d <- plot_HLP_data_14d %>%
  mutate(time_point = factor(time_point, levels = c("6mo", "4mo", "2mo", "baseline")),
         HLP_gp = factor(HLP_gp, levels = c("Normal", "HLP")))


## plot overall 
p_HLP_overall_14d <- ggplot(plot_HLP_data_14d, aes(y = time_point, x = percent, fill = HLP_gp,
                                                   text = paste0("分組：", HLP_gp, "\nn：", n, "\n百分比：", round(percent, 1), "%"))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Normal" = "#77AC30",
    "HLP" = "#a50f15"
  )) +
  labs(
    title = "6個月減重成效——高血脂",
    x = "百分比", y = "時間點",
    fill = "分組"
  ) +
  theme_minimal(base_size = 14)

ggplotly(p_HLP_overall_14d, tooltip = "text")




# 建立 baseline group
HLP_baseline_info_14d <- stat_table_14d %>%
  filter(client_id %in% HLP_valid_clients_14d, month_tag == "2mo") %>%
  select(client_id, baseline_group = HLP_base)

# 建立 transition data（只保留完整三期資料的人）
HLP_transition_data_14d <- stat_table_14d %>%
  filter(month_tag %in% c("2mo", "4mo", "6mo"),
         client_id %in% HLP_valid_clients_14d) %>%
  left_join(HLP_baseline_info_14d, by = "client_id") %>%
  select(client_id, baseline_group, time_point = month_tag, HLP_gp = HLP_end)


HLP_plot_transition_data_14d <- HLP_transition_data_14d %>%
  filter(!is.na(baseline_group), !is.na(HLP_gp)) %>%
  group_by(baseline_group, time_point, HLP_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(baseline_group, time_point) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    time_point = factor(time_point, levels = c("6mo", "4mo", "2mo")),
    baseline_group = factor(baseline_group,
                            levels = c("Normal", "HLP"))
  )


# 畫橫向 facet 堆疊圖
p_HLP_group_14d <- ggplot(HLP_plot_transition_data_14d, aes(
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
    title = "6個月減重成效——高血脂（依 Baseline 分群）",
    x = "百分比", y = "時間點",
    fill = "分組"
  ) +
  facet_wrap(~ baseline_group, ncol = 2) +
  theme_minimal(base_size = 14)

ggplotly(p_HLP_group_14d, tooltip = "text")




































































###############################################################################

sorted_data <- stat_table %>%
  group_by(client_id) %>%
  arrange(started_at, .by_group = TRUE)


client_duration <- stat_table %>%
  mutate(
    started_at = as.Date(started_at),
    finished_at = as.Date(finished_at)
  ) %>%
  group_by(client_id) %>%
  summarise(
    total_days = as.numeric(max(finished_at, na.rm = TRUE) - min(started_at, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    total_months = total_days / 30,
    tag_group = case_when(
      total_days >= 436  & total_days <= 374  ~ "12mo",
      total_days >= 186  & total_days <= 314  ~ "10mo",
      total_days >= 226  & total_days <= 254  ~ "8mo",
      total_days >= 166 & total_days <= 194 ~ "6mo",
      total_days >= 106 & total_days <= 134 ~ "4mo",
      total_days >= 46  & total_days <= 74  ~ "2mo",
      TRUE ~ "other"
    )
  )


client_duration %>% count(tag_group)


# 計算每位 client_id 的紀錄是否有超過 30 天未接續
client_gap_info <- stat_table %>%
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

client_duration_tagged <- client_duration %>%
  left_join(client_gap_info, by = "client_id")


a <- client_duration_tagged %>% filter(tag_group == 'other')

stat_table_tagged <- stat_table %>%
  left_join(client_duration_tagged, by = "client_id")

b <- stat_table_tagged %>%
  select(client_id, started_at, finished_at, total_days, total_months, tag_group, has_gap_over_30days)

c <- stat_table_tagged %>%
  filter(has_gap_over_30days == TRUE) %>%
  select(client_id, started_at, finished_at, total_days, total_months, tag_group, has_gap_over_30days)






ggplot(c, aes(x = started_at, xend = finished_at, y = as.factor(client_id), yend = as.factor(client_id))) +
  geom_segment(size = 4, color = "tomato") +
  labs(
    title = "Clients with >30 Day Gaps (Gantt Chart)",
    x = "Date",
    y = "Client ID"
  ) +
  theme_minimal()


