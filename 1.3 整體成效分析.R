library(pacman)
pacman::p_load(showtext, stringr, scales, forcats, plotly, patchwork, tidyverse, googleVis, magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, extraInserts, ggvenn, ggplot2,knitr, kableExtra, openxlsx, lubridate, cowplot, ggpubr, webshot)

# 使用 Google 字體（思源黑體，適合中文）
font_add_google("Noto Sans TC", "noto")
showtext_auto()

# data_cleaning
## dataset_input
setwd("/Users/clover/Documents/cofit/gitbook")
getwd()
stat_data <- read_excel("client_intervention_data_OGIRT_classified_20250529.xlsx")



## Criteria: client with W0 & W8 weight 
stat_data_wl <- stat_data %>%
  filter(!is.na(weight_w0) &
           !is.na(weight_w8))

stat_data_wl <- stat_data_wl%>%
  mutate(
    delta_weight = ifelse(!is.na(weight_w0) & !is.na(weight_w8),
                          -(weight_w8 - weight_w0), NA),
    delta_weight_without_fat = ifelse(!is.na(weight_without_fat_w0) & !is.na(weight_without_fat_w8),
                                      weight_without_fat_w8 - weight_without_fat_w0, NA),
    delta_bmi = ifelse(!is.na(bmi_w0) & !is.na(bmi_w8),
                       bmi_w8 - bmi_w0, NA),
    delta_waist_circumference = ifelse(!is.na(waist_circumference_w0) & !is.na(waist_circumference_w8),
                                       waist_circumference_w8 - waist_circumference_w0, NA),
    delta_body_fat_mass = ifelse(!is.na(body_fat_mass_w0) & !is.na(body_fat_mass_w8),
                                 body_fat_mass_w0 - body_fat_mass_w8, NA),
    delta_body_fat_mass_percentage = ifelse(!is.na(body_fat_mass_percentage_w0) & !is.na(body_fat_mass_percentage_w8),
                                            body_fat_mass_percentage_w8 - body_fat_mass_percentage_w0, NA),
    delta_vfa = ifelse(!is.na(vfa_w0) & !is.na(vfa_w8),
                       vfa_w8 - vfa_w0, NA),
    delta_muscle_mass = ifelse(!is.na(muscle_mass_w0) & !is.na(muscle_mass_w8),
                               muscle_mass_w0 - muscle_mass_w8, NA)
  )


stat_data_wl <- stat_data_wl %>% 
  mutate(
    weight_loss_percent = -(weight_w8 - weight_w0) / weight_w0 * 100,
    fat_loss_percent = -(body_fat_mass_w8 - body_fat_mass_w0) / body_fat_mass_w0 * 100,
    muscle_loss_percent = -(muscle_mass_w8 - muscle_mass_w0) / muscle_mass_w0 * 100)

stat_data_wl <- stat_data_wl %>%
  mutate(
    homa_beta_base = case_when(
      !is.na(insulin_base) & !is.na(glucose_ac_base) & glucose_ac_base >63 ~ round(360 * insulin_base / (glucose_ac_base - 63), 1),
      TRUE ~ NA_real_
    ),
    homa_beta_end = case_when(
      !is.na(insulin_end) & !is.na(glucose_ac_end) & glucose_ac_end >63 ~ round(360 * insulin_end / (glucose_ac_end - 63), 1),
      TRUE ~ NA_real_
    )
  )



############################# plot ##############################
# 控糖減重成效-體重下降比例 (N = 1808)
## grouping by fat loss percentage
stat_data_wl$wl_gp <- cut(
  stat_data_wl$weight_loss_percent,
  breaks = c(-100, 5, 10, 100),
  labels = c("Low (<5%)", "Moderate (5–10%)", "High (>10%)")
)


plot_data <- stat_data_wl %>%
  group_by(sex, wl_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sex) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

#total_n_data <- stat_data_wl %>%
#  group_by(sex) %>%
#  summarise(total_n = n(), .groups = "drop")

# combined
#plot_data <- plot_data %>%
#  left_join(total_n_data, by = "sex")


# plot
p <- ggplot(plot_data, aes(
  x = sex,
  y = prop,
  fill = wl_gp,
  text = paste0(
    "Group: ", wl_gp, "<br>",
    "N: ", n, "<br>",
    "Proportion: ", scales::percent(prop, accuracy = 0.1)
  )
)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +  # expand 讓 bar 貼齊 x 軸
  scale_fill_manual(
    values = c(
      "Low (<5%)" = "#d73027",
      "Moderate (5–10%)" = "#fee090",
      "High (>10%)" = "#4575b4"
    ),
    name = "Weight Loss %"
  ) +
  labs(
    title = "控糖減重成效 - 體重下降比例",
    x = "Sex",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 14)
  )


# 轉為互動式圖表
bar_1 <- ggplotly(p, tooltip = "text", width = 600, height = 500)

htmlwidgets::saveWidget(bar_1, "bar_1.html")



# filter BMI >=24
plot_data_ob <- stat_data_wl %>%
  filter(bmi_w0 >= 24) %>%
  group_by(sex, wl_gp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sex) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

  
# 建立 ggplot 圖
p_ob <- ggplot(plot_data_ob, aes(
  x = sex,
  y = prop,
  fill = wl_gp,
  text = paste0(
    "Group: ", wl_gp, "<br>",
    "N: ", n, "<br>",
    "Proportion: ", scales::percent(prop, accuracy = 0.1)
  )
)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +  # expand 讓 bar 貼齊 x 軸
  scale_fill_manual(
    values = c(
      "Low (<5%)" = "#d73027",
      "Moderate (5–10%)" = "#fee090",
      "High (>10%)" = "#4575b4"
    ),
    name = "Weight Loss %"
  ) +
  labs(
    title = "控糖減重成效 - 體重下降比例（過重或肥胖）",
    x = "Sex",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 14)
  )


# 轉為互動式圖表
bar_1_ob <- ggplotly(p_ob, tooltip = "text", width = 600, height = 500)

htmlwidgets::saveWidget(bar_1_ob, "bar_1_ob.html")




# 控糖減重成效 - 脂肪與肌肉在體重下降中的平均比例
# Stacked bar plot of composition of weight loss (N = 1315)
## Criteria: filter out weight gain, fat gain, muscle gain and NA 
stat_data_wl$gp <- cut(
  stat_data_wl$weight_loss_percent,
  breaks = c(-100, 5, 10, 100),
  labels = c("Low", "Moderate", "High")
)

bar_2_summary <- stat_data_wl %>%
  filter(delta_weight > 0 &
           !is.na(delta_body_fat_mass) &
           !is.na(delta_muscle_mass) &
           delta_body_fat_mass > 0 &
           delta_muscle_mass > 0) %>%
  mutate(
    fat_loss_pct = delta_body_fat_mass / delta_weight,
    muscle_loss_pct = delta_muscle_mass / delta_weight) %>%
  group_by(sex, gp) %>%
  summarise(
    n = n(),
    avg_weight_loss = mean(delta_weight, na.rm = TRUE),
    Fat = mean(fat_loss_pct, na.rm = TRUE),
    Muscle = mean(muscle_loss_pct, na.rm = TRUE),
    Others = 1 - Fat - Muscle)


df_bar_2 <- bar_2_summary %>%
  pivot_longer(cols = c(Fat, Muscle, Others),
               names_to = "component", values_to = "value") %>%
  mutate(
    text = paste0(
    #  "Sex: ", sex, "\n",
    #  "Group: ", wl_gp, "\n",
      "Component: ", component, "\n",
      "Mean: ", scales::percent(value, accuracy = 0.1), "\n",
      "Total N = ", n
      ))



p2 <- ggplot(df_bar_2, aes(x = gp, y = value, fill = component, text = text)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ sex, switch = "x") +
  labs(
    x = "",  # x 軸不顯示標籤
    y = "mean percentage",
    fill = "Component",
    title = "控糖減重成效 - 脂肪與肌肉在體重下降中的平均比例"
  ) +
  scale_fill_manual(
    values = c("Fat" = "#257D8B",
               "Muscle" = "#ED8D5A",
               "Others" = "#EFCE87"),
    labels = c("Fat", "Muscle", "Others")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    labels = percent_format(accuracy = 1)  # 顯示百分比，如 60%
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 14)
  )

bar_2 <- ggplotly(p2, tooltip = "text", width = 600, height = 500)

htmlwidgets::saveWidget(bar_2, "bar_2.html")





# 控糖減重成效-各組平均體重變化 (N = 1808)
# bar plot of mean delta weight of each group
df_delta_weight <- stat_data_wl %>%
 # filter(delta_weight > 0 & !is.na(gp) & !is.na(sex)) %>%
  group_by(sex, gp) %>%
  summarise(
    n = n(),
    mean_weight_loss = mean(delta_weight, na.rm = TRUE),
    se = sd(delta_weight, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    text = paste0(
      "Group: ", sex, " - ", gp, "\n",
      "Mean Δweight: ", round(mean_weight_loss, 1), " kg\n",
      "SE: ±", round(se, 2), " kg\n",
      "n = ", n
    )
  )


p3_wl <- ggplot(df_delta_weight, aes(x = gp, y = mean_weight_loss, fill = sex, text = text)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = mean_weight_loss - se, ymax = mean_weight_loss + se),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  labs(
    x = "",
    y = "Mean ± SE \n Weight loss (kg)",
    title = "控糖減重成效 - 各組平均體重變化",
    fill = "Sex"
  ) +
  scale_fill_manual(
    values = c("female" = "#E3625D",
               "male" = "#3E90BF"))+
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

bar_3_wl <- ggplotly(p3_wl, tooltip = "text", width = 600, height = 500)

htmlwidgets::saveWidget(bar_3_wl, "bar_3_wl.html")





# 控糖減重成效-各組平均脂肪變化 (N = 1808)
# bar plot of mean delta fat mass of each group
df_delta_fat <- stat_data_wl %>%
  # filter(delta_weight > 0 & !is.na(gp) & !is.na(sex)) %>%
  group_by(sex, gp) %>%
  summarise(
    n = n(),
    mean_fat_loss = mean(delta_body_fat_mass, na.rm = TRUE),
    se = sd(delta_body_fat_mass, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    text = paste0(
      "Group: ", sex, " - ", gp, "\n",
      "Mean Δfat: ", round(mean_fat_loss, 1), " kg\n",
      "SE: ±", round(se, 2), " kg\n",
      "n = ", n
    )
  )


p3_fl <- ggplot(df_delta_fat, aes(x = gp, y = mean_fat_loss, fill = sex, text = text)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = mean_fat_loss - se, ymax = mean_fat_loss + se),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  labs(
    x = "",
    y = "Mean ± SE \n Fat loss (kg)",
    title = "控糖減重成效 - 各組平均脂肪變化",
    fill = "Sex"
  ) +
  scale_fill_manual(
    values = c("female" = "#E3625D",
               "male" = "#3E90BF"))+
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

bar_3_fl <- ggplotly(p3_fl, tooltip = "text", width = 600, height = 500)

htmlwidgets::saveWidget(bar_3_fl, "bar_3_fl.html")






# 控糖減重成效-各組平均肌肉變化 (N = 1808)
# bar plot of mean delta fat mass of each group
df_delta_muscle <- stat_data_wl %>%
  # filter(delta_weight > 0 & !is.na(gp) & !is.na(sex)) %>%
  group_by(sex, gp) %>%
  summarise(
    n = n(),
    mean_muscle_loss = mean(delta_muscle_mass, na.rm = TRUE),
    se = sd(delta_muscle_mass, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    text = paste0(
      "Group: ", sex, " - ", gp, "\n",
      "Mean Δmuscle: ", round(mean_muscle_loss, 1), " kg\n",
      "SE: ±", round(se, 2), " kg\n",
      "n = ", n
    )
  )


p3_ml <- ggplot(df_delta_muscle, aes(x = gp, y = mean_muscle_loss, fill = sex, text = text)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(
    aes(ymin = mean_muscle_loss - se, ymax = mean_muscle_loss + se),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  labs(
    x = "",
    y = "Mean ± SE \n Muscle loss (kg)",
    title = "控糖減重成效 - 各組平均肌肉變化",
    fill = "Sex"
  ) +
  scale_fill_manual(
    values = c("female" = "#E3625D",
               "male" = "#3E90BF"))+
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major.x = element_blank()
  )

bar_3_ml <- ggplotly(p3_ml, tooltip = "text", width = 600, height = 500)

htmlwidgets::saveWidget(bar_3_ml, "bar_3_ml.html")





# 前後變化線條圖
stat_data_long_wl <- stat_data_wl %>%
  select(client_id, weight_w0, weight_w8,
         bmi_w0, bmi_w8,
         body_fat_mass_w0, body_fat_mass_w8,
         body_fat_mass_percentage_w0, body_fat_mass_percentage_w8,
         muscle_mass_w0, muscle_mass_w8,
         vfa_w0, vfa_w8,
         waist_circumference_w0, waist_circumference_w8,
         weight_without_fat_w0, weight_without_fat_w8,
        # bmr (有缺)
        hba1c_base, hba1c_end,
        glucose_ac_base, glucose_ac_end,
        insulin_base, insulin_end,
        homa_ir_base, homa_ir_end,
        homa_beta_base, homa_beta_end,
        triglyceride_base, triglyceride_end,
        total_cholesterol_base, total_cholesterol_end,
        high_density_lipoprotein_base, high_density_lipoprotein_end,
        low_density_lipoprotein_cholesterol_base, low_density_lipoprotein_cholesterol_end,
        uric_acid_base, uric_acid_end,
        amylase_base, amylase_end,
        lipase_base, lipase_end)



# 建立清洗函數（可以移除非數字、空白、逗號等）
clean_numeric <- function(x) {
  x <- trimws(x)
  x <- gsub(",", "", x)  # 去掉千分位逗號
  x <- gsub("[^0-9.]", "", x)  # 保留數字和小數點
  as.numeric(x)
}

# 將 character 欄位轉換成數字
stat_data_long_wl_clean <- stat_data_long_wl %>%
  mutate(across(where(is.character), clean_numeric))

percentage_data <- stat_data_long_wl_clean %>%
  mutate(
    weight_pct = if_else(!is.na(weight_w0) & !is.na(weight_w8), 
                         (weight_w8 - weight_w0) / weight_w0 * 100, NA_real_),
    
    bmi_pct = if_else(!is.na(bmi_w0) & !is.na(bmi_w8), 
                      (bmi_w8 - bmi_w0) / bmi_w0 * 100, NA_real_),
    
    body_fat_mass_pct = if_else(!is.na(body_fat_mass_w0) & !is.na(body_fat_mass_w8), 
                                (body_fat_mass_w8 - body_fat_mass_w0) / body_fat_mass_w0 * 100, NA_real_),
    
    body_fat_mass_percentage_pct = if_else(!is.na(body_fat_mass_percentage_w0) & !is.na(body_fat_mass_percentage_w8), 
                                           (body_fat_mass_percentage_w8 - body_fat_mass_percentage_w0) / body_fat_mass_percentage_w0 * 100, NA_real_),
    
    muscle_mass_pct = if_else(!is.na(muscle_mass_w0) & !is.na(muscle_mass_w8), 
                              (muscle_mass_w8 - muscle_mass_w0) / muscle_mass_w0 * 100, NA_real_),
    
    vfa_pct = if_else(!is.na(vfa_w0) & !is.na(vfa_w8), 
                      (vfa_w8 - vfa_w0) / vfa_w0 * 100, NA_real_),
    
    waist_circumference_pct = if_else(!is.na(waist_circumference_w0) & !is.na(waist_circumference_w8), 
                                      (waist_circumference_w8 - waist_circumference_w0) / waist_circumference_w0 * 100, NA_real_),
    
    weight_without_fat_pct = if_else(!is.na(weight_without_fat_w0) & !is.na(weight_without_fat_w8), 
                                     (weight_without_fat_w8 - weight_without_fat_w0) / weight_without_fat_w0 * 100, NA_real_),
    
    hba1c_pct = if_else(!is.na(hba1c_base) & !is.na(hba1c_end), 
                        (hba1c_end - hba1c_base) / hba1c_base * 100, NA_real_),
    
    glucose_ac_pct = if_else(!is.na(glucose_ac_base) & !is.na(glucose_ac_end), 
                             (glucose_ac_end - glucose_ac_base) / glucose_ac_base * 100, NA_real_),
    
    insulin_pct = if_else(!is.na(insulin_base) & !is.na(insulin_end), 
                          (insulin_end - insulin_base) / insulin_base * 100, NA_real_),
    
    homa_ir_pct = if_else(!is.na(homa_ir_base) & !is.na(homa_ir_end), 
                          (homa_ir_end - homa_ir_base) / homa_ir_base * 100, NA_real_),
    
    homa_beta_pct = if_else(!is.na(homa_beta_base) & !is.na(homa_beta_end), 
                            (homa_beta_end - homa_beta_base) / homa_beta_base * 100, NA_real_),
    
    triglyceride_pct = if_else(!is.na(triglyceride_base) & !is.na(triglyceride_end), 
                               (triglyceride_end - triglyceride_base) / triglyceride_base * 100, NA_real_),
    
    total_cholesterol_pct = if_else(!is.na(total_cholesterol_base) & !is.na(total_cholesterol_end), 
                                    (total_cholesterol_end - total_cholesterol_base) / total_cholesterol_base * 100, NA_real_),
    
    high_density_lipoprotein_pct = if_else(!is.na(high_density_lipoprotein_base) & !is.na(high_density_lipoprotein_end), 
                                           (high_density_lipoprotein_end - high_density_lipoprotein_base) / high_density_lipoprotein_base * 100, NA_real_),
    
    low_density_lipoprotein_cholesterol_pct = if_else(!is.na(low_density_lipoprotein_cholesterol_base) & !is.na(low_density_lipoprotein_cholesterol_end), 
                                                      (low_density_lipoprotein_cholesterol_end - low_density_lipoprotein_cholesterol_base) / low_density_lipoprotein_cholesterol_base * 100, NA_real_),
    
    uric_acid_pct = if_else(!is.na(uric_acid_base) & !is.na(uric_acid_end), 
                            (uric_acid_end - uric_acid_base) / uric_acid_base * 100, NA_real_),
    
    amylase_pct = if_else(!is.na(amylase_base) & !is.na(amylase_end), 
                          (amylase_end - amylase_base) / amylase_base * 100, NA_real_),
    
    lipase_pct = if_else(!is.na(lipase_base) & !is.na(lipase_end), 
                         (lipase_end - lipase_base) / lipase_base * 100, NA_real_)
  )


# 先選取所有 *_pct 欄位
mean_pct_data <- percentage_data %>%
  select(client_id, ends_with("_pct")) %>%
  pivot_longer(-client_id,
               names_to = "variable",
               values_to = "percent_change") %>%
  mutate(
    variable = str_replace(variable, "_pct$", "")  # 去掉 _pct，對應原始變數名稱
  ) %>%
  group_by(variable) %>%
  summarise(mean_percent_change = mean(percent_change, na.rm = TRUE), .groups = "drop")


stat_data_long_wl_clean <- stat_data_long_wl_clean %>%
  pivot_longer(
    cols = -client_id,
    names_to = c("variable", "Time"),
    names_pattern = "(.*)_(w0|w8|base|end)",
    values_to = "Value"
  ) %>%
  mutate(
    Time = recode(Time,
                  "w0" = "Before",
                  "base" = "Before",
                  "w8" = "After",
                  "end" = "After"),
    Time = factor(Time, levels = c("Before", "After"))
  ) 


mean_data <- stat_data_long_wl_clean %>%
  group_by(variable, Time) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE), .groups = "drop")

# 合併百分比資訊到 mean_data（只保留 After 時點加文字）
#mean_data_labeled <- mean_data %>%
#  filter(Time == "After") %>%
#  left_join(mean_pct_data, by = "variable") %>%
#  mutate(label = paste0(round(mean_percent_change, 2), "%"))

label_midpoints <- mean_data %>%
  pivot_wider(names_from = Time, values_from = mean_value) %>%
  mutate(
    mid_x = 1.95,  # x 軸是 factor，Before = 1, After = 2，中心是 1.5
    mid_y = (Before + After) / 2  # 線條中間的 y 值
  ) %>%
  left_join(mean_pct_data, by = "variable") %>%
  mutate(label = paste0(round(mean_percent_change, 2), "%"))


# recode variables name
mean_data <- mean_data %>%
  mutate(
    variable = recode(variable,
                      "weight" = "體重",
                      "bmi" = "BMI",
                      "body_fat_mass" = "體脂重",
                      "body_fat_mass_percentage" = "體脂率",
                      "muscle_mass" = "骨骼肌重",
                      "vfa" = "內臟脂肪",
                      "waist_circumference" = "腰圍",
                      "weight_without_fat" = "除脂體重",
                      "hba1c" = "糖化血色素",
                      "glucose_ac" = "空腹血糖",
                      "insulin" = "空腹胰島素",
                      "homa_ir" = "胰島素阻抗值",
                      "homa_beta" = "β細胞功能",
                      "triglyceride" = "三酸甘油脂",
                      "total_cholesterol" = "總膽固醇",
                      "high_density_lipoprotein" = "高密度脂蛋白",
                      "low_density_lipoprotein_cholesterol" = "低密度脂蛋白",
                      "uric_acid" = "尿酸",
                      "amylase" = "澱粉酶",
                      "lipase" = "解脂酶" ))
mean_data$variable <- factor(mean_data$variable, levels = c(
                        "體重", "BMI", "體脂重", "體脂率", "骨骼肌重", "內臟脂肪", "腰圍", "除脂體重",
                        "糖化血色素", "空腹血糖", "空腹胰島素", "胰島素阻抗值", "β細胞功能",
                        "三酸甘油脂", "總膽固醇", "高密度脂蛋白", "低密度脂蛋白",
                        "尿酸", "澱粉酶", "解脂酶"
                      ))

label_midpoints <- label_midpoints %>%
  mutate(
    variable = recode(variable,
                      "weight" = "體重",
                      "bmi" = "BMI",
                      "body_fat_mass" = "體脂重",
                      "body_fat_mass_percentage" = "體脂率",
                      "muscle_mass" = "骨骼肌重",
                      "vfa" = "內臟脂肪",
                      "waist_circumference" = "腰圍",
                      "weight_without_fat" = "除脂體重",
                      "hba1c" = "糖化血色素",
                      "glucose_ac" = "空腹血糖",
                      "insulin" = "空腹胰島素",
                      "homa_ir" = "胰島素阻抗值",
                      "homa_beta" = "β細胞功能",
                      "triglyceride" = "三酸甘油脂",
                      "total_cholesterol" = "總膽固醇",
                      "high_density_lipoprotein" = "高密度脂蛋白",
                      "low_density_lipoprotein_cholesterol" = "低密度脂蛋白",
                      "uric_acid" = "尿酸",
                      "amylase" = "澱粉酶",
                      "lipase" = "解脂酶" ))
label_midpoints$variable <- factor(label_midpoints$variable, levels = c(
  "體重", "BMI", "體脂重", "體脂率", "骨骼肌重", "內臟脂肪", "腰圍", "除脂體重",
  "糖化血色素", "空腹血糖", "空腹胰島素", "胰島素阻抗值", "β細胞功能",
  "三酸甘油脂", "總膽固醇", "高密度脂蛋白", "低密度脂蛋白",
  "尿酸", "澱粉酶", "解脂酶"
))



line_plot <- 
  ggplot(mean_data, aes(x = Time, y = mean_value, group = variable)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  geom_text(
    data = label_midpoints,
    aes(x = mid_x, y = mid_y, label = label),
    size = 3.5,
    show.legend = FALSE,
    color = "black"
  ) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "成效（Mean）", title = "")+
  xlim("Before", "After") +
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.3))) +
  facet_wrap(vars(variable), scales = "free") +
  theme_linedraw() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 15),
    text = element_text(family = "noto", size = 12)
  ) 


ggsave("line_plot.png", plot = line_plot, width = 8.14, height = 6.64, dpi = 300, units = "in")




  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
############################## current not necessary ###########################3  
  # 互動式 stacked bar（tertile group）
  long_data_clean <- stat_data_wl %>%
    pivot_longer(cols = ends_with("_loss_percent"),
                 names_to = "loss_type",
                 values_to = "loss_value") %>%
    mutate(
      loss_type = recode(loss_type,
                         "weight_loss_percent" = "Weight",
                         "fat_loss_percent" = "Fat",
                         "muscle_loss_percent" = "Muscle")
    ) %>%
    filter(!is.na(loss_value)) %>%
    group_by(loss_type) %>%
    mutate(
      total_n = n(),
      tertile_num = ntile(loss_value, 3),
      tertile = case_when(
        tertile_num == 1 ~ "High",
        tertile_num == 2 ~ "Moderate",
        tertile_num == 3 ~ "Low"
      ),
      tertile = factor(tertile, levels = c("High", "Moderate", "Low"))
    ) %>%
    ungroup()
  
  # 設定 facet 標籤
  facet_labels <- long_data_clean %>%
    group_by(loss_type, total_n) %>%
    summarise(.groups = "drop") %>%
    mutate(loss_type_label = paste0(loss_type, " (N=", total_n, ")")) %>% 
    select(loss_type, loss_type_label)
  
  # 整理成繪圖用資料
  plot_data <- long_data_clean %>%
    left_join(facet_labels, by = "loss_type") %>%
    group_by(sex, loss_type_label, tertile) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(sex, loss_type_label) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  # 畫 ggplot
  p <- ggplot(plot_data, aes(x = sex, y = prop, fill = tertile, text = paste0("Tertile: ", tertile, "<br>N: ", n, "<br>Proportion: ", scales::percent(prop, accuracy = 0.1)))) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~ loss_type_label) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = c("High" = "#4575b4", "Moderate" = "#fee090", "Low" = "#d73027")) +
    labs(
      title = "控糖減重成效-身體組成",
      x = NULL,
      y = "Percentage",
      fill = "Tertile"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0, margin = margin(b = 10)# 調整標題大小、加粗、置中
      ))
  
  # 轉為互動式 plotly
  ggplotly(p, tooltip = "text")
  
  


