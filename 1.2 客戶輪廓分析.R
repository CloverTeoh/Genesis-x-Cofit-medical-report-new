library(pacman)
pacman::p_load(googleVis, magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, extraInserts, ggvenn, ggplot2,knitr, kableExtra, openxlsx, lubridate, cowplot, ggpubr, webshot)

# data_cleaning
## dataset_input
setwd("/Users/clover/Documents/cofit/gitbook")
getwd()
stat_data <- read_excel("client_intervention_data_OGIRT_classified_20250529.xlsx")

## define DM
stat_table <- stat_table %>%
  mutate(DM_base = case_when(
    is.na(hba1c_base) & is.na(glucose_ac_base) & is.na(glucose_pc_2hr_base) ~ "Unclassified",
    
    hba1c_base >= 6.5 | glucose_ac_base >= 126 | glucose_pc_2hr_base >= 200 ~ "DM",
    
    hba1c_base >= 5.7 & hba1c_base < 6.5 |
      glucose_ac_base >= 100 & glucose_ac_base < 126 |
      glucose_pc_2hr_base >= 140 & glucose_pc_2hr_base < 200 ~ "Pre-DM",
    
    !is.na(hba1c_base) | !is.na(glucose_ac_base) | !is.na(glucose_pc_2hr_base) ~ "Normal",
    
    TRUE ~ "Unclassified"
  ))

## define_metabolic syndrome
stat_table <- stat_table %>% 
  mutate(
    met_abdominal = ifelse((sex == "male" & waist_circumference_w0  >= 90) |
                             (sex == "female" & waist_circumference_w0 >= 80), 1, 0),
    
    met_tg = ifelse(triglyceride_base >= 150, 1, 0),
    
    met_hdl = ifelse((sex == "male" & high_density_lipoprotein_base < 40) |
                       (sex == "female" & high_density_lipoprotein_base < 50), 1, 0),
    
    met_bp = ifelse(systolic_blood_pressure_base >= 130 | diastolic_blood_pressure_base >= 85, 1, 0),
    
    met_fbg = ifelse(glucose_ac_base >= 100, 1, 0)
  ) %>%
  rowwise() %>%
  mutate(
    met_sum = sum(c_across(c(met_abdominal, met_tg, met_hdl, met_bp, met_fbg)), na.rm = TRUE),
    met_n = sum(!is.na(c_across(c(met_abdominal, met_tg, met_hdl, met_bp, met_fbg)))),
    metax_base = case_when(
      met_sum >= 3 ~ "Metabolic Syndrome",
      met_n < 5 ~ "Unclassified",
      TRUE ~ "Normal"
    )
  ) %>%
  ungroup()

## define_hypertension
stat_table <- stat_table %>%
  mutate(htn_base = case_when(
    is.na(systolic_blood_pressure_base) & is.na(diastolic_blood_pressure_base) ~ "Unclassified",
    
    systolic_blood_pressure_base >= 140 | diastolic_blood_pressure_base >= 90 ~ "HTN (Stage 2)",
    
    systolic_blood_pressure_base >= 130 & systolic_blood_pressure_base < 140 | diastolic_blood_pressure_base >= 80 & diastolic_blood_pressure_base < 90 ~ "HTN (Stage 1)",
    
    systolic_blood_pressure_base >= 120 & systolic_blood_pressure_base < 130 & diastolic_blood_pressure_base < 80 ~ "Elevated",
    
    systolic_blood_pressure_base < 120 & diastolic_blood_pressure_base < 80 ~ "Normal",
    
    !is.na(systolic_blood_pressure_base) | !is.na(diastolic_blood_pressure_base) ~ "Unclassified",
    
    TRUE ~ "Unclassified"
  ))

## define_hyperlipidemia
stat_table <- stat_table %>%
  mutate(HLP_base = case_when(
    is.na(total_cholesterol_base) & is.na(low_density_lipoprotein_cholesterol_base) & is.na(high_density_lipoprotein_base) & is.na(triglyceride_base) ~ "Unclassified",
    
    total_cholesterol_base >= 200 |
      low_density_lipoprotein_cholesterol_base >= 130 |
      (sex == "male" & high_density_lipoprotein_base < 40) |
      (sex == "female" & high_density_lipoprotein_base < 50) |
      triglyceride_base >= 150 ~ "HLP",
    
    total_cholesterol_base < 200 &
      low_density_lipoprotein_cholesterol_base < 130 &
      ((sex == "male" & high_density_lipoprotein_base >= 40) | (sex == "female" & high_density_lipoprotein_base >= 50)) &
      triglyceride_base < 150 ~ "Normal",
    
    TRUE ~ "Unclassified"
  ))


na_stats <- data.frame(
  Variable = names(stat_table),
  Non_NA_Count = sapply(names(stat_table), function(col) {
    if (col %in% special_cols) {
      sum(!is.na(stat_table[[col]]) & stat_table[[col]] != "Unclassified")
    } else {
      sum(!is.na(stat_table[[col]]))
    }
  }),
  NA_Count = sapply(names(stat_table), function(col) {
    if (col %in% special_cols) {
      sum(is.na(stat_table[[col]]) | stat_table[[col]] == "Unclassified")
    } else {
      sum(is.na(stat_table[[col]]))
    }
  })
)


# ## Analysis - 1: Baseline 分布 (Pie-chart) --------------------------------

#[Pie chart] 1. cut (cluster) 2. call pie chart
## Age
stat_table$age_gp <- cut(stat_table$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))

stat_pie_01 <- stat_table %>% filter(!is.na(age_gp)) %>% group_by(age_gp) %>% summarise(n = n())

n_pie_01 <- sum(stat_pie_01$n)

title_pie_01 <- paste0("Age (N = ", n_pie_01, ")")

pie_01 <- 
  stat_table %>% filter(!is.na(age_gp)) %>% group_by(age_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = title_pie_01,
                                                                                         legend = "{position:'right'}",
                                                                                         pieHole = 0.5,
                                                                                         #slices = "{1:{offset:0.1}}",
                                                                                         backgroundColor = "#f9fffb",
                                                                                         width = "500",
                                                                                         height = "400"))

plot(pie_01)




## Gender
stat_pie_02 <- stat_table %>% filter(!is.na(sex)) %>% group_by(sex) %>% summarise(n = n())

n_pie_02 <- sum(stat_pie_02$n)

title_pie_02 <- paste0("Gender (N = ", n_pie_02, ")")

pie_02 <- 
  stat_table %>% filter(!is.na(sex)) %>% group_by(sex) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = title_pie_02,
                                                                                                legend = "{position:'right'}",
                                                                                                pieHole = 0.5,
                                                                                                #slices = "{0:{offset:0.1}}",
                                                                                                backgroundColor = "#f9fffb",
                                                                                                colors = "['#DC3912', '#3366CC']",
                                                                                                width = "500",
                                                                                                height = "400"))
plot(pie_02)




## BMI x Obesity
stat_table$bmi_gp <- cut(stat_table$bmi_w0, c(0,18.5,24,27,100), c("Underweight", "Normal", "Overweight", "Obesity"))

stat_table$bmi_gp_2 <- stat_table$bmi_w0 %>% cut(c(0,18.5,24,27, 30, 35 ,100), c("Underweight", "Normal", "Overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)


stat_pie_03 <- stat_table %>% filter(sex == "male" & !is.na(bmi_gp)) %>% group_by(bmi_gp) %>% summarise(n = n())

n_pie_03 <- sum(stat_pie_03$n)

title_pie_03 <- paste0("Male (N = ", n_pie_03, ")")

pie_03 <- 
  stat_table %>% filter(sex == "male" & !is.na(bmi_gp)) %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = title_pie_03,
                                                                                                                             legend = "{position:'right'}",
                                                                                                                             pieHole = 0.5,
                                                                                                                             #slices = "{2:{offset:0.1}}",
                                                                                                                             backgroundColor = "#f9fffb",
                                                                                                                             width = "500",
                                                                                                                             height = "400"))
plot(pie_03)


stat_pie_04 <- stat_table %>% filter(sex == "female" & !is.na(bmi_gp)) %>% filter(bmi_gp_2 != "Underweight") %>% group_by(bmi_gp) %>% summarise(n = n())

n_pie_04 <- sum(stat_pie_04$n)

title_pie_04 <- paste0("Female (N = ", n_pie_04, ")")

pie_04 <- 
  stat_table %>% filter(sex == "female" & !is.na(bmi_gp)) %>% filter(bmi_gp_2 != "Underweight") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = title_pie_04,
                                                                                                                               legend = "{position:'right'}",
                                                                                                                               pieHole = 0.5,
                                                                                                                               #slices = "{1:{offset:0.1}}",
                                                                                                                               backgroundColor = "#f9fffb",
                                                                                                                               width = "500",
                                                                                                                               height = "400"))
plot(pie_04)



stat_pie_03_2 <- stat_table %>% filter(sex == "male" & !is.na(bmi_gp)) %>% group_by(bmi_gp_2) %>% summarise(n = n())

n_pie_03_2 <- sum(stat_pie_03_2$n)

title_pie_03_2 <- paste0("Male (N = ", n_pie_03_2, ")")

pie_03_2 <- 
  stat_table %>% filter(sex == "male" & !is.na(bmi_gp)) %>% group_by(bmi_gp_2) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = title_pie_03_2,
                                                                                                                               legend = "{position:'right'}",
                                                                                                                               pieHole = 0.5,
                                                                                                                               #slices = "{2:{offset:0.1}}",
                                                                                                                               backgroundColor = "#f9fffb",
                                                                                                                               width = "500",
                                                                                                                               height = "400"))
plot(pie_03_2)



stat_pie_04_2 <- stat_table %>% filter(sex == "female" & !is.na(bmi_gp)) %>% filter(bmi_gp_2 != "Underweight") %>% group_by(bmi_gp_2) %>% summarise(n = n())

n_pie_04_2 <- sum(stat_pie_04_2$n)

title_pie_04_2 <- paste0("Female (N = ", n_pie_04_2, ")")

pie_04_2 <- 
  stat_table %>% filter(sex == "female" & !is.na(bmi_gp)) %>% filter(bmi_gp_2 != "Underweight") %>% group_by(bmi_gp_2) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = title_pie_04_2,
                                                                                                                                                                       legend = "{position:'right'}",
                                                                                                                                                                       pieHole = 0.5,
                                                                                                                                                                       #slices = "{1:{offset:0.1}}",
                                                                                                                                                                       backgroundColor = "#f9fffb",
                                                                                                                                                                       width = "500",
                                                                                                                                                                       height = "400"))
plot(pie_04_2)



## Disease
  ### DM
stat_pie_05_DM <- stat_table %>% group_by(DM_base) %>% filter(DM_base != "Unclassified") %>% summarise(n = n())

n_pie_05_DM <- sum(stat_pie_05_DM$n)

title_pie_05_DM <- paste0("Diabetes (N = ", n_pie_05_DM, ")")

pie_05_DM <- 
  stat_table %>% group_by(DM_base) %>% filter(DM_base != "Unclassified") %>% summarise(n = n()) %>% gvisPieChart(options = list(title = title_pie_05_DM,
                                                                                                                                legend = "{position:'right'}",
                                                                                                                                pieHole = 0.5,
                                                                                                                                #slices = "{1:{offset:0.1}}",
                                                                                                                                backgroundColor = "#f9fffb",
                                                                                                                                width = "500",
                                                                                                                                height = "400"))
plot(pie_05_DM)



  ### HTN
stat_pie_05_HTN <-  stat_table %>% group_by(htn_base) %>% filter(htn_base != "Unclassified") %>% summarise(n = n())

n_pie_05_HTN <- sum(stat_pie_05_HTN$n)

title_pie_05_HTN <- paste0("Hypertension (N = ", n_pie_05_HTN, ")")

pie_05_HTN <- 
  stat_table %>% group_by(htn_base) %>% filter(htn_base != "Unclassified") %>% summarise(n = n()) %>% gvisPieChart(options = list(title = title_pie_05_HTN,
                                                                                                                                              legend = "{position:'right'}",
                                                                                                                                              pieHole = 0.5,
                                                                                                                                              #slices = "{1:{offset:0.1}}",
                                                                                                                                              backgroundColor = "#f9fffb",
                                                                                                                                              width = "500",
                                                                                                                                              height = "400"))

plot(pie_05_HTN)
stat_table %>% count(htn_base)


  ### HLP
#HLP
stat_pie_05_HLP <-  stat_table %>% group_by(HLP_base) %>% filter(HLP_base != "Unclassified") %>% summarise(n = n())

n_pie_05_HLP <- sum(stat_pie_05_HLP$n)

title_pie_05_HLP <- paste0("Hyperlipidemia (N = ", n_pie_05_HLP, ")")

pie_05_HLP <- 
  stat_table %>% group_by(HLP_base) %>% filter(HLP_base != "Unclassified") %>% summarise(n = n()) %>% gvisPieChart(options = list(title = title_pie_05_HLP,
                                                                                                                                                 legend = "{position:'right'}",
                                                                                                                                                 pieHole = 0.5,
                                                                                                                                                 #slices = "{1:{offset:0.1}}",
                                                                                                                                                 backgroundColor = "#f9fffb",
                                                                                                                                                 width = "500",
                                                                                                                                                 height = "400"))
plot(pie_05_HLP)
stat_table %>% count(HLP_base)



  ### Metabolic syndrome
stat_pie_05_MetX <-  stat_table %>% filter(metax_base != "Unclassified") %>% group_by(metax_base) %>%  summarise(n = n())

n_pie_05_MetX <- sum(stat_pie_05_MetX$n)

title_pie_05_MetX <- paste0("Metabolic Syndrome (N = ", n_pie_05_MetX, ")")

pie_05_MetX <- 
  stat_table %>% filter(metax_base != "Unclassified") %>% group_by(metax_base) %>%  summarise(n = n()) %>% gvisPieChart(options = list(title = title_pie_05_MetX,
                                                                                                                                                   legend = "{position:'right'}",
                                                                                                                                                   pieHole = 0.5,
                                                                                                                                                   #slices = "{1:{offset:0.1}}",
                                                                                                                                                   backgroundColor = "#f9fffb",
                                                                                                                                                   width = "500",
                                                                                                                                                   height = "400"))
plot(pie_05_MetX)
stat_table %>% count(metax_base)



  ### Insulin Pattern
stat_pie_05_Ins <-  stat_table %>% filter(summary_pattern_base_v1 != "Unclassified" & !is.na(summary_pattern_base_v1)) %>% 
  group_by(summary_pattern_base_v1) %>% summarise(n = n())

n_pie_05_Ins <- sum(stat_pie_05_Ins$n)

title_pie_05_Ins <- paste0("Insulin Response Pattern (N = ", n_pie_05_Ins, ")")

pie_05_Ins <-
  stat_table %>% filter(summary_pattern_base_v1 != "Unclassified" & !is.na(summary_pattern_base_v1)) %>% 
  group_by(summary_pattern_base_v1) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = title_pie_05_Ins,
                                                                                           legend = "{position:'right'}",
                                                                                           pieHole = 0.5,
                                                                                           #slices = "{1:{offset:0.1}}",
                                                                                           colors="['#628bd6','#f8e05c','#ffc081','#ff834a','#ff5959']",
                                                                                           backgroundColor = "#f9fffb",
                                                                                           width = "500",
                                                                                           height = "400"))
                                                                                             
                                                                                                                                             
                                                     
plot(pie_05_Ins)
stat_table %>% count(summary_pattern_base_v1)


#p1 <- gvisMerge(pie_01, pie_02, horizontal = TRUE,
#                tableOptions = "cellspacing=\"5\" bgcolor=\"#AABBCC\"")

#plot(p1, tag = "chart", file = "pie_age_gender.html")

#p2 <- gvisMerge(pie_03, pie_04, horizontal = TRUE,
#                tableOptions = "cellspacing=\"5\" bgcolor=\"#AABBCC\"")

#print(p2, tag = "chart", file = "pie_bmi.html")

#p3 <- gvisMerge(gvisMerge(pie_05_DM, pie_05_Ins, horizontal = TRUE),
#                gvisMerge(pie_05_HTN, pie_05_HLP,horizontal = TRUE),
#                tableOptions = "cellspacing=\"5\" bgcolor=\"#AABBCC\"")
                
#print(pie_05_MetX, tag = "chart", file = "pie_disease_1.html")
#print(p3, tag = "chart", file = "pie_disease_2.html")
      
                                               
print(pie_01, tag = "chart", file = "pie_01.html")
print(pie_02, tag = "chart", file = "pie_02.html")
print(pie_03, tag = "chart", file = "pie_03.html")
print(pie_04, tag = "chart", file = "pie_04.html")
print(pie_03_2, tag = "chart", file = "pie_03_2.html")
print(pie_04_2, tag = "chart", file = "pie_04_2.html")
print(pie_05_DM, tag = "chart", file = "pie_05_DM.html")
print(pie_05_HLP, tag = "chart", file = "pie_05_HLP.html")
print(pie_05_HTN, tag = "chart", file = "pie_05_HTN.html")
print(pie_05_Ins, tag = "chart", file = "pie_05_Ins.html")
print(pie_05_MetX, tag = "chart", file = "pie_05_MetX.html")





