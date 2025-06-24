
# Load relevant packages
library(haven)
library(plyr)
library(purrr)
library(broom)
library(estimatr)
library(ggeffects)
library(ggh4x)
library(ggimage)
library(patchwork)
library(here)
library(tidyverse)

# path to data folder
path_data_in    <- paste0(dirname(here()), "/Data/CILS")


### Data Preparation ###
########################
##
preprocess_data <- function(path_data_in,
                            subpath_data_in,
                            prefix,
                            wave,
                            missing_condition) {
  data <- read_dta(
    paste0(
      path_data_in,
      "/",
      subpath_data_in
    ),
    encoding = "latin1"
  )

  prefix_vars <- data %>%
    select(starts_with(paste0(prefix, "_"))) %>%
    colnames()

  data %>%
    mutate_all(~ replace(., . < missing_condition, NA)) %>%
    mutate_if(
      is.labelled,
      ~ as_factor(.) %>% fct_drop()
    ) %>%
    mutate(wave = wave) %>%
    rename_at(prefix_vars, ~ substr(., start = 4, stop = 1000)) %>%
    select(youthid, wave, everything())
}


firstvalid <- function(v) {
  v[!is.na(v)][1]
}


## Main study data
cils_main_w1_ge <- preprocess_data(path_data_in = path_data_in,
                subpath_data_in = "/Licensed version 1.2.0/Data Sets/full version/youth main/w1_ym_ge_v1.2.0.dta",
                prefix = "y1",
                wave = 1,
                missing_condition = 0
              ) 


cils_main_w2_ge <- preprocess_data(path_data_in = path_data_in,
                subpath_data_in = "/Licensed version 2.3.0/Data Sets/full version/youth main/w2_ym_ge_v2.3.0.dta",
                prefix = "y2",
                wave = 2,
                missing_condition = 0
              ) 


cils_main_w3_ge <- preprocess_data(path_data_in = path_data_in,
                subpath_data_in = "/Licensed version 3.3.0/Data Sets/full version/youth main/w3_ym_ge_v3.3.0.dta",
                prefix = "y3",
                wave = 3,
                missing_condition = 0
              ) 


cils_main_w4_ge <- preprocess_data(path_data_in = path_data_in,
                subpath_data_in = "CILS4EU-DE v7.0.0/ZA6656_CILS4EU-DE_v7-0-0_stata/w4_ym_ge_v7.0.0_rv.dta",
                prefix = "y4",
                wave = 4,
                missing_condition = 0
              ) 


cils_main_w5_ge <- preprocess_data(path_data_in = path_data_in,
                subpath_data_in = "CILS4EU-DE v7.0.0/ZA6656_CILS4EU-DE_v7-0-0_stata/w5_ym_ge_v7.0.0_rv.dta",
                prefix = "y5",
                wave = 5,
                missing_condition = 0
              ) 


cils_main_w6_ge <- preprocess_data(path_data_in = path_data_in,
                subpath_data_in = "CILS4EU-DE v7.0.0/ZA6656_CILS4EU-DE_v7-0-0_stata/w6_ym_ge_v7.0.0_rv.dta",
                prefix = "y6",
                wave = 6,
                missing_condition = 0
              )               


cils_main_w7_ge <- preprocess_data(path_data_in = path_data_in,
                subpath_data_in = "CILS4EU-DE v7.0.0/ZA6656_CILS4EU-DE_v7-0-0_stata/w7_ym_ge_v7.0.0_rv.dta",
                prefix = "y7",
                wave = 7,
                missing_condition = 0
              ) 


cils_main_w8_ge <- preprocess_data(path_data_in = path_data_in,
                subpath_data_in = "CILS4EU-DE v7.0.0/ZA6656_CILS4EU-DE_v7-0-0_stata/w8_ym_ge_v7.0.0_rv.dta",
                prefix = "y8",
                wave = 8,
                missing_condition = 0
              )               


cils_main_w9_ge <- preprocess_data(path_data_in = path_data_in,
                subpath_data_in = "CILS4EU-DE v7.0.0/ZA6656_CILS4EU-DE_v7-0-0_stata/w9_ym_ge_v7.0.0_rv.dta",
                prefix = "y9",
                wave = 9,
                missing_condition = 0
              ) 



## Parental data

data_parents <- read_dta(paste0(path_data_in, "/", "/Licensed version 1.2.0/Data Sets/full version/parents/w1_p_ge_v1.2.0.dta"), encoding = "latin1")
data_parents_prefix_vars <- data_parents %>% select(starts_with(paste0("p1", "_"))) %>% colnames() 
data_parents_join_vars <- data_parents %>% select(!starts_with(paste0("p1", "_"))) %>% select(-youthid) %>% colnames() 
data_parents <- data_parents %>% select(-all_of(data_parents_join_vars))

cils_parent_w1_ge <-  data_parents %>%
    mutate_all(~replace(., .< 0, NA)) %>%
    mutate_if(
      is.labelled,
      ~as_factor(.)
    ) %>%
    rename_at(data_parents_prefix_vars, ~paste0("p_", substr(., start = 4, stop = 1000))) %>%
    mutate(parent_interview = 1, wave =1)


cils_full_ge <- cils_main_w1_ge %>% 
  left_join(cils_parent_w1_ge,  by = c("youthid", "wave")) 


# month of birth list
map_dobm <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december", NA)

    
# Prepare time-constant data (combine waves and pick first valid observation)
cils_religion <- cils_full_ge %>% select(-intdat_ymRV) %>%
  bind_rows(cils_main_w2_ge %>% mutate(svyunit_c = as.numeric(svyunit_c))) %>%
  bind_rows(cils_main_w3_ge %>% mutate(youthid = as.numeric(as.character(youthid)), svyunit_c = as.numeric(svyunit_c), svyunit_s = as.character(svyunit_s))) %>%
  bind_rows(cils_main_w4_ge %>% mutate(youthid = as.numeric(as.character(youthid))) %>% select(-intdat_ym)) %>%
  bind_rows(cils_main_w5_ge %>% mutate(youthid = as.numeric(as.character(youthid))) %>% select(-intdat_ym)) %>%
  bind_rows(cils_main_w6_ge %>% mutate(youthid = as.numeric(as.character(youthid))) %>% select(-c(intdat_ym, genflag_missG, genflag_ntG, coflag_missG, coflag_ntG))) %>%
  bind_rows(cils_main_w7_ge %>% mutate(youthid = as.numeric(as.character(youthid))) %>% select(-c(intdat_ym))) %>%
  bind_rows(cils_main_w8_ge %>% mutate(youthid = as.numeric(as.character(youthid))) %>% select(-c(intdat_ym))) %>%
  bind_rows(cils_main_w9_ge %>% mutate(youthid = as.numeric(as.character(youthid))) %>% select(-c(intdat_ym))) %>%
  transmute(

    totwgts = totwgts,
    calwgt = calwgt,

    youthid = youthid,
    wave = wave,
    month = mapvalues(dobm, map_dobm, c(1:12, NA)),    
    birth_date = make_date(year = as.character(doby), month = as.character(month)),
    birth_date = format(birth_date, "%Y-%m"),

    girl = ifelse(sex %in% c("girl", "female"), "Girl", "Boy"),
    rel1 = rel1,
    c_religion = case_when(
                  rel1 %in% c("no religion") ~"No religion",
                  rel1 %in% c("christianity", "christianity: catholic", "christianity: protestant", "christianity: other") ~"Christian",
                  rel1 %in% c("islam", "islam: sunnite", "islam: shiite", "islam: alevi", "islam: other") ~"Muslim",
                  rel1 %in% c("hinduism") ~"Hindu",
                  rel1 %in% c("buddhism") ~"Buddhist",
                  rel1 %in% c("judaism") ~ "Jew",
                  rel1 %in% c("sikhism") & wave != 2 ~ "Sikh",
                  rel1 %in% c("sikh") & wave == 2 ~ "Sikh",
                  TRUE ~ NA_character_
              ),
    c_religion_other = ifelse(rel1 %in% "other religion" | (rel1 == "sikhism" & wave == 2), "Other religion", NA_character_),

    # Code parental data to use if adolescent data is missing
    p_religion = case_when(
                    p_rel1 %in% c("no religion") ~"No religion",
                    p_rel1 %in% c("christianity", "christianity: catholic", "christianity: protestant") ~"Christian",
                    p_rel1 %in% c("islam") ~"Muslim",
                    p_rel1 %in% c("hinduism") ~"Hindu",
                    p_rel1 %in% c("buddhism") ~"Buddhist",
                    p_rel1 %in% c("judaism") ~ "Jew",
                    p_rel1 %in% c("sikhism") ~ "Sikh",
                    TRUE ~ NA_character_
                ),
    p_religion_other = ifelse(p_rel1 %in% "other religion:", "Other religion", NA_character_),

    religion = ifelse(is.na(c_religion), p_religion, c_religion),
    religion_other = ifelse(is.na(c_religion_other), p_religion_other, c_religion_other),
  ) %>%
  group_by(youthid) %>% 
  arrange(youthid, wave) %>% # pick first valid observation
  mutate_at(vars(-wave, -youthid), ~firstvalid(.)) %>% 
  filter(wave == first(wave)) %>% 
  ungroup() %>%
  mutate(religion = ifelse(is.na(religion) & religion_other == "Other religion", "Other religion", religion))

# Prepare time-varying data 
cils_long <- cils_full_ge %>% mutate(intdat_ymRV = as.factor(as.character(intdat_ymRV))) %>%
  bind_rows(cils_main_w2_ge %>% mutate(svyunit_c = as.numeric(svyunit_c))) %>%
  bind_rows(cils_main_w3_ge %>% mutate(youthid = as.numeric(as.character(youthid)), svyunit_c = as.numeric(svyunit_c), svyunit_s = as.character(svyunit_s))) %>%
  bind_rows(cils_main_w4_ge %>% mutate(youthid = as.numeric(as.character(youthid))) %>% select(-intdat_ym)) %>%
  bind_rows(cils_main_w5_ge %>% mutate(youthid = as.numeric(as.character(youthid))) %>% select(-intdat_ym)) %>%
  bind_rows(cils_main_w6_ge %>% mutate(youthid = as.numeric(as.character(youthid))) %>% select(-c(intdat_ym, genflag_missG, genflag_ntG, coflag_missG, coflag_ntG))) %>%
  bind_rows(cils_main_w7_ge %>% mutate(youthid = as.numeric(as.character(youthid))) %>% select(-c(intdat_ym))) %>%
  bind_rows(cils_main_w8_ge %>% mutate(youthid = as.numeric(as.character(youthid))) %>% select(-c(intdat_ym))) %>%
  bind_rows(cils_main_w9_ge %>% mutate(youthid = as.numeric(as.character(youthid))) %>% select(-c(intdat_ym))) %>%
  mutate(
    months_since_2010 = as.numeric(as.character(intdat_ymRV)) - 600,
    survey_date = format(ymd("2010-01-01") %m+% months(months_since_2010), "%Y-%m"),

    # Leisure time activities 
    lta_rel = 5 - as.numeric(lta1),
    lta_cin = 5 - as.numeric(lta2),
    lta_out = 5 - as.numeric(lta3),
    lta_read = 5 - as.numeric(lta4),
    lta_club = 5 - as.numeric(lta5),
    lta_mus = 5 - as.numeric(lta7), 
    lta_news = 5 - as.numeric(lta8),
    lta_tv = 5 - as.numeric(lta9),
    lta_chat = 5 - as.numeric(lta10),
    lta_house = 5 - as.numeric(lta12),
    lta_house8 = 5 - as.numeric(lta20),    
    lta_compa = 5 - as.numeric(lta13),
    lta_compo = 5 - as.numeric(lta14),
    lta_sport = 5 - as.numeric(heab3),
  )

# Combine longitudinal with time-constant data
cils_all <- cils_long %>% select(youthid, wave, starts_with("lta"), survey_date) %>%
    left_join(cils_religion %>% select(-wave), by = "youthid")


#####################
### Main Analysis ###
#####################

setwd(here("leisure-time-activities-replication"))

# Prepare wave 9 sports data to merge to wave 8 data
merge_w9 <- cils_all %>% 
  filter(wave == 9) %>%
  mutate(lta_sport9 = lta_sport, wave = 8) %>%
  select(youthid, wave, lta_sport9) 

# Names for leisure time activities
names <- tribble(
~names_orig, ~names_form, ~names_order, ~type,
"lta_sport", "Do sports", 1, "Out-of-home activities",
"lta_out",  "Go out", 2, "Out-of-home activities",
"lta_club", "Go to club", 3, "Out-of-home activities",
"lta_mus", "Go to museum", 4, "Out-of-home activities",
"lta_cin", "Go to cinema", 5, "Out-of-home activities",
"lta_rel", "Visit relatives", 6, "Out-of-home activities",

"lta_read", "Read a book", 7, "Activities at home",
"lta_news", "Read newspaper", 8, "Activities at home",
"lta_compa", "Play computer (alone)", 9, "Activities at home",
"lta_compo", "Play computer (w/ others)", 10, "Activities at home",
"lta_tv", "Watch TV", 11, "Activities at home",
"lta_chat", "Chat online", 12, "Activities at home",
"lta_house", "Help around house", 13, "Activities at home",
  )

# Prepare data set
prep_data <- cils_all %>% 
  filter(wave %in% c(1, 4, 8)) %>%
  left_join(merge_w9, by = c("youthid", "wave")) %>%
  mutate(
    lta_sport = ifelse(wave == 8, lta_sport9, lta_sport),
    lta_house = ifelse(wave == 8, lta_house8, lta_house)
  ) %>%
  filter(!is.na(religion), !is.na(girl)) %>%
  mutate(
    religion = ifelse(religion == "Muslim", "Muslim", "Nonmuslim"),
    religion = fct_relevel(religion, "Muslim"),
    girl = fct_relevel(girl, "Girl"),
    religion_girl = interaction(religion, girl),
    age = case_when(
      wave == 1~"14-15",
      wave == 4~"18-19",
      wave == 8~"24-25"
    )
  )


# Descriptives: Sample size and composition

cils_all %>% 
  filter(wave %in% c(1, 4, 8)) %>%
  left_join(merge_w9, by = c("youthid", "wave")) %>%
  filter(!is.na(religion), !is.na(girl)) %>%
  group_by(wave) %>%
  summarize(
    n = n(),
    n_mus = sum(religion == "Muslim"),
    n_chr = sum(religion == "Christian"),
    n_none = sum(religion == "No religion"),
    n_oth = sum(religion %in% c("Buddhist", "Hindu", "Jew", "Other religion", "Sikh"))
    )

cils_all %>% 
  filter(wave %in% c(1, 4, 8)) %>%
  left_join(merge_w9, by = c("youthid", "wave")) %>%
  filter(!is.na(religion), !is.na(girl)) %>%
  filter(religion != "Muslim") %>%
  summarize(
    sh_mus = mean(religion == "Muslim"),
    sh_chr = mean(religion == "Christian"),
    sh_none = mean(religion == "No religion"),
    sh_oth = mean(religion %in% c("Buddhist", "Hindu", "Jew", "Other religion", "Sikh"))
    )




### Function to generate the main plot, dependent on sample definition

generate_plot <- function(prep_data = prep_data, sample = "balanced", names = names) {

### Different types of samples: 
# balanced: sample available in all waves (1, 4, 8)
# totwgts: weighting with totwgts (wave 1 weight in all waves)
# calwgt8: wave 8 weighted with wave 6 refreshment sample weight, wave 1 and 4 weighted with totwgts (wave 1 weight)  -- this is default for main visualization

# balanced sample (weighted with totwgts, necessarily available because everyone in the balanced sample participated in W1)

if (sample == "balanced") {

prep_data <- prep_data %>% 
  group_by(youthid) %>%
  mutate(n = n()) %>%
  filter(n == 3) %>%
  ungroup() %>%
  mutate(wgt = totwgts)

 } 

# weighted with totwgts

if (sample == "totwgts") {

prep_data <- prep_data %>% 
  mutate(wgt = totwgts) %>%
  filter(!is.na(wgt))

}

# weighted with calwgt in wave 8

if (sample == "calwgt8") {

prep_data <- prep_data %>% 
  mutate(wgt = ifelse(wave == 8, calwgt, totwgts)) %>%
  filter(!is.na(wgt))

}


### Calculcate group-specific means

means <- prep_data %>%
  pivot_longer(
    cols = c("lta_rel", "lta_cin", "lta_club", "lta_out", "lta_read", "lta_mus", "lta_news", "lta_tv", "lta_chat", "lta_house", "lta_compa", "lta_compo", "lta_sport")
  ) %>%
  group_by(name, age, religion, girl) %>%
  nest() %>%
    mutate(
    model = map(data, ~ lm_robust(value ~ 1, weights = wgt, data = .x)),
    tidy_res = map(model, tidy, conf.level = .95),
  ) %>%
  unnest(tidy_res) %>%
  ungroup() %>%
  mutate(
    name_formatted = mapvalues(name, names %>% pull(names_orig), names %>% pull(names_form)),
    name_type = mapvalues(name, names %>% pull(names_orig), names %>% pull(type)),
    name_order = mapvalues(name, names %>% pull(names_orig), names %>% pull(names_order)) %>% as.numeric(), 
    name_formatted = fct_reorder(name_formatted, name_order),
    image = ifelse(religion == "Muslim", paste0(here("leisure-time-activities-replication"), "/icons/mus_sym_only.png"), NA)
  ) 


### Calculate contrasts between groups

contrasts <- prep_data %>%
  pivot_longer(
    cols = c("lta_rel", "lta_cin", "lta_club", "lta_out", "lta_read", "lta_mus", "lta_news", "lta_tv", "lta_chat", "lta_house", "lta_compa", "lta_compo", "lta_sport")
    ) %>%
  group_by(name, age) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm_robust(value ~ religion * girl, weights = wgt, data = .x)), # Apply regression to each nested dataset
    contrast_res = map(model, test_predictions, terms = c("girl", "religion")),
  ) %>%
  unnest(contrast_res) %>%
  mutate(
    girl_ego = str_extract(girl, "^[^-]+"),
    girl_alter = str_extract(girl, "(?<=-).*"),
    religion_ego = str_extract(religion, "^[^-]+"),
    religion_alter = str_extract(religion, "(?<=-).*"),
    ) %>%
  ungroup()


### Create lines for network graphs

contrasts_lines <- contrasts %>%
  filter(p.value < .05) %>%
  mutate(
    girl_ego = fct_relevel(girl_ego, "Girl"),
    girl_alter = fct_relevel(girl_alter, "Girl"),
    x = case_when(
      religion_ego == "Muslim" & girl_ego == "Girl"~1,
      religion_ego == "Muslim" & girl_ego == "Boy"~1,
      religion_ego == "Nonmuslim" & girl_ego == "Girl"~.7,
      religion_ego == "Nonmuslim" & girl_ego == "Boy"~1.3
      ),
    y = case_when(
      religion_ego == "Muslim" & girl_ego == "Girl"~.7,
      religion_ego == "Muslim" & girl_ego == "Boy"~1.3,
      religion_ego == "Nonmuslim" & girl_ego == "Girl"~1,
      religion_ego == "Nonmuslim" & girl_ego == "Boy"~1,
      ),
    xend = case_when(
      religion_alter == "Muslim" & girl_alter == "Girl"~1,
      religion_alter == "Muslim" & girl_alter == "Boy"~1,
      religion_alter == "Nonmuslim" & girl_alter == "Girl"~.7,
      religion_alter == "Nonmuslim" & girl_alter == "Boy"~1.3,
      ),
    yend = case_when(
      religion_alter == "Muslim" & girl_alter == "Girl"~.7,
      religion_alter == "Muslim" & girl_alter == "Boy"~1.3,
      religion_alter == "Nonmuslim" & girl_alter == "Girl"~1,
      religion_alter == "Nonmuslim" & girl_alter == "Boy"~1,
      ),
    name_formatted = mapvalues(name, names %>% pull(names_orig), names %>% pull(names_form)),
    name_type = mapvalues(name, names %>% pull(names_orig), names %>% pull(type)),
    name_order = mapvalues(name, names %>% pull(names_orig), names %>% pull(names_order)) %>% as.numeric(), 
    name_formatted = fct_reorder(name_formatted, name_order),
    y = y - 1.5, yend = yend - 1.5,
     x = case_when(
      age == "14-15"~x,
      age == "18-19"~x+1,
      age == "24-25"~x+2,
      ),
    xend = case_when(
      age == "14-15"~xend,
      age == "18-19"~xend+1,
      age == "24-25"~xend+2,
      ),
    Contrast = abs(Contrast),
    min_Contrast = min(Contrast),
    max_Contrast = max(Contrast),
    Contrast = (Contrast - min_Contrast) / (max_Contrast - min_Contrast),
    Contrast = .2 + 1.3 * Contrast
    )


### Create points for network graphs

contrasts_points <- tribble(
  ~x, ~y, ~image, ~girl,
  .7, 1, NA, "girl",
  1, .7, paste0(here("leisure-time-activities-replication"  ), "/icons/mus_sym_only.png"), "girl",
  1, 1.3, paste0(here("leisure-time-activities-replication" ), "/icons/mus_sym_only.png"), "boy",
  1.3, 1, NA, "boy",
  1, .5, NA, NA,
  ) %>% mutate(y = y - 1.5)


contrasts_points <- means %>% distinct(name_type, name_formatted, age) %>%
  crossing(contrasts_points) %>%
  mutate(
     x = case_when(
      age == "14-15"~x,
      age == "18-19"~x+1,
      age == "24-25"~x+2,
      )
     ) %>%
  mutate(girl = ifelse(girl == "girl", "Girl", "Boy"))



### Subgraph for out-of-home activities

name_type_activities <- "Out-of-home activities"

p1 <- means %>%
    filter(name_type == name_type_activities) %>%
    mutate(age = ifelse(name == "lta_sport" & age == "24-25", "26-27", age)) %>%
    ggplot(aes(x = age, y = estimate, group = girl, fill = girl)) +
    geom_path(aes(x = age, y = estimate, group = interaction(religion, girl))) +
    geom_point(aes(x = age, y = estimate, group = interaction(religion, girl)), pch = 21, color = "black", fill = "white", size = 4, data = means %>% filter(name_type == name_type_activities) %>% mutate(girl = "None") %>% mutate(age = ifelse(name == "lta_sport" & age == "24-25", "26-27", age))) +
    geom_point(pch = 21, size = 4, alpha = .6) +
    geom_image(aes(image = image), size = .04) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, linewidth = Contrast), data = contrasts_lines %>% filter(name_type == name_type_activities) %>% mutate(age = ifelse(name == "lta_sport" & age == "24-25", "26-27", age))) +
    geom_point(aes(x = x, y = y), pch = 21, color = "black", fill = "white", size = 3, data = contrasts_points %>% filter(name_type == name_type_activities) %>% mutate(girl = "None") %>% mutate(age = ifelse(name_formatted == "Do sport" & age == "24-25", "26-27", age))) +
    geom_point(aes(x = x, y = y), pch = 21, size = 3, alpha = .6, data = contrasts_points %>% filter(name_type == name_type_activities) %>% mutate(age = ifelse(name_formatted == "Do sport" & age == "24-25", "26-27", age))) +
    geom_image(aes(x = x, y = y, image = image), data = contrasts_points %>% filter(name_type == name_type_activities) %>% mutate(age = ifelse(name_formatted == "Do sport" & age == "24-25", "26-27", age)), size = .03) +
    geom_hline(aes(yintercept = 0)) +
    facet_nested(~name_formatted, scales = "free") +
    scale_linewidth_identity() +
    scale_fill_manual(values = scales::alpha(c("Girl" = "red",  "Boy" = "blue", "None" = "white"), 1)) +
    scale_y_continuous(
      breaks = c(-.5, 0, 1, 2, 3),
      limits = c(-0.85, 3.2),
      labels = c(
        "\n\ndifference\nsignificant\nat p < .05",
        "never",
        "less\noften",
        "≥ once\na month",
        "≥ once\na week"
      )
    ) +
    labs(
      x = "Age",
      y = "",
      title = "A: Out-of-home activities"
      ) +
    theme_classic() +
    theme(
      legend.position = "off",
    ) 

p1


### Subgraph for activities at home

name_type_activities <- "Activities at home"


### Annotations to explain scales

add_scale_info <- tribble(
  ~name_formatted, ~x, ~y, ~annotation, ~girl,
  "Read newspaper", "24-25", 3.1, "left scale", "girl",
  "Play computer (alone)", "14-15", 3.1, "right scale", "girl"
  ) %>%
  mutate(
    name_order = mapvalues(name_formatted, names %>% pull(names_form), names %>% pull(names_order)) %>% as.numeric(), 
    name_formatted = fct_reorder(name_formatted, name_order),
  )


add_scale_info_arrow <- tribble(
  ~name_formatted, ~x, ~y, ~xend, ~yend, ~girl,
  "Read newspaper", Inf, 2.95, "18-19", 2.95, "girl",
  "Play computer (alone)", -Inf, 2.95, "18-19", 2.95, "girl"
  ) %>%
  mutate(
    name_order = mapvalues(name_formatted, names %>% pull(names_form), names %>% pull(names_order)) %>% as.numeric(), 
    name_formatted = fct_reorder(name_formatted, name_order),
  )



p2 <- means %>%
    filter(name_type == name_type_activities) %>%
    ggplot(aes(x = age, y = estimate, group = girl, fill = girl)) +
    geom_text(aes(x = x, y = y, label = annotation), size = 2.5, data = add_scale_info) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(type = "closed", length = unit(0.1, "cm")), linewidth = .3, data = add_scale_info_arrow) +
    geom_path(aes(x = age, y = estimate, group = interaction(religion, girl))) +
    geom_point(aes(x = age, y = estimate, group = interaction(religion, girl)), pch = 21, color = "black", fill = "white", size = 4, data = means %>% filter(name_type == name_type_activities) %>% mutate(girl = "None")) +
    geom_point(pch = 21, size = 4, alpha = .6) +
    geom_image(aes(image = image), size = .04) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, linewidth = Contrast), data = contrasts_lines %>% filter(name_type == name_type_activities)) +
    geom_point(aes(x = x, y = y), pch = 21, color = "black", fill = "white", size = 3, data = contrasts_points %>% filter(name_type == name_type_activities) %>% mutate(girl = "None")) +
    geom_point(aes(x = x, y = y), pch = 21, size = 3, alpha = .6, data = contrasts_points %>% filter(name_type == name_type_activities)) +
    geom_image(aes(x = x, y = y, image = image), data = contrasts_points %>% filter(name_type == name_type_activities), size = .03) +
    geom_hline(aes(yintercept = 0)) +
    facet_nested(~name_formatted, scales = "free") +
    scale_linewidth_identity() +
    scale_fill_manual(values = scales::alpha(c("Girl" = "red",  "Boy" = "blue", "None" = "white"), 1)) +
    scale_y_continuous(
      breaks = c(-.5, 0, 1, 2, 3),
      limits = c(-0.85, 3.2),
      labels = c(
        "\n\ndifference\nsignificant\nat p < .05",
        "never",
        "less\noften",
        "≥ once\na month",
        "≥ once\na week"
      ),
     sec.axis = dup_axis(
      breaks = c(-0.5, 0, 1, 2, 3),
      labels = c(
        "\n\ndifference\nsignificant\nat p < .05",
        "no time",
        "< 1 hour\n/day",
        "1 hour\n/day",
        "2 hours\n/day"
      ))
    ) +
    labs(
      x = "Age",
      y = "",
      title = "B: Activities at home"
      ) +
    theme_classic() +
    theme(
      legend.position = "off",
    ) 

### Generate complete plot

plot <- (p1 + plot_spacer() + plot_layout(widths = c(6, 1))) / p2

return(list(plot, means))

}

### Main visualization

plot <- generate_plot(prep_data = prep_data, sample = "calwgt8", names = names)
plot[[1]] #+ labs(caption = "Note: Panel data from wave 1 (ages 14-15), wave 4 (ages 18-19), and wave 8 (ages 24-25) from CILS4EU and CILS4EU-DE datasets. Wave 1 and 4 weighted with wave 1 weights; wave 8 weighted\nwith wave 6 longitudinal and refreshment sample weights. Data on doing sports from wave 9 (age 26-27).") + theme(plot.caption = element_text(hjust = 0))
ggsave("01_leisure_time_main.png", width = 12.5, height = 6.5)


plot[[2]] %>%
  select(name_formatted, name_order, age, girl, religion, estimate, std.error) %>%
  mutate(
    estimate = prettyNum(round(estimate, 2), nsmall = 2),
    std.error = paste0("(", prettyNum(round(std.error, 2), nsmall = 2), ")")
    ) %>%
  pivot_wider(
    names_from = c("girl", "religion"),
    values_from = c("estimate", "std.error")
    ) %>%
  arrange(name_order, age) %>%
  select(-name_order) %>%
  mutate(name_formatted = as.character(name_formatted)) %>%
  select(name_formatted, age, estimate_Girl_Muslim, std.error_Girl_Muslim, estimate_Boy_Muslim, std.error_Boy_Muslim, estimate_Girl_Nonmuslim, std.error_Girl_Nonmuslim, estimate_Boy_Nonmuslim, std.error_Boy_Nonmuslim) %>%
  stargazer::stargazer(type = "html", summary = FALSE, out = "01_leisure_time_main_table.html")




### Figure S1: Only sample with totwgts (dropping refreshment sample)

polot <- generate_plot(prep_data = prep_data, sample = "totwgts", names = names)
plot[[1]] + labs(caption = "Note: Panel data from wave 1 (ages 14-15), wave 4 (ages 18-19), and wave 8 (ages 24-25) from CILS4EU and CILS4EU-DE datasets. Wave 1, 4, and 8 weighted with wave 1 weights; refreshment\nsample dropped. Data on doing sports from wave 9 (age 26-27).") + theme(plot.caption = element_text(hjust = 0))
ggsave("02_leisure_time_totwgts.png", width = 12.5, height = 6.5)


### Figure S2: Only balanced panel sample

plot <- generate_plot(prep_data = prep_data, sample = "balanced", names = names)
plot[[1]] + labs(caption = "Note: Panel data from wave 1 (ages 14-15), wave 4 (ages 18-19), and wave 8 (ages 24-25) from CILS4EU and CILS4EU-DE datasets. Wave 1, 4, and 8 weighted with wave 1 weights; balanced\nsample only. Data on doing sports from wave 9 (age 26-27).") + theme(plot.caption = element_text(hjust = 0))
ggsave("03_leisure_time_balanced.png", width = 12.5, height = 6.5)



### Figure S4: Non-Muslims: only Christian and no religion

prep_data_noother <- cils_all %>% 
  filter(wave %in% c(1, 4, 8)) %>%
  left_join(merge_w9, by = c("youthid", "wave")) %>%
  mutate(
    lta_sport = ifelse(wave == 8, lta_sport9, lta_sport),
    lta_house = ifelse(wave == 8, 5 - as.numeric(lta20), lta_house)
  ) %>%
  filter(!is.na(religion), !is.na(girl)) %>%
  filter(religion %in% c("Muslim", "Christian", "No religion")) %>%
  mutate(
    religion = ifelse(religion == "Muslim", "Muslim", "Nonmuslim"),
    religion = fct_relevel(religion, "Muslim"),
    girl = fct_relevel(girl, "Girl"),
    religion_girl = interaction(religion, girl),
    age = case_when(
      wave == 1~"14-15",
      wave == 4~"18-19",
      wave == 8~"24-25"
    )
  )

plot <- generate_plot(prep_data = prep_data_noother, sample = "calwgt8", names = names)
plot[[1]] + labs(caption = "Note: Panel data from wave 1 (ages 14-15), wave 4 (ages 18-19), and wave 8 (ages 24-25) from CILS4EU and CILS4EU-DE datasets. Wave 1 and 4 weighted with wave 1 weights; wave 8 weighted\nwith wave 6 longitudinal and refreshment sample weights. Data on doing sports from wave 9 (age 26-27).") + theme(plot.caption = element_text(hjust = 0))
ggsave("05_leisure_time_noother.png", width = 12.5, height = 6.5)



### Figure S3: Contrasting Christian with no religion

prep_data_chr <- cils_all %>% 
  filter(wave %in% c(1, 4, 8)) %>%
  left_join(merge_w9, by = c("youthid", "wave")) %>%
  mutate(
    lta_sport = ifelse(wave == 8, lta_sport9, lta_sport),
    lta_house = ifelse(wave == 8, 5 - as.numeric(lta20), lta_house)
  ) %>%
  filter(!is.na(religion), !is.na(girl)) %>%
  filter(religion %in% c("Christian", "No religion")) %>%
  mutate(
    religion = ifelse(religion == "Christian", "Christian", "Noreligion"),
    religion = fct_relevel(religion, "Christian"),
    girl = fct_relevel(girl, "Girl"),
    religion_girl = interaction(religion, girl),
    age = case_when(
      wave == 1~"14-15",
      wave == 4~"18-19",
      wave == 8~"24-25"
    )
  )


generate_plot_chr <- function(prep_data = prep_data, sample = "balanced", names = names) {

### Different types of samples: balanced, all with totwgts, wave 8 weighted with calwgt

# balanced sample (weighted with totwgts, necessarily available because everyone in the balanced sample participated in W1)

if (sample == "balanced") {

prep_data <- prep_data %>% 
  group_by(youthid) %>%
  mutate(n = n()) %>%
  filter(n == 3) %>%
  ungroup() %>%
  mutate(wgt = totwgts)

 } 

# weighted with totwgts

if (sample == "totwgts") {

prep_data <- prep_data %>% 
  mutate(wgt = totwgts) %>%
  filter(!is.na(wgt))

}

### weighted with calwgt in wave 8

if (sample == "calwgt8") {

prep_data <- prep_data %>% 
  mutate(wgt = ifelse(wave == 8, calwgt, totwgts)) %>%
  filter(!is.na(wgt))

}


means <- prep_data %>%
  pivot_longer(
    cols = c("lta_rel", "lta_cin", "lta_club", "lta_out", "lta_read", "lta_mus", "lta_news", "lta_tv", "lta_chat", "lta_house", "lta_compa", "lta_compo", "lta_sport")
  ) %>%
  group_by(name, age, religion, girl) %>%
  nest() %>%
    mutate(
    model = map(data, ~ lm_robust(value ~ 1, weights = wgt, data = .x)),
    tidy_res = map(model, tidy, conf.level = .95),
  ) %>%
  unnest(tidy_res) %>%
  ungroup() %>%
  mutate(
    name_formatted = mapvalues(name, names %>% pull(names_orig), names %>% pull(names_form)),
    name_type = mapvalues(name, names %>% pull(names_orig), names %>% pull(type)),
    name_order = mapvalues(name, names %>% pull(names_orig), names %>% pull(names_order)) %>% as.numeric(), 
    name_formatted = fct_reorder(name_formatted, name_order),
    image = ifelse(religion == "Christian", paste0(here("leisure-time-activities-replication"), "/icons/chr_sym_only.png"), NA)
  ) 


contrasts <- prep_data %>%
  pivot_longer(
    cols = c("lta_rel", "lta_cin", "lta_club", "lta_out", "lta_read", "lta_mus", "lta_news", "lta_tv", "lta_chat", "lta_house", "lta_compa", "lta_compo", "lta_sport")
    ) %>%
  group_by(name, age) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm_robust(value ~ religion * girl, weights = wgt, data = .x)), # Apply regression to each nested dataset
    contrast_res = map(model, test_predictions, terms = c("girl", "religion")),
  ) %>%
  unnest(contrast_res) %>%
  mutate(
    girl_ego = str_extract(girl, "^[^-]+"),
    girl_alter = str_extract(girl, "(?<=-).*"),
    religion_ego = str_extract(religion, "^[^-]+"),
    religion_alter = str_extract(religion, "(?<=-).*"),
    ) %>%
  ungroup()



contrasts_lines <- contrasts %>%
  filter(p.value < .05) %>%
  mutate(
    girl_ego = fct_relevel(girl_ego, "Girl"),
    girl_alter = fct_relevel(girl_alter, "Girl"),
    x = case_when(
      religion_ego == "Christian" & girl_ego == "Girl"~1,
      religion_ego == "Christian" & girl_ego == "Boy"~1,
      religion_ego == "Noreligion" & girl_ego == "Girl"~.7,
      religion_ego == "Noreligion" & girl_ego == "Boy"~1.3
      ),
    y = case_when(
      religion_ego == "Christian" & girl_ego == "Girl"~.7,
      religion_ego == "Christian" & girl_ego == "Boy"~1.3,
      religion_ego == "Noreligion" & girl_ego == "Girl"~1,
      religion_ego == "Noreligion" & girl_ego == "Boy"~1,
      ),
    xend = case_when(
      religion_alter == "Christian" & girl_alter == "Girl"~1,
      religion_alter == "Christian" & girl_alter == "Boy"~1,
      religion_alter == "Noreligion" & girl_alter == "Girl"~.7,
      religion_alter == "Noreligion" & girl_alter == "Boy"~1.3,
      ),
    yend = case_when(
      religion_alter == "Christian" & girl_alter == "Girl"~.7,
      religion_alter == "Christian" & girl_alter == "Boy"~1.3,
      religion_alter == "Noreligion" & girl_alter == "Girl"~1,
      religion_alter == "Noreligion" & girl_alter == "Boy"~1,
      ),
    name_formatted = mapvalues(name, names %>% pull(names_orig), names %>% pull(names_form)),
    name_type = mapvalues(name, names %>% pull(names_orig), names %>% pull(type)),
    name_order = mapvalues(name, names %>% pull(names_orig), names %>% pull(names_order)) %>% as.numeric(), 
    name_formatted = fct_reorder(name_formatted, name_order),
    y = y - 1.5, yend = yend - 1.5,
     x = case_when(
      age == "14-15"~x,
      age == "18-19"~x+1,
      age == "24-25"~x+2,
      ),
    xend = case_when(
      age == "14-15"~xend,
      age == "18-19"~xend+1,
      age == "24-25"~xend+2,
      ),
    Contrast = abs(Contrast),
    min_Contrast = min(Contrast),
    max_Contrast = max(Contrast),
    Contrast = (Contrast - min_Contrast) / (max_Contrast - min_Contrast),
    Contrast = .2 + 1.3 * Contrast
    )

contrasts_points <- tribble(
  ~x, ~y, ~image, ~girl,
  .7, 1, NA, "girl",
  1, .7, paste0(here("leisure-time-activities-replication"), "/icons/chr_sym_only.png"), "girl",
  1, 1.3, paste0(here("leisure-time-activities-replication"), "/icons/chr_sym_only.png"), "boy",
  1.3, 1, NA, "boy",
  1, .5, NA, NA,
  ) %>% mutate(y = y - 1.5)


contrasts_points <- means %>% distinct(name_type, name_formatted, age) %>%
  crossing(contrasts_points) %>%
  mutate(
     x = case_when(
      age == "14-15"~x,
      age == "18-19"~x+1,
      age == "24-25"~x+2,
      )
     ) %>%
  mutate(girl = ifelse(girl == "girl", "Girl", "Boy"))


### Subgraph for out-of-home activities

name_type_activities <- "Out-of-home activities"


p1 <- means %>%
    filter(name_type == name_type_activities) %>%
    mutate(age = ifelse(name == "lta_sport" & age == "24-25", "26-27", age)) %>%
    ggplot(aes(x = age, y = estimate, group = girl, fill = girl)) +
    geom_path(aes(x = age, y = estimate, group = interaction(religion, girl))) +
    geom_point(aes(x = age, y = estimate, group = interaction(religion, girl)), pch = 21, color = "black", fill = "white", size = 4, data = means %>% filter(name_type == name_type_activities) %>% mutate(girl = "None") %>% mutate(age = ifelse(name == "lta_sport" & age == "24-25", "26-27", age))) +
    geom_point(pch = 21, size = 4, alpha = .6) +
    geom_image(aes(image = image), size = .12) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, linewidth = Contrast), data = contrasts_lines %>% filter(name_type == name_type_activities) %>% mutate(age = ifelse(name == "lta_sport" & age == "24-25", "26-27", age))) +
    geom_point(aes(x = x, y = y), pch = 21, color = "black", fill = "white", size = 3, data = contrasts_points %>% filter(name_type == name_type_activities) %>% mutate(girl = "None") %>% mutate(age = ifelse(name_formatted == "Do sport" & age == "24-25", "26-27", age))) +
    geom_point(aes(x = x, y = y), pch = 21, size = 3, alpha = .6, data = contrasts_points %>% filter(name_type == name_type_activities) %>% mutate(age = ifelse(name_formatted == "Do sport" & age == "24-25", "26-27", age))) +
    geom_image(aes(x = x, y = y, image = image), data = contrasts_points %>% filter(name_type == name_type_activities) %>% mutate(age = ifelse(name_formatted == "Do sport" & age == "24-25", "26-27", age)), size = .09) +
    geom_hline(aes(yintercept = 0)) +
    facet_nested(~name_formatted, scales = "free") +
    scale_linewidth_identity() +
    scale_fill_manual(values = scales::alpha(c("Girl" = "red",  "Boy" = "blue", "None" = "white"), 1)) +
    scale_y_continuous(
      breaks = c(-.5, 0, 1, 2, 3),
      limits = c(-0.85, 3.2),
      labels = c(
        "\n\ndifference\nsignificant\nat p < .05",
        "never",
        "less\noften",
        "≥ once\na month",
        "≥ once\na week"
      )
    ) +
    labs(
      x = "Age",
      y = "",
      title = "A: Out-of-home activities"
      ) +
    theme_classic() +
    theme(
      legend.position = "off",
    ) 

### Subgraph for activities at home

name_type_activities <- "Activities at home"

### Annotations to explain scales

add_scale_info <- tribble(
  ~name_formatted, ~x, ~y, ~annotation, ~girl,
  "Read newspaper", "24-25", 3.1, "left scale", "girl",
  "Play computer (alone)", "14-15", 3.1, "right scale", "girl"
  ) %>%
  mutate(
    name_order = mapvalues(name_formatted, names %>% pull(names_form), names %>% pull(names_order)) %>% as.numeric(), 
    name_formatted = fct_reorder(name_formatted, name_order),
  )


add_scale_info_arrow <- tribble(
  ~name_formatted, ~x, ~y, ~xend, ~yend, ~girl,
  "Read newspaper", Inf, 2.95, "18-19", 2.95, "girl",
  "Play computer (alone)", -Inf, 2.95, "18-19", 2.95, "girl"
  ) %>%
  mutate(
    name_order = mapvalues(name_formatted, names %>% pull(names_form), names %>% pull(names_order)) %>% as.numeric(), 
    name_formatted = fct_reorder(name_formatted, name_order),
  )



p2 <- means %>%
    filter(name_type == name_type_activities) %>%
    ggplot(aes(x = age, y = estimate, group = girl, fill = girl)) +
    geom_text(aes(x = x, y = y, label = annotation), size = 2.5, data = add_scale_info) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(type = "closed", length = unit(0.1, "cm")), linewidth = .3, data = add_scale_info_arrow) +
    geom_path(aes(x = age, y = estimate, group = interaction(religion, girl))) +
    geom_point(aes(x = age, y = estimate, group = interaction(religion, girl)), pch = 21, color = "black", fill = "white", size = 4, data = means %>% filter(name_type == name_type_activities) %>% mutate(girl = "None")) +
    geom_point(pch = 21, size = 4, alpha = .6) +
    geom_image(aes(image = image), size = .12) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, linewidth = Contrast), data = contrasts_lines %>% filter(name_type == name_type_activities)) +
    geom_point(aes(x = x, y = y), pch = 21, color = "black", fill = "white", size = 3, data = contrasts_points %>% filter(name_type == name_type_activities) %>% mutate(girl = "None")) +
    geom_point(aes(x = x, y = y), pch = 21, size = 3, alpha = .6, data = contrasts_points %>% filter(name_type == name_type_activities)) +
    geom_image(aes(x = x, y = y, image = image), data = contrasts_points %>% filter(name_type == name_type_activities), size = .09) +
    geom_hline(aes(yintercept = 0)) +
    facet_nested(~name_formatted, scales = "free") +
    scale_linewidth_identity() +
    scale_fill_manual(values = scales::alpha(c("Girl" = "red",  "Boy" = "blue", "None" = "white"), 1)) +
    scale_y_continuous(
      breaks = c(-.5, 0, 1, 2, 3),
      limits = c(-0.85, 3.2),
      labels = c(
        "\n\ndifference\nsignificant\nat p < .05",
        "never",
        "less\noften",
        "≥ once\na month",
        "≥ once\na week"
      ),
     sec.axis = dup_axis(
      breaks = c(-0.5, 0, 1, 2, 3),
      labels = c(
        "\n\ndifference\nsignificant\nat p < .05",
        "no time",
        "< 1 hour\n/day",
        "1 hour\n/day",
        "2 hours\n/day"
      ))
    ) +
    labs(
      x = "Age",
      y = "",
      title = "B: Activities at home"
      ) +
    theme_classic() +
    theme(
      legend.position = "off",
    ) 


plot <- (p1 + plot_spacer() + plot_layout(widths = c(6, 1))) / p2

return(list(plot, means))

}


plot <- generate_plot_chr(prep_data = prep_data_chr, sample = "calwgt8", names = names)
plot[[1]]  + labs(caption = "Note: Panel data from wave 1 (ages 14-15), wave 4 (ages 18-19), and wave 8 (ages 24-25) from CILS4EU and CILS4EU-DE datasets. Wave 1 and 4 weighted with wave 1 weights; wave 8 weighted\nwith wave 6 longitudinal and refreshment sample weights. Data on doing sports from wave 9 (age 26-27).") + theme(plot.caption = element_text(hjust = 0))
ggsave("04_leisure_time_christ.png", width = 12.5, height = 6.5)



### Visualization of ordered data ###

generate_plot_prop <- function(prep_data = prep_data, sample = "balanced", names = names, value_list = c(0, 1)) {

### Different types of samples: balanced, all with totwgts, wave 8 weighted with calwgt

# balanced sample (weighted with totwgts, necessarily available because everyone in the balanced sample participated in W1)

if (sample == "balanced") {

prep_data <- prep_data %>% 
  group_by(youthid) %>%
  mutate(n = n()) %>%
  filter(n == 3) %>%
  ungroup() %>%
  mutate(wgt = totwgts)

 } 

# weighted with totwgts

if (sample == "totwgts") {

prep_data <- prep_data %>% 
  mutate(wgt = totwgts) %>%
  filter(!is.na(wgt))

}

# weighted with calwgt in wave 8

if (sample == "calwgt8") {

prep_data <- prep_data %>% 
  mutate(wgt = ifelse(wave == 8, calwgt, totwgts)) %>%
  filter(!is.na(wgt))

}



means <- prep_data %>%
  pivot_longer(
    cols = c("lta_rel", "lta_cin", "lta_club", "lta_out", "lta_read", "lta_mus", "lta_news", "lta_tv", "lta_chat", "lta_house", "lta_compa", "lta_compo", "lta_sport")
  ) %>%
  mutate(value = ifelse(value %in% value_list, 0, 1)) %>%
  group_by(name, age, religion, girl) %>%
  nest() %>%
    mutate(
    model = map(data, ~ lm_robust(value ~ 1, weights = wgt, data = .x)),
    tidy_res = map(model, tidy, conf.level = .95),
  ) %>%
  unnest(tidy_res) %>%
  ungroup() %>%
  mutate(
    name_formatted = mapvalues(name, names %>% pull(names_orig), names %>% pull(names_form)),
    name_type = mapvalues(name, names %>% pull(names_orig), names %>% pull(type)),
    name_order = mapvalues(name, names %>% pull(names_orig), names %>% pull(names_order)) %>% as.numeric(), 
    name_formatted = fct_reorder(name_formatted, name_order),
    image = ifelse(religion == "Muslim", paste0(here(), "/icons/mus_sym_only.png"), NA)
  ) 


### Calculate contrasts between groups

contrasts <- prep_data %>%
  pivot_longer(
    cols = c("lta_rel", "lta_cin", "lta_club", "lta_out", "lta_read", "lta_mus", "lta_news", "lta_tv", "lta_chat", "lta_house", "lta_compa", "lta_compo", "lta_sport")
    ) %>%
  mutate(value = ifelse(value %in% value_list, 0, 1)) %>%
  group_by(name, age) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm_robust(value ~ religion * girl, weights = wgt, data = .x)), # Apply regression to each nested dataset
    contrast_res = map(model, test_predictions, terms = c("girl", "religion")),
  ) %>%
  unnest(contrast_res) %>%
  mutate(
    girl_ego = str_extract(girl, "^[^-]+"),
    girl_alter = str_extract(girl, "(?<=-).*"),
    religion_ego = str_extract(religion, "^[^-]+"),
    religion_alter = str_extract(religion, "(?<=-).*"),
    ) %>%
  ungroup()


### Create lines for network graphs

contrasts_lines <- contrasts %>%
  filter(p.value < .05) %>%
  mutate(
    girl_ego = fct_relevel(girl_ego, "Girl"),
    girl_alter = fct_relevel(girl_alter, "Girl"),
    x = case_when(
      religion_ego == "Muslim" & girl_ego == "Girl"~1,
      religion_ego == "Muslim" & girl_ego == "Boy"~1,
      religion_ego == "Nonmuslim" & girl_ego == "Girl"~.75,
      religion_ego == "Nonmuslim" & girl_ego == "Boy"~1.25
      ),
    y = case_when(
      religion_ego == "Muslim" & girl_ego == "Girl"~.7,
      religion_ego == "Muslim" & girl_ego == "Boy"~1.3,
      religion_ego == "Nonmuslim" & girl_ego == "Girl"~1,
      religion_ego == "Nonmuslim" & girl_ego == "Boy"~1,
      ),
    xend = case_when(
      religion_alter == "Muslim" & girl_alter == "Girl"~1,
      religion_alter == "Muslim" & girl_alter == "Boy"~1,
      religion_alter == "Nonmuslim" & girl_alter == "Girl"~.75,
      religion_alter == "Nonmuslim" & girl_alter == "Boy"~1.25,
      ),
    yend = case_when(
      religion_alter == "Muslim" & girl_alter == "Girl"~.7,
      religion_alter == "Muslim" & girl_alter == "Boy"~1.3,
      religion_alter == "Nonmuslim" & girl_alter == "Girl"~1,
      religion_alter == "Nonmuslim" & girl_alter == "Boy"~1,
      ),
    name_formatted = mapvalues(name, names %>% pull(names_orig), names %>% pull(names_form)),
    name_type = mapvalues(name, names %>% pull(names_orig), names %>% pull(type)),
    name_order = mapvalues(name, names %>% pull(names_orig), names %>% pull(names_order)) %>% as.numeric(), 
    name_formatted = fct_reorder(name_formatted, name_order),
    y = y - 1.5, yend = yend - 1.5,
     x = case_when(
      age == "14-15"~x,
      age == "18-19"~x+1,
      age == "24-25"~x+2,
      ),
    xend = case_when(
      age == "14-15"~xend,
      age == "18-19"~xend+1,
      age == "24-25"~xend+2,
      ),
    Contrast = abs(Contrast),
    min_Contrast = min(Contrast),
    max_Contrast = max(Contrast),
    Contrast = (Contrast - min_Contrast) / (max_Contrast - min_Contrast),
    Contrast = .2 + 1.3 * Contrast
    )



contrasts_lines <- contrasts_lines %>% 
  mutate(
    y = .5 * y, yend = .5 * yend,
    y = case_when(y >=-.1~-.15, y == -.4~-.35, TRUE~y),
    yend = case_when(yend >= -.12~-.15, yend == -.4~-.35, TRUE~yend)
    )



### Create points for network graphs

contrasts_points <- tribble(
  ~x, ~y, ~image, ~girl,
  .7, 1, NA, "girl",
  1, .7, paste0(here(), "/icons/mus_sym_only.png"), "girl",
  1, 1.3, paste0(here(), "/icons/mus_sym_only.png"), "boy",
  1.3, 1, NA, "boy",
  #1, .5, NA, NA,
  ) %>% mutate(y = y - 1.5)



contrasts_points <- contrasts_points %>% mutate(
  x = case_when(x == .7~.75 , x == 1.3~1.25, TRUE~x),
  y = .5 * y,
  y = case_when(y >= -.12~-.15, y == -.4~-.35, TRUE~y)
  )



contrasts_points <- means %>% distinct(name_type, name_formatted, age) %>%
  crossing(contrasts_points) %>%
  mutate(
     x = case_when(
      age == "14-15"~x,
      age == "18-19"~x+1,
      age == "24-25"~x+2,
      )
     ) %>%
  mutate(girl = ifelse(girl == "girl", "Girl", "Boy"))



### Subgraph for out-of-home activities

name_type_activities <- "Out-of-home activities"

label <- case_when(length(value_list) == 3~"A: Proportion who do the activity at least weekly", length(value_list) == 2~"B: Proportion who do the activity at least monthly", length(value_list) == 1~"C: Proportion who do the activity at least sometimes (not never)")

p1 <- means %>%
    filter(name_type == name_type_activities | name_formatted == "Read a book" | name_formatted == "Read newspaper") %>%
    mutate(age = ifelse(name == "lta_sport" & age == "24-25", "26-27", age)) %>%
    ggplot(aes(x = age, y = estimate, group = girl, fill = girl)) +
    geom_path(aes(x = age, y = estimate, group = interaction(religion, girl))) +
    geom_point(aes(x = age, y = estimate, group = interaction(religion, girl)), pch = 21, color = "black", fill = "white", size = 4, data = means %>% filter(name_type == name_type_activities | name_formatted == "Read a book" | name_formatted == "Read newspaper") %>% mutate(girl = "None") %>% mutate(age = ifelse(name == "lta_sport" & age == "24-25", "26-27", age))) +
    geom_point(pch = 21, size = 4, alpha = .6) +
    geom_image(aes(image = image), size = .04) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, linewidth = Contrast), data = contrasts_lines %>% filter(name_type == name_type_activities | name_formatted == "Read a book" | name_formatted == "Read newspaper") %>% mutate(age = ifelse(name == "lta_sport" & age == "24-25", "26-27", age))) +
    geom_point(aes(x = x, y = y), pch = 21, color = "black", fill = "white", size = 3, data = contrasts_points %>% filter(name_type == name_type_activities | name_formatted == "Read a book" | name_formatted == "Read newspaper") %>% mutate(girl = "None") %>% mutate(age = ifelse(name_formatted == "Do sport" & age == "24-25", "26-27", age))) +
    geom_point(aes(x = x, y = y), pch = 21, size = 3, alpha = .6, data = contrasts_points %>% filter(name_type == name_type_activities | name_formatted == "Read a book" | name_formatted == "Read newspaper") %>% mutate(age = ifelse(name_formatted == "Do sport" & age == "24-25", "26-27", age))) +
    geom_image(aes(x = x, y = y, image = image), data = contrasts_points %>% filter(name_type == name_type_activities | name_formatted == "Read a book" | name_formatted == "Read newspaper") %>% mutate(age = ifelse(name_formatted == "Do sport" & age == "24-25", "26-27", age)), size = .03) +
    geom_hline(aes(yintercept = 0)) +
    facet_nested(~name_formatted, scales = "free") +
    scale_linewidth_identity() +
    scale_fill_manual(values = scales::alpha(c("Girl" = "red",  "Boy" = "blue", "None" = "white"), 1)) +
    scale_y_continuous(
      breaks = c(-.25, 0, .25, .5, .75, 1),
      limits = c(-0.45, 1.1),
      labels = c(
        "\ndifference\nsignificant\nat p < .05",
        "0%",
        "25%",
        "50%",
        "75%",
        "100%"
      )
    ) +
    labs(
      x = "Age",
      y = "",
      title = label
      ) +
    theme_classic() +
    theme(
      legend.position = "off",
    ) 

return(p1)

}


plot_c <- generate_plot_prop(prep_data = prep_data, sample = "calwgt8", names = names, value_list = c(0))
plot_c <- plot_c + labs(caption = "\n\n\n\n\n\n\nNote: Panel data from wave 1 (ages 14-15), wave 4 (ages 18-19), and wave 8 (ages 24-25) from CILS4EU and CILS4EU-DE datasets. Wave 1 and 4 weighted with wave 1 weights; wave 8weighted with wave 6 longitudinal and refreshment\nsample weights. Data on doing sports from wave 9 (age 26-27).") + theme(plot.caption = element_text(hjust = 0))


plot_b <- generate_plot_prop(prep_data = prep_data, sample = "calwgt8", names = names, value_list = c(0, 1))

plot_a <- generate_plot_prop(prep_data = prep_data, sample = "calwgt8", names = names, value_list = c(0, 1, 2))



plot_a / plot_b / plot_c  + plot_layout(heights = c(1, 1, 1.24))

ggsave("06_leisure_time_main_props.png", width = 14, height = 10.5)





