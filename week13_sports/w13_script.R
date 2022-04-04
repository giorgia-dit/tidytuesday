# LOADING PACKAGES #

library(tidyverse) 
library(RColorBrewer)
library(tidytuesdayR)
library(sysfonts)
library(showtext)
library(ggsci)
library(grid)
library(gridExtra)
library(ggnewscale)
library(colorspace)
library(emoGG)

# LOADING UTILITIES #

showtext_opts(dpi = 300)
showtext_auto()
font_add_google("Roboto", "roboto")
font_add_google("Work Sans", "work")

# LOADING DATA #

tuesdata <- tidytuesdayR::tt_load('2022-03-29')
sports <- tuesdata$sports
str(sports)

# CLEANING AND MANIPULATION #

sports_df <- sports %>%
  mutate_if(is.character, as.factor) %>%
  replace_na(list(partic_men = 0, partic_women = 0,
                  partic_coed_men = 0, partic_coed_women = 0)) %>%
  rename(sportsname = sports)

# EXPLORATION AND VISUALIZATION #

# 1. How is the participation splitted among boys and girls in the different sports?

sports_sex <- sports_df %>%
  group_by(sportscode, sportsname) %>%
  summarise(female_partic = sum(sum_partic_women),
            male_partic = sum(sum_partic_men),
            tot_partic = sum(female_partic, male_partic)) %>%
  filter(tot_partic != 0) %>%
  mutate(female_partic_prop = female_partic / tot_partic,
         male_partic_prop = male_partic / tot_partic) %>%
  gather(gender, partic_prop, c(female_partic_prop, male_partic_prop), factor_key = T) %>%
  mutate(gender = fct_recode(gender, Female = "female_partic_prop", Male = "male_partic_prop"))

sex_plot <- ggplot(sports_sex,
       aes(x = fct_reorder(sportsname, male_partic / tot_partic), y = partic_prop, fill = gender)) +
  geom_col(width = 0.8, alpha = 0.8, position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#FF7043", "#26A69A")) +
  coord_flip() +
  labs(y = "Participation rate", x = "Sport", fill = "Gender",
       title = "Collegiate sports: gender distribution",
       caption = "#TidyTuesday Week 13 - 2022 | Data: Equity in Athletics Data Analysis | Chart: Giorgia Ditano") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = 0.2),
        panel.background = element_blank(),
        panel.ontop = T,
        plot.margin = unit(c(2, 2, 2, 2), "cm"), 
        plot.background = element_rect(fill = "#f7f7f7"),
        plot.title = element_text(family = "work", size = 16, hjust = 0.5,
                                  margin = unit(c(25, 0, 20, 0), "pt")),
        #plot.subtitle = element_text(family = "work", size = 13, hjust = 0.5,
                                     #margin = unit(c(0, 0, 15, 0), "pt")),
        plot.caption = element_text(family = "work", size = 8, hjust = 1,
                                    margin = unit(c(20, 0, 0, 0), "pt")),
        legend.background = element_rect(fill = "#f7f7f7"),
        legend.title = element_text(family = "roboto", face = "italic", size = 9,
                                    margin = unit(c(1, 0, 4, 0), "pt")),
        legend.text = element_text(face = "italic", hjust = 1),
        legend.box.background = element_rect(colour = "black", linetype = 2),
        axis.text.x = element_text(family = "roboto"),
        axis.text.y = element_text(family = "work", size = 10, face = "italic", 
                                   color = "black", margin = unit(c(0,-20,0,0), "pt")),
        axis.title = element_text(family = "work", size = 12),
        axis.title.x = element_text(margin = unit(c(10,0,20,0), "pt")),
        axis.title.y = element_text(margin = unit(c(0,10,0,10), "pt")),
        axis.ticks.y = element_blank()
  )

ggsave(sex_plot, filename = "w13_plot1.png", width = 15, height = 9.375)


 # 2. How do expenditure and revenue per capita vary among sports and gender? 

sports_exprev <- sports_df %>%
  select(sportscode:sportsname) %>%
  mutate(sum_partic_menwomen = sum_partic_men + sum_partic_women) %>%
  mutate(exp_pc_men = exp_men / sum_partic_men,
         exp_pc_women = exp_women / sum_partic_women,
         exp_pc_menwomen = total_exp_menwomen / sum_partic_menwomen,
         rev_pc_men = rev_men / sum_partic_men,
         rev_pc_women = rev_women / sum_partic_women,
         rev_pc_menwomen = total_rev_menwomen / sum_partic_menwomen) %>%
  group_by(sportscode, sportsname) %>%
  summarise(across(partic_men:sum_partic_menwomen, sum, na.rm = T),
            across(exp_pc_men:rev_pc_menwomen, mean, na.rm = T, .names = "avg_{.col}")) %>%
  rename_with(stringr::str_replace_all,
              pattern = c("_men" = ".men", "_women" = ".women", "total_" = "")) %>%
  pivot_longer(cols = partic.men:avg_rev_pc.menwomen,
               names_to = c("value_type", "gender"),
               names_sep = "\\.",
               values_to = "value") %>% 
  pivot_wider(names_from = "value_type", values_from = "value") %>%
  mutate(across(starts_with("avg"), ~ na_if(., "NaN"))) %>%
  mutate(gender = ifelse(gender == "menwomen", "total", gender))


exp_plot <- ggplot(sports_exprev %>% 
         filter(gender != "total") %>%
         mutate(avg_exp_pc = ifelse(gender == "men", -avg_exp_pc, avg_exp_pc),
                avg_rev_pc = ifelse(gender == "men", -avg_rev_pc, avg_rev_pc)) %>%
         group_by(sportsname) %>%
         mutate(diff_avg_exp_pc = sum(avg_exp_pc),
                diff_avg_rev_pc = sum(avg_rev_pc)) %>%
         filter(!is.na(diff_avg_exp_pc) & !is.na(diff_avg_rev_pc))) +
  geom_col(aes(x = fct_reorder(sportsname, diff_avg_exp_pc), 
               y = avg_exp_pc, fill = gender), width = 0.5, alpha = 0.8) +
  scale_y_continuous(breaks = c(-50000, -25000, 0, 25000, 50000), 
                     labels = scales::dollar(abs(c(-50000, -25000, 0, 25000, 50000))),
                     limits = c(-60000, 60000)) +
  scale_fill_manual(values = c("#26A69A", "#FF7043"), labels = c("Male", "Female"),
                    name = "", guide = guide_legend(order = 1)) +
  new_scale_fill() +
  geom_col(data = . %>% filter(gender == "women"),
           aes(x = fct_reorder(sportsname, diff_avg_exp_pc), y = diff_avg_exp_pc,
               fill = gender), width = 0.5) +
  scale_fill_manual(values = "#757575", labels = "Expense difference between\nfemales and males", 
                    name = "", guide = guide_legend(order = 2)) +
  labs(y = "Average expense per capita", x = "Sport",
       title = "", subtitle = "", 
       caption = "#TidyTuesday Week 13 - 2022 | Data: Equity in Athletics Data Analysis | Chart: Giorgia Ditano") +
  coord_flip() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = 0.2),
        panel.grid.major.y = element_line(size = 0.2, color = "#E0E0E0"),
        #panel.background = element_blank(),
        #panel.ontop = T,
        plot.margin = unit(c(2, 1, 2, 1), "cm"), 
        plot.background = element_rect(fill = "#f7f7f7"),
        plot.title = element_text(family = "work", size = 16, hjust = 0.5,
                                  margin = unit(c(25, 0, 5, 0), "pt")),
        plot.subtitle = element_text(family = "work", size = 13, hjust = 0.5,
                                     margin = unit(c(0, 0, 15, 0), "pt")),
        plot.caption = element_text(family = "work", size = 8, hjust = 1,
        margin = unit(c(20, 0, 0, 0), "pt")),
        legend.background = element_rect(fill = "#f7f7f7"),
        legend.text = element_text(face = "italic", hjust = 1),
        legend.box.background = element_rect(colour = "black", linetype = 2),
        axis.text.x = element_text(family = "roboto"),
        axis.text.y = element_text(family = "work", size = 10, face = "italic", 
                                   color = "black", margin = unit(c(0,0,0,0), "pt")),
        axis.title = element_text(family = "work", size = 12),
        axis.title.x = element_text(margin = unit(c(10,0,20,0), "pt")),
        axis.title.y = element_text(margin = unit(c(0,10,0,10), "pt")),
        axis.ticks.y = element_blank()
  )


rev_plot <- ggplot(sports_exprev %>% 
         filter(gender != "total") %>%
         mutate(avg_exp_pc = ifelse(gender == "men", -avg_exp_pc, avg_exp_pc),
                avg_rev_pc = ifelse(gender == "men", -avg_rev_pc, avg_rev_pc)) %>%
         group_by(sportsname) %>%
         mutate(diff_avg_exp_pc = sum(avg_exp_pc),
                diff_avg_rev_pc = sum(avg_rev_pc)) %>%
         filter(!is.na(diff_avg_exp_pc) & !is.na(diff_avg_rev_pc))) +
  geom_col(aes(x = fct_reorder(sportsname, diff_avg_rev_pc), 
               y = avg_rev_pc, fill = gender), width = 0.5, alpha = 0.8) +
  scale_y_continuous(breaks = c(-50000, -25000, 0, 25000, 50000), 
                     labels = scales::dollar(abs(c(-50000, -25000, 0, 25000, 50000))),
                     limits = c(-60000, 60000)) +
  scale_fill_manual(values = c("#26A69A", "#FF7043"), labels = c("Male", "Female"),
                    name = "", guide = guide_legend(order = 1)) +
  new_scale_fill() +
  geom_col(data = . %>% filter(gender == "women"),
           aes(x = fct_reorder(sportsname, diff_avg_rev_pc), y = diff_avg_rev_pc,
               fill = gender), width = 0.5) +
  scale_fill_manual(values = "#757575", labels = "Revenue difference between\nfemales and males", 
                    name = "", guide = guide_legend(order = 2)) +
  labs(y = "Average revenue per capita", x = "Sport",
       title = "", subtitle = "", 
       caption = "#TidyTuesday Week 13 - 2022 | Data: Equity in Athletics Data Analysis | Chart: Giorgia Ditano") +
  coord_flip() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = 0.2),
        panel.grid.major.y = element_line(size = 0.2, color = "#E0E0E0"),
        #panel.background = element_blank(),
        #panel.ontop = T,
        plot.margin = unit(c(2, 1, 2, 1), "cm"), 
        plot.background = element_rect(fill = "#f7f7f7"),
        plot.title = element_text(family = "work", size = 16, hjust = 0.5,
                                  margin = unit(c(25, 0, 5, 0), "pt")),
        plot.subtitle = element_text(family = "work", size = 13, hjust = 0.5,
                                     margin = unit(c(0, 0, 15, 0), "pt")),
        plot.caption = element_text(family = "work", size = 8, hjust = 1,
                                    margin = unit(c(20, 0, 0, 0), "pt")),
        legend.background = element_rect(fill = "#f7f7f7"),
        legend.text = element_text(face = "italic", hjust = 1),
        legend.box.background = element_rect(colour = "black", linetype = 2),
        axis.text.x = element_text(family = "roboto"),
        axis.text.y = element_text(family = "work", size = 10, face = "italic", 
                                   color = "black", margin = unit(c(0,0,0,0), "pt")),
        axis.title = element_text(family = "work", size = 12),
        axis.title.x = element_text(margin = unit(c(10,0,20,0), "pt")),
        axis.title.y = element_text(margin = unit(c(0,10,0,10), "pt")),
        axis.ticks.y = element_blank()
  )


# 3. College spending on top-5 practiced sports vs. other sports
    
sports_exptime <- sports_df %>%
  select(year, state_cd, sportscode:sportsname) %>%
  mutate(sum_partic_menwomen = sum_partic_men + sum_partic_women,
         sum_partic_menwomen = ifelse(is.na(sum_partic_menwomen), 0, sum_partic_menwomen)) %>%
  mutate(sportsname = fct_lump(sportsname, 5, w = sum_partic_menwomen,
                               other_level = "Other")) %>%
  mutate(exp_pc_men = exp_men / sum_partic_men,
         exp_pc_women = exp_women / sum_partic_women,
         exp_pc_menwomen = total_exp_menwomen / sum_partic_menwomen) %>%
  group_by(year, state_cd, sportsname) %>%
  summarise(across(partic_men:sum_partic_menwomen, sum, na.rm = T),
            across(exp_pc_men:exp_pc_menwomen, mean, na.rm = T, .names = "avg_{.col}")) %>%
  rename_with(stringr::str_replace_all,
              pattern = c("_men" = ".men", "_women" = ".women", "total_" = "")) %>%
  pivot_longer(cols = partic.men:avg_exp_pc.menwomen,
               names_to = c("value_type", "gender"),
               names_sep = "\\.",
               values_to = "value") %>% 
  pivot_wider(names_from = "value_type", values_from = "value") %>%
  mutate(across(starts_with("avg"), ~ na_if(., "NaN"))) %>%
  mutate(gender = ifelse(gender == "menwomen", "total", gender),
         gender = fct_recode(gender, Males = "men", Females = "women", Total = "total"),
         gender = fct_relevel(gender, c("Males", "Females", "Total")))

top_plot <- ggplot(sports_exptime) +
  geom_col(aes(x = year, y = avg_exp_pc, 
               fill = fct_relevel(sportsname, c("Football", "Basketball", "Baseball", "Soccer",
                                                "All Track Combined", "Other"))), 
           position = "fill", width = 0.9, alpha = 0.75) +
  scale_fill_manual(values = c("#D84315", "#FB8C00", "#FFCA28", "#7CB342", "#26A69A", "#006064"),
                    name = "Sport") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ gender) +
  labs(x = "Year", y = "Expense per capita",
       title = "College spending on top-5 practiced sports vs. other sports",
       caption = "#TidyTuesday Week 13 - 2022 | Data: Equity in Athletics Data Analysis | Chart: Giorgia Ditano") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.background = element_blank(),
        panel.ontop = T,
        plot.margin = unit(c(2, 2, 2, 2), "cm"), 
        plot.background = element_rect(fill = "#f7f7f7"),
        plot.title = element_text(family = "work", size = 16, hjust = 0.5,
                                  margin = unit(c(25, 0, 20, 0), "pt")),
        #plot.subtitle = element_text(family = "work", size = 13, hjust = 0.5,
                                     #margin = unit(c(0, 0, 15, 0), "pt")),
        plot.caption = element_text(family = "work", size = 8, hjust = 1,
                                    margin = unit(c(20, 0, 0, 0), "pt")),
        legend.title = element_text(family = "work", size = 11, 
                                    margin = unit(c(5,0,10,0), "pt")),
        legend.background = element_rect(fill = "#f7f7f7"),
        legend.text = element_text(face = "italic", hjust = 1, size = 10),
        legend.box.background = element_rect(colour = "black", linetype = 2),
        axis.text.x = element_text(family = "roboto"),
        axis.text.y = element_text(family = "roboto", size = 9, 
                                   color = "black"),
        axis.title = element_text(family = "work", size = 12),
        axis.title.x = element_text(margin = unit(c(10,0,10,0), "pt")),
        axis.title.y = element_text(margin = unit(c(0,10,0,10), "pt")),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(fill = "#CFD8DC"),
        strip.text = element_text(family = "work", face = "italic",
                                  size = 12, margin = unit(c(7,2,7,2), "pt"))
  )

ggsave(top_plot, filename = "w13_plot2.png", width = 15, height = 9.375)
         
















