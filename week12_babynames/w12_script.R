# LOADING PACKAGES #

library(tidyverse) 
library(RColorBrewer)
library(tidytuesdayR)
library(sysfonts)
library(showtext)
library(ggsci)
library(gganimate)
library(animation)
library(grid)
library(gifski)

# LOADING UTILITIES #

showtext_opts(dpi = 300)
showtext_auto()
font_add_google("Roboto", "roboto")
font_add_google("Work Sans", "work")

# LOADING DATA #

install.packages("babynames")
library(babynames)

applicants <- babynames::applicants
births <- babynames::births
lifetables <- babynames::lifetables
babynames <- babynames::babynames

# CLEANING AND MANIPULATION #

str(babynames) 

babynames <- babynames %>%
  mutate(sex = factor(sex, levels = c("M", "F"), labels = c("Male", "Female"))) %>%
  mutate(name = factor(name))


# VISUALIZATION #

# 1. Top 15, by sex over years

# get the top 15 names per sex, over all times
names_top15 <- babynames %>%
  group_by(sex, name) %>%
  summarise(total_n = sum(n)) %>%
  top_n(15, total_n) %>%
  select(-total_n)

babynames_top15 <- babynames %>%
  # new column: percent of specific sex total birth [per year, per name]
  group_by(year, sex) %>%
  mutate(total_sex_year = sum(n),) %>%
  ungroup() %>%
  mutate(prop_sex_year = n / total_sex_year) %>%
  select(-total_sex_year) %>%
  # filtering for the top 15 names
  right_join(names_top15, by = c("sex","name"))


rm(names_top15)

yticks_year <- babynames_top15$year[babynames_top15$year %% 10 == 0] %>% unique()
ytext_year <-  as.character(babynames_top15$year[babynames_top15$year %% 10 == 0] %>% unique())

names_pal <- c(brewer.pal(12, "Set3"), brewer.pal(3, "Set2"))

# Female top15 plot (static)

female_grob <- rectGrob(x = 0, y = seq(0, 1, length.out = 15), width = unit(2.1, "cm"), height = unit(0.6, "cm"),
                        gp = gpar(fill = names_pal, alpha = 0.6, lty = "dotted"))

female_static_plot <- ggplot(babynames_top15 %>% filter(sex == "Female"),
                             aes(x = fct_rev(fct_inseq(name)), y = year, fill = prop_sex_year)) +
  geom_tile(alpha = 0.95, width = .5, height = 1) +
  scale_y_continuous(breaks = yticks_year, labels = ytext_year) +
  scale_fill_material(palette = "pink", labels = scales::percent) +
  labs(title = "What's her name?",
       subtitle = "Most popular female baby names in the US over time", 
       x = "Name", y = "Year",
       fill = "Percent of female\nbirths (per year)",
       caption = "#TidyTuesday Week 12 - 2022 | Data: Hadley Wickham | Chart: Giorgia Ditano") +
  coord_flip(clip = 'off') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = 0.2),
        panel.background = element_blank(),
        panel.ontop = T,
        plot.margin = unit(c(1, 8, 1, 8), "cm"), 
        plot.background = element_rect(fill = "#f7f7f7"),
        plot.title = element_text(family = "work", size = 16, hjust = 0.5,
                                  margin = unit(c(25, 0, 5, 0), "pt")),
        plot.subtitle = element_text(family = "work", size = 13, hjust = 0.5,
                                     margin = unit(c(0, 0, 15, 0), "pt")),
        plot.caption = element_text(family = "work", size = 8, hjust = 1,
                                    margin = unit(c(20, 0, 0, 0), "pt")),
        legend.background = element_rect(fill = "#f7f7f7"),
        legend.title = element_text(family = "roboto", face = "italic", size = 9,
                                    margin = unit(c(1, 0, 4, 0), "pt")),
        legend.text = element_text(face = "italic", hjust = 1),
        legend.box.background = element_rect(colour = "black", linetype = 2),
        axis.text.x = element_text(family = "roboto"),
        axis.text.y = element_text(family = "work", size = 11, face = "italic", 
                                   color = "black", hjust = 0.5),
        axis.title = element_text(family = "work", size = 12),
        axis.title.x = element_text(margin = unit(c(10,0,20,0), "pt")),
        axis.title.y = element_text(margin = unit(c(0,25,0,10), "pt")),
        axis.ticks.y = element_blank()
  ) +
  annotation_custom(
    grob = female_grob, xmin = 1, xmax = 15, ymin = 1862.2, ymax = 1862.2)

ggsave(female_static_plot, filename = "female_st_plot.png", width = 15, height = 9.375)

# Male top15 plot (static)

male_grob <- rectGrob(x = 0, y = seq(0, 1, length.out = 15), width = unit(2.4, "cm"), height = unit(0.6, "cm"),
                      gp = gpar(fill = names_pal, alpha = 0.6, lty = "dotted"))


male_static_plot <- ggplot(babynames_top15 %>% filter(sex == "Male"),
                           aes(x = fct_rev(fct_inseq(name)), y = year, fill = prop_sex_year)) +
  geom_tile(alpha = 0.95, width = .5, height = 1) +
  scale_y_continuous(breaks = yticks_year, labels = ytext_year) +
  scale_fill_material(palette = "blue", labels = scales::percent) +
  labs(title = "What's his name?",
       subtitle = "Most popular male baby names in the US over time", 
       x = "Name", y = "Year",
       fill = "Percent of male\nbirths (per year)",
       caption = "#TidyTuesday Week 12 - 2022 | Data: Hadley Wickham | Chart: Giorgia Ditano") +
  coord_flip(clip = 'off') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = 0.2),
        panel.background = element_blank(),
        panel.ontop = T,
        plot.margin = unit(c(1, 8, 1, 8), "cm"), 
        plot.background = element_rect(fill = "#f7f7f7"),
        plot.title = element_text(family = "work", size = 16, hjust = 0.5,
                                  margin = unit(c(25, 0, 5, 0), "pt")),
        plot.subtitle = element_text(family = "work", size = 13, hjust = 0.5,
                                     margin = unit(c(0, 0, 15, 0), "pt")),
        plot.caption = element_text(family = "work", size = 8, hjust = 1,
                                    margin = unit(c(20, 0, 0, 0), "pt")),
        legend.background = element_rect(fill = "#f7f7f7"),
        legend.title = element_text(family = "roboto", face = "italic", size = 9,
                                    margin = unit(c(1, 0, 4, 0), "pt")),
        legend.text = element_text(face = "italic", hjust = 1),
        legend.box.background = element_rect(colour = "black", linetype = 2),
        axis.text.x = element_text(family = "roboto"),
        axis.text.y = element_text(family = "work", size = 11, face = "italic", 
                                   color = "black", hjust = 0.5),
        axis.title = element_text(family = "work", size = 12),
        axis.title.x = element_text(margin = unit(c(10,0,20,0), "pt")),
        axis.title.y = element_text(margin = unit(c(0,25,0,10), "pt")),
        axis.ticks.y = element_blank()
  ) +
  annotation_custom(
    grob = male_grob, xmin = 1, xmax = 15, ymin = 1860, ymax = 1860)

ggsave(male_static_plot, filename = "male_st_plot.png", width = 15, height = 9.375)



# 2. Top 15 by sex, over the years - Animation try

top15_names <- babynames %>%
  # new var: rank variable
  group_by(year, sex) %>%
  mutate(rank_year_sex = row_number(desc(n))) %>%
  filter(rank_year_sex <= 15) %>%
  # new var: percent of specific sex total birth [per year, per name]
  mutate(total_sex_year = sum(n)) %>%
  ungroup() %>%
  mutate(prop_sex_year = n / total_sex_year) %>%
  select(-total_sex_year)


static_plot <- ggplot(top15_names %>% filter(sex == "Female"),
         aes(x = rank_year_sex, fill = name, color = name)) +
  geom_col(aes(y = prop_sex_year), width = 0.8, alpha = 0.7) +
  geom_text(aes(y = 0, label = paste0(name, "   ")), vjust = 1, hjust = 1,
            size = 8, fontface = "bold", family = "mono") +
  # geom_text(aes(y = n,label = scales::comma(n, accuracy = 1), hjust=0)) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(plot.background = element_rect(fill = "#f7f7f7"),
        plot.margin = margin(4, 2, 2, 4, "cm"),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(family = "roboto", size = 15),
        legend.position="none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size=.2, color="white"),
        panel.grid.minor.x = element_line(size=.2, color="white"),
        panel.ontop = T,
        plot.title = element_text(family = "mono", size = 25, hjust = 0.5,
                                  margin = unit(c(25, 0, 5, 0), "pt")),
        plot.subtitle = element_text(family = "mono", size = 20, hjust = 0.5,
                                     margin = unit(c(0, 0, 15, 0), "pt"))
        )

anim <- static_plot +
  transition_states(year, transition_length = 4, state_length = 2) +
  view_static() +
  labs(x = "Percent of female babies born",
       y = "",
       title = "What's her name?",
       subtitle = 'Year: {closest_state}')
animate(anim, nframes = 138, detail = 5, fps = 5, width = 1440, height = 900,
        renderer = gifski_renderer("2022_03_22_tidy_tuesday.gif"))


# 3. Babies with common names

common_names <- babynames %>%
  # new var: rank variable
  group_by(year, sex) %>%
  mutate(common_name = row_number(desc(n)) <= 100) %>%
  # new vars: 
  # - percent of births with common names [per year]
  # - dummy, common name or uncommon
  count(common_name, wt = n) %>%
  mutate(total_sex_year = sum(n),
         prop_sex_year = n / total_sex_year) %>%
  ungroup() %>%
  mutate(common_name = factor(ifelse(common_name, "Top 100", "Other name"))) %>%
  select(-total_sex_year)

common_plot <- ggplot(common_names) +
  geom_area(aes(x = year, y = prop_sex_year, 
                fill = sex, alpha = fct_rev(common_name))) +
  scale_fill_manual(values = c("#1976D2", "#C2185B")) +
  scale_alpha_manual(values = c(0.8, 0.4)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ sex) +
  labs(x = "Year", y = "Percent of births",
       title = "Call me by your name (or another)",
       subtitle = "Proportion of babies with common (top 100) names, per year",
       alpha = "Name ranking",
       caption = "#TidyTuesday Week 12 - 2022 | Data: Hadley Wickham | Chart: Giorgia Ditano") +
  guides(fill = "none",
         alpha = guide_legend(label.vjust = 0.5)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 3, 1.3, 3), "cm"), 
    plot.background = element_rect(fill = "#f7f7f7"),
    plot.title = element_text(family = "work", size = 16, hjust = 0.5,
                              margin = unit(c(30, 0, 5, 0), "pt")),
    plot.subtitle = element_text(family = "work", size = 13, hjust = 0.5,
                                 margin = unit(c(0, 0, 25, 0), "pt")),
    plot.caption = element_text(family = "work", size = 8, hjust = 1,
                                margin = unit(c(20, 0, 0, 0), "pt")),
    legend.background = element_rect(fill = "#f7f7f7"),
    legend.title = element_text(family = "roboto", face = "italic", size = 10,
                                margin = unit(c(1, 10, 0, 0), "pt"), vjust = 0.55),
    legend.text = element_text(face = "italic", hjust = 0, size = 9,
                               margin = unit(c(2, 0, 2, 0), "pt")),
    legend.position = "bottom",
    legend.box.background = element_rect(colour = "black", linetype = 2),
    axis.text.x = element_text(family = "roboto", size = 10),
    axis.text.y = element_text(family = "roboto", size = 10),
    axis.title = element_text(family = "work", size = 12),
    axis.title.x = element_text(margin = unit(c(10,0,20,0), "pt")),
    axis.title.y = element_text(margin = unit(c(0,20,0,0), "pt")),
    strip.background = element_rect(),
    strip.text = element_text(family = "work", size = 12, 
                              margin = unit(c(10,0,10,0), "pt"))
  )
ggsave(common_plot, filename = "common_plot.png", width = 15, height = 9.375)
       
  













