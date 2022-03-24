# LOADING PACKAGES #

install.packages("tidyverse")
install.packages("tidytuesdayR")
install.packages("RColorBrewer")
install.packages("countrycode")
install.packages("sysfonts")
install.packages("showtext")
install.packages(c("ggspatial", "sf", "rnaturalearth", "rnaturalearthdata"))
install.packages("ggsci")
devtools::install_github("dill/emoGG")

library(tidyverse) 
library(RColorBrewer)
library(tidytuesdayR)
library(countrycode)
library(sysfonts)
library(showtext)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggsci)
library(emoGG)


# LOADING DATA #

tuesdata <- tidytuesdayR::tt_load('2022-03-08')
erasmus <- tuesdata$erasmus


# LOADING UTILITIES #
showtext_opts(dpi = 300)
showtext_auto()
font_add_google("Roboto", "roboto")
font_add_google("Work Sans", "work")


# CLEANING #

str(erasmus)

erasmus_df <- erasmus %>%
  # year_month (chr) to date
  mutate(mobility_start_month = str_c(mobility_start_month, "01", sep = "-")) %>%
  mutate(mobility_end_month = str_c(mobility_end_month, "01", sep = "-")) %>%
  mutate(mobility_start_date = as.Date(mobility_start_month, "%Y-%m-%d")) %>%
  mutate(mobility_end_date = as.Date(mobility_end_month, "%Y-%m-%d")) %>%
  # character vars to factor
  mutate_if(is.character, as.factor)

str(erasmus_df) 
# TODO: 
# - re-compute mobility duration in months, removing the existing one
#   (odd: most of the people have 1-day mobility)
# - clean participant age
# - remove participant_profile (all learners)
# - remove group_leader (all NO)
# - remove sending and receiving organization names and codes (not needed)
# - remove education level, field of education (no values)
# - remove cities 
# - create (sending/receiving) country names
levels(erasmus_df$participant_nationality) # TODO: "-" to NA

erasmus_df <- erasmus_df %>%
  mutate(mobility_months = round(x = ((mobility_end_date - mobility_start_date + 30)), 0)) %>%
  mutate(mobility_months = as.numeric(round(mobility_months / 30, 0))) %>%
  select(-mobility_duration, -participant_profile, -group_leader, -education_level, -field_of_education,
         -sending_organization, -sending_organisation_erasmus_code, -receiving_organization, 
         -receiving_organisation_erasmus_code, -sending_city, -receiving_city) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(participant_age = replace(participant_age, (participant_age < 10 | participant_age > 30), NA)) %>%
  mutate(participant_nationality = factor(participant_nationality, exclude = "-")) %>%
  mutate(sending_country_name = countrycode(sending_country_code, origin = "iso2c", destination = "country.name")) %>%
  mutate(receiving_country_name = countrycode(receiving_country_code, origin = "iso2c", destination = "country.name"))
  
# some values not matched unambiguously (EL, UK): correction
erasmus_df <- erasmus_df %>%
  mutate(sending_country_name = replace(sending_country_name, sending_country_code == "EL", "Greece")) %>% 
  mutate(receiving_country_name = replace(receiving_country_name, receiving_country_code == "EL", "Greece")) %>% 
  mutate(sending_country_name = replace(sending_country_name, sending_country_code == "UK", "United Kingdom")) %>% 
  mutate(receiving_country_name = replace(receiving_country_name, receiving_country_code == "UK", "United Kingdom"))


# EXPLORING AND VISUALIZING #

# Which destinations are the most popular for Erasmus students?

erasmus_top_dest <- erasmus_df %>%
  select(academic_year, receiving_country_name) %>%
  add_count(academic_year, name = "tot_students_year") %>%
  add_count(academic_year, receiving_country_name, name = "tot_students_year_dest") %>%
  mutate(perc_students_year_dest = tot_students_year_dest / tot_students_year) %>%
  distinct(key = paste0(academic_year, receiving_country_name), .keep_all=T) %>%
  select(-key) %>%
  bind_rows(group_by(., receiving_country_name) %>%
              summarise(tot_students_year_dest = sum(tot_students_year_dest)) %>%
              ungroup() %>%
              mutate(academic_year = "Total", 
                     tot_students_year = sum(tot_students_year_dest),
                     perc_students_year_dest = tot_students_year_dest / tot_students_year))

dest_plot <- ggplot(erasmus_top_dest,
  aes(x = academic_year, y = fct_reorder(receiving_country_name, perc_students_year_dest), 
                                         fill = perc_students_year_dest)) +
  geom_tile(alpha = 0.95, width = .5, height = .9, colour = "Black") +
  labs(x = "Academic year", y = "Receiving country", 
       fill = "Percent of Erasmus\nstudents received\n(per year)",
       title = "Most popular destination for Erasmus students") +
  scale_fill_distiller(type = "seq", palette = "Oranges", direction = 1, trans = "sqrt", 
                       labels = scales::percent) +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(),
        plot.margin = unit(c(1, 8, 1, 8), "cm"), 
        plot.background = element_rect(fill = "#f7f7f7"),
        plot.title = element_text(family = "work", size = 16, hjust = 0.5,
                                  margin = unit(c(25, 0, 15, 0), "pt")),
        legend.background = element_rect(fill = "#f7f7f7"),
        legend.title = element_text(family = "roboto", face = "italic", size = 9,
                                    margin = unit(c(1, 0, 4, 0), "pt")),
        legend.text = element_text(face = "italic", hjust = 1),
        legend.box.background = element_rect(colour = "black", linetype = 2),
        axis.text = element_text(family = "roboto"),
        axis.title = element_text(family = "work", size = 12),
        axis.title.x = element_text(margin = unit(c(10,0,20,0), "pt")))
ggsave(dest_plot, filename = "1_pop_dest.png", width = 15, height = 9.375)


# Which countries are the main exporters of Erasmus students?

erasmus_top_orig <- erasmus_df %>%
  select(academic_year, sending_country_name, sending_country_code) %>%
  add_count(academic_year, name = "tot_students_year") %>%
  add_count(academic_year, sending_country_name, name = "tot_students_year_orig") %>%
  mutate(perc_students_year_orig = tot_students_year_orig / tot_students_year) %>%
  distinct(key = paste0(academic_year, sending_country_name), .keep_all=T) %>%
  select(-key) %>%
  bind_rows(group_by(., sending_country_name, sending_country_code) %>%
              summarise(tot_students_year_orig = sum(tot_students_year_orig)) %>%
              ungroup() %>%
              mutate(academic_year = "Total", 
                     tot_students_year = sum(tot_students_year_orig),
                     perc_students_year_orig = tot_students_year_orig / tot_students_year))


world <- ne_countries(scale = "medium", returnclass = "sf")

europe_map_top_orig <- world %>%
  filter(region_wb == "Europe & Central Asia") %>%
  left_join(erasmus_top_orig %>% filter(academic_year == "Total"), 
            by = c("iso_a2" = "sending_country_code"))


orig_plot <- ggplot(europe_map_top_orig) + 
  geom_sf(aes(fill = perc_students_year_orig), alpha = 0.95) +
  scale_fill_material("teal", labels = scales::percent) +
  coord_sf(xlim = c(-30, 90), ylim = c(25, 80)) +
  labs(x = "Latitude", y = "Longitude",
       title = "Main European exporters of Erasmus students",
       subtitle = "Academic years: 2014/2015 to 2019/2020",
       fill = "Percent of Erasmus\nstudents exported") +
  geom_rect(xmin = 70, xmax = 93, ymin = 66, ymax = 81, linetype = 2,
            fill = "#f7f7f7", colour = "black", size = 0.2) +
  annotate("text", x = 71.5, y = 79.5, family = "work", size = 4.3,
           hjust = 0, vjust = 1, label = "Top 5 exporters") +
  annotate("text", x = 76.6, y = 77.1, size = 4,
           hjust = 0, vjust = 1, label = "Germany\nPoland\nSpain\nFrance\nHungary\n") +
  add_emoji("1f1e9-1f1ea", x= 73.3, y = 76.6, ysize = 3) +
  add_emoji("1f1f5-1f1f1", x= 73.3, y = 74.63, ysize = 3) +
  add_emoji("1f1ea-1f1f8", x= 73.3, y = 72.60, ysize = 3) +
  add_emoji("1f1eb-1f1f7", x= 73.3, y = 70.55, ysize = 3) +
  add_emoji("1f1ed-1f1fa", x= 73.3, y = 68.50, ysize = 3) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(fill = "#f7f7f7"),
        plot.title = element_text(family = "work", size = 16, hjust = 0.5,
                                  margin = unit(c(25, 0, 5, 0), "pt")),
        plot.subtitle = element_text(family = "work", size = 12, hjust = 0.5,
                                  margin = unit(c(5, 0, 15, 0), "pt")),
        legend.background = element_rect(fill = "#f7f7f7"),
        legend.title = element_text(family = "roboto", face = "italic", size = 9,
                                    margin = unit(c(1, 0, 4, 0), "pt")),
        legend.text = element_text(face = "italic", hjust = 1),
        legend.box.background = element_rect(colour = "black", linetype = 2),
        axis.text = element_text(family = "roboto"),
        axis.title = element_text(family = "work", size = 12),
        axis.title.x = element_text(margin = unit(c(10,0,20,0), "pt")),
        axis.title.y = element_text(margin = unit(c(0,10,0,20), "pt"))) 
ggsave(orig_plot, filename = "2_pop_orig.png", width = 15, height = 9.375)








