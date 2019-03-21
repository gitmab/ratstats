# ----- Project overview ------------------------------------------------------------------------------

# Exploring 311 Daily Service Requests data from Somerville, MA, with particular focus on looking at
# rat-related calls over time
# https://data.somervillema.gov/311-Call-Center/311-Daily-Service-Requests/xs7t-pxkc
#
# Outputs: pdf plots (in pdf subfolder)
#
# Meredith Brown mbrown@alum.mit.edu
# Last updated 2019-03-21

# ----- Load libraries and set up ---------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(ggmap)

# google maps API setup: see instructions at https://github.com/dkahle/ggmap
#register_google(key = "redacted", write = TRUE) 

# directory for plot pdfs
pdf_dir = "/Users/brownm/Documents/ratmaps/pdf/"

# ----- Data imports ----------------------------------------------------------------------------------

# read in Somerville 311 data from web
alldata <- read_csv(url("https://data.somervillema.gov/api/views/xs7t-pxkc/rows.csv"))

# ----- (Data cleanup, merging, train/test splits, etc) -----------------------------------------------

# ----- Plotting rat (and other animal) calls by month ------------------------------------------------

# look at seasonal trends by aggregating by month-year (yyyy-mm), ward number, issue type 
# (for select issues)

monthdata <- alldata %>% 
  mutate(issue_type = gsub("Wild animal/rabies questions", "Wild animal/rabies inquiry", issue_type),
         issue_type = gsub("Injured animal", "Injured or dead animal", issue_type),
         issue_type = gsub("Dead animal", "Injured or dead animal", issue_type)) %>%
  mutate(ticket_created_date_time = mdy_hms(ticket_created_date_time, tz = "EST")) %>%
  separate(col = "ticket_created_date_time", 
           into = c("year_ticket_created", "month_ticket_created", "day_ticket_created",
                    "hour_ticket_created", "minute_ticket_created", "second_ticket_created")) %>%
  mutate(month_year = factor(paste(year_ticket_created, month_ticket_created, sep="-"))) %>%
  group_by(month_year, neighborhood_district, issue_type) %>%
  summarise(count=n()) %>%
  mutate(freq = count / sum(count)) %>%
  filter(issue_type == "Rats" | issue_type == "Wild animal/rabies inquiry" | 
           issue_type == "Injured or dead animal") %>%
  mutate(issue_type = factor(issue_type, levels = c("Rats", "Injured or dead animal", 
                                                    "Wild animal/rabies inquiry")))

monthplot <- ggplot(monthdata, aes(x = month_year, y = count, colour = issue_type, 
                                   group = issue_type)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "Month", y = "Number of issues reported", colour = "Issue type") +
  scale_x_discrete(breaks = unique(monthdata$month_year)[seq(1, length(unique(monthdata$month_year)), 
                                                             by = 3)]) +
  geom_point() + 
  geom_line() + 
  scale_colour_manual(values = c("darkgoldenrod1", "firebrick2", "slateblue1")) + 
  facet_wrap(~neighborhood_district, nrow = 4)

ggsave(monthplot, filename=paste0(pdf_dir, 'monthplot_20190318.pdf'), width = 6.69, height = 3.79, 
       units = "in", device=cairo_pdf)

# ----- Plotting rat (and other animal) calls by year -------------------------------------------------

# what is the total volume of 311 work orders & proportion of annual 311 work orders related to rats 
# for each ward?

# total volume

yeardata <- alldata %>% 
  mutate(issue_type = gsub("Wild animal/rabies questions", "Wild animal/rabies inquiry", issue_type),
         issue_type = gsub("Injured animal", "Injured or dead animal", issue_type),
         issue_type = gsub("Dead animal", "Injured or dead animal", issue_type)) %>%
  mutate(year_ticket_created = year(mdy_hms(ticket_created_date_time, tz = "EST"))) %>%
  mutate(neighborhood_district = ifelse(is.na(neighborhood_district), "NA", neighborhood_district)) %>%
  mutate(neighborhood_district = factor(neighborhood_district, levels = c("Ward 1", "Ward 2", "Ward 3",
                                                                          "Ward 4", "Ward 5", "Ward 6", 
                                                                          "Ward 7", "NA"))) %>%
  group_by(year_ticket_created, neighborhood_district) %>%
  summarise(count=n())
  
totalplot <- ggplot(yeardata, aes(x = year_ticket_created, y = count, colour = neighborhood_district, 
                                  group = neighborhood_district)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "Year", y = "Number of issues reported", colour = "Issue type") +
  geom_point() + 
  geom_line() + 
  scale_colour_manual(values = c("#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
                                   "#ffb90f", "#A65628", "#F781BF", "#E41A1C"))

ggsave(totalplot, filename=paste0(pdf_dir, 'total_calls_plot_20190318.pdf'), width = 6.69, 
       height = 3.79, units = "in", device=cairo_pdf)

# proportion of rat calls by ward: 2018-2019

ratyeardata <- alldata %>% 
  mutate(neighborhood_district = ifelse(is.na(neighborhood_district), "NA", neighborhood_district)) %>%
  mutate(neighborhood_district = factor(neighborhood_district, levels = c("Ward 1", "Ward 2", "Ward 3",
                                                                          "Ward 4", "Ward 5", "Ward 6", 
                                                                          "Ward 7", "NA"))) %>%
  mutate(year_ticket_created = year(mdy_hms(ticket_created_date_time, tz = "EST"))) %>%
  filter(year_ticket_created >= 2018) %>%
  group_by(neighborhood_district, issue_type) %>%
  summarise(count=n()) %>%
  mutate(freq = count / sum(count)) %>%
  filter(issue_type == "Rats")

ratyearplot <- ggplot(ratyeardata, aes(x = neighborhood_district, y = freq, 
                                       colour = neighborhood_district)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "Proportion of issues reported (2018-19)") +
  theme(legend.position = "none") +
  geom_bar(fill = "white", stat = "identity") + 
  scale_colour_manual(values=c("#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
                               "#ffb90f", "#A65628", "#F781BF", "#E41A1C"))

ggsave(ratyearplot, filename=paste0(pdf_dir, 'yearplot_20190318.pdf'), width = 6.69, height = 3.79, 
       units = "in", device=cairo_pdf)

# ----- Map out rat work orders -----------------------------------------------------------------------

# Plot by time (month-year) and ward 

ratmapdata <- alldata %>%
  mutate(neighborhood_district = ifelse(is.na(neighborhood_district), "NA", neighborhood_district)) %>%
  mutate(coords = sub("\\).*", "", sub(".*\\(", "", location))) %>%
  separate(coords, c('lat', 'lon'), sep = ", ", convert = TRUE) %>%
  filter(lat!=0 & lon!=0) %>%
  mutate(ticket_created_date_time = mdy_hms(ticket_created_date_time, tz = "EST")) %>%
  separate(col = "ticket_created_date_time", 
           into = c("year_ticket_created", "month_ticket_created", "day_ticket_created",
                    "hour_ticket_created", "minute_ticket_created", "second_ticket_created")) %>%
  mutate(month_year = factor(paste(year_ticket_created, month_ticket_created, sep="-"))) 

# set center of map
center.lat = 42.392050 
center.lon = -71.109106

# time map (darker grey = more recent)
ratmap_time <- qmap(c(lon = center.lon, lat = center.lat), source = "google", maptype = "roadmap", 
                    zoom=14) + 
  geom_point(aes(x = lon, y = lat, color = month_year), size = 2, alpha = 0.5, 
             data = ratmapdata %>% filter(issue_type == "Rats")) + 
  scale_colour_grey(start = .9, end = 0, guide = FALSE) 

ggsave(ratmap_time, 
       filename=paste0(pdf_dir, 'ratmap_time_20190318.pdf'),
       device=cairo_pdf)

# ward map
ratmap_wards <- qmap(c(lon = center.lon, lat = center.lat), source = "google", maptype = "roadmap", 
                     zoom=14) + 
  geom_point(aes(x = lon, y = lat, color = neighborhood_district), size = 2, alpha = 0.5, 
             data = ratmapdata %>% filter(issue_type == "Rats")) +
  scale_colour_manual(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
                          "#FF7F00", "#ffb90f", "#A65628", "#F781BF")) + 
  theme(legend.title=element_blank())

ggsave(ratmap_wards, 
       filename=paste0(pdf_dir, 'ratmap_wards_20190318.pdf'),
       device=cairo_pdf)
