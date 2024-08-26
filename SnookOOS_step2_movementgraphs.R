##Snook out of system detection code
#Authors: Victoria Goldner, Mack White, Ryan James, Jonathan Rodemann


#set the stage
librarian::shelf(tidyverse, readr, janitor, lubridate, stringr, readxl)

SRsnook <- read_rds("sharkriver_V_POR_Aug14.rds") 
snook_all <- read_rds("snook_compiled_filtered_Aug14.rds") #filtered for 2x/array in 24hrs - consider adjusting to 48 hrs instead to be less strict



#goal: ID departures vs emigrations---------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

#make a list of all tags ever detected "out"
oostags_arrays <- snook_all %>% 
  filter(in_or_out == "out") %>%
  select(transmitter, array) %>% 
  distinct()
oostags <- oostags_arrays %>% 
  select(transmitter) %>% 
  distinct()
#would be good to make some kind of visual for which arrays appear most often

#step 1: add in river distances to SR only detections based on station------------------------------------------------

stationdistances <- read_csv("data/Station_Distance_Updated07142020.csv") |>
  janitor::clean_names()
glimpse(stationdistances)
glimpse(SRsnook)

# stationdistances <- stationdistances %>% rename(station_name = station)
# SRsnook_dist <- left_join(SRsnook, stationdistances, by = "station_name") #4434954 dist NAs
# 
# summary(SRsnook)
# summary(SRsnook_dist)

#lots of NAs - isolate them and figure out what the deal is
# NAs <- SRsnook_dist %>% filter(is.na(distance))
# NAstations <- NAs %>% select(station_name) %>% distinct()

#try with vue_name instead of station?
stationdistances <- stationdistances %>% rename(station_name = vue_name)
SRsnook_dist <- left_join(SRsnook, stationdistances, by = "station_name") #1649231 dist NAs

summary(SRsnook_dist)
#better, but still NAs.... investigate

NAs <- SRsnook_dist %>% filter(is.na(distance))
NAstations <- NAs %>% select(station_name) %>% distinct()
#issue stems from mixed station and vue names - need to get everything on the same page.
#pull out the detections with NA distances, bind using station, then put back together?

SRsnook_dist_noNA <- SRsnook_dist %>% filter(!is.na(distance)) #4435000
SRsnook_distNA <- SRsnook_dist %>% filter(is.na(distance)) #1649231
#should add to 6084231.... it does! now tack on distance using the other column to the NAs and recombine

stationdistances <- stationdistances %>% rename(vue_name = station_name)
stationdistances <- stationdistances %>% rename(station_name = station)

#need to remove the last 4 columns and then join
SRsnook_distNA <- SRsnook_distNA %>% select(-c(station, distance, latitude.y, longitude.y))


SRsnook_distNA <- left_join(SRsnook_distNA, stationdistances, by = "station_name")
summary(SRsnook_distNA)

#now rejoin the two files
SRsnook_distance_ready <- bind_rows(SRsnook_distNA, SRsnook_dist_noNA)
summary(SRsnook_distance_ready)

SRsnook_distance_ready <- SRsnook_distance_ready %>% 
  mutate(lat = coalesce(latitude, latitude.y),
  long = coalesce(longitude, longitude.y))

SRsnook_distance_ready <- SRsnook_distance_ready %>%
  select(-latitude, -latitude.y, -longitude, -longitude.y, -latitude.x, -longitude.x) 

SRsnook_distance_ready <- SRsnook_distance_ready %>%
  rename(latitude = lat,
         longitude = long)

#hell yeah! ready to go baby.

#instead, ditch any fish w/ <10 dets
#group by id, count # obs and then remove the <10 and keep the rest. 15km is guideline for indicating as moving into the lower river
#ie has a detection in the lower river / lower 15km, lines at 15 and 23 using "ghm line"
#geom_hline(yintercept =15, linetype ="dashed", color ="red", size =1), can also do it for the x int=# days to apr 1 via # day of the year


#bring in mack's code---------------------------------------------------------------

# clean <- SRsnook_distance_ready |> 
#   # remove IDs detected only in one river zone
#   group_by(transmitter) |> 
#   mutate(zones_visited = n_distinct(zone)) |> 
#   filter(zones_visited > 1) |>  #lost ? detections
#   ### exclude first seven days of detection history
#   mutate(min_date = min(datetime_utc)) |>
#   filter(datetime_utc > min_date + days(7)) |>
#   ### remove columns used for filtering
#   select(-zones_visited, -min_date) |>
#   ungroup()

# breaks <- yday(as.Date(paste0("2023-", 1:12, "-01")))
# labels <- month.name[1:12]
# 
# migration_plot <- function(f) {
#   dt_hydro_filter |> 
#     filter(id == f) |> 
#     ggplot() +
#     geom_line(aes(x = dayofyear, y = distance, color = as.factor(year)), size = 1) +
#     scale_x_continuous(breaks = breaks, labels = labels) +
#     theme_classic() +
#     labs(title = f,
#          x = 'date',
#          y = 'distance') +
#     scale_fill_brewer(palette = "Set1") + 
#     theme(axis.text.x = element_text(angle = 45, hjust = 1),
#           panel.background = element_rect(fill = "white"),
#           axis.line = element_line("black"),
#           axis.text = element_text(face = "bold"),
#           axis.title = element_text(face = "bold"))
# }
# 
# # Generate boxplot figures for each projecthabitat
# migration_test <- map(unique(dt_hydro_filter$id), migration_plot)
# 
# ggsave(
#   filename = "inter-annual-migration-timing.pdf",
#   # path = "migration_test",
#   plot = marrangeGrob(migration_test, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

#------------------------------------------------------------------------------
#filter out all fish with fewer than 10 detections in SR

SRclean <- SRsnook_distance_ready %>% 
  mutate(year = year(datetime_utc)) %>%
  group_by(transmitter) %>%
  ungroup

SRcount <- SRclean %>% 
  count(transmitter) %>% 
  filter(n < 10) #only transmitter A69-9001-21964 should be removed

SRclean <- SRclean %>% filter(transmitter != "A69-9001-21964")

#mutate a column to indicate a high, mid, or departure zone detection (distance <15)

SRdetzone <- SRclean %>% mutate(detection_zone = case_when(distance <= 15 ~ "departure",
                                                           distance > 15 & distance < 23 ~ "mid",
                                                           distance >= 23 ~ "upper"))

#write_csv(SRdetzone, "SR_PORwithzones_V_OOS_Aug25.csv")

#-----------------------------------------------------
# w cody 8/22
#put dept classification in sr file and make the sr and oos detections match zone, dist and then add them together

#SRdetzone <- read.csv("SR_PORwithzones_V_OOS_Aug25.csv")


glimpse(SRdetzone)
glimpse(snook_all)

#once SR is clean, can put this on the end of the code chunk and see what happens


SRzones_final <- SRdetzone %>% 
  #mutate(date = date(datetime_utc)) %>%
  #filter(transmitter %in% c("A69-1303-58417", "A69-1303-32902")) %>%
  #filter(year %in% c("2014", "2015")) %>%
  group_by(transmitter) %>% 
  arrange(datetime_utc, .by_group = TRUE) %>%
  # mutate(depart_criteria = factor(if_else(detection_zone == "departure" & 
  #                                           datetime_utc == max(datetime_utc), "finaldept", 
  #                                  #if_else(detection_zone == "departure" & (lead(datetime_utc) > 1728000), "dept",
  #                                  #if_else(detection_zone == "departure" & (lag(datetime_utc) > 1728000), "return",
  #                                  "OK")),
  #        depart_criteria2 = factor(if_else(detection_zone == "departure" & 
  #                                            lead(datetime_utc) > 5184000 & 
  #                                               depart_criteria != "finaldept", "dept", depart_criteria)),
  #        depart_criteria3 = factor(if_else(lag(depart_criteria2) == "dept" & 
  #                                            !depart_criteria2 %in% c("finaldept","dept"), "return", depart_criteria2)),
  #        depart_criteria4 = factor(if_else(is.na(depart_criteria3), "NA", depart_criteria3))) %>%
  mutate(check = datetime_utc - lag(datetime_utc),
         check2 = datetime_utc - lead(datetime_utc),
         check3 = difftime(datetime_utc, lag(datetime_utc), units = "days"),
         check4 = difftime(datetime_utc, lead(datetime_utc), units = "days")) %>%
  # mutate(depart_criteria = factor(case_when(detection_zone == "departure" & datetime_utc == max(datetime_utc) ~ "finaldept", TRUE ~ NA_character_)),
  #        depart_criteria2 = factor(case_when(detection_zone == "departure" & check3 > 20 #&
  #                                            # depart_criteria != "finaldept" 
  #                                              ~ "dept", TRUE ~ NA_character_)),
  #        depart_criteria3 = factor(case_when(detection_zone == "departure" & check4 < -20 ~ "return", TRUE ~ NA_character_))) %>%
                                   # detection_zone == "departure" & lead(datetime_utc) > 5184000 & depart_criteria != "finaldept" ~ "dept",
                                   # detection_zone == "departure" & lag(datetime_utc) > 5184000 ~ "return",
                                  #TRUE ~ NA_character_))) %>%  # Use NA_character_ for missing values in character columns))
  mutate(depart_criteria = factor(case_when(detection_zone == "departure" & check4 < -20 ~ "dept",
                                            check3 > 20 & lag(detection_zone) == "departure" & 
                                              datetime_utc != max(datetime_utc) ~ "return", 
                                            detection_zone == "departure" & 
                                              datetime_utc == max(datetime_utc) ~ "finaldept",
                                            .default = "detection"))) %>%
  #filter(depart_criteria %in% c("dept", "return")) %>%
  ungroup() %>% #) #criteria for departure
  mutate(depart_criteria = fct_relevel(depart_criteria, "dept", "return", "finaldept"))

#-----------------------------------------------------------------------------------------------------

#JON: Lines 208 thru216 are an unfinished for-loop Ryan James started helping me with. Above we were able to ID
#departs and returns without accounting for the migration, Ryan wanted to add ini a for loop to look at those departures
#and ID whether in the 30 days before a coded departure, the fish had been detected in a different river zone

#Ryan for loop:--------------------------------------------------------------------------------------------------
#add a line of code asking if within last month or similar, were they detected in a different zone?
dept = SRzones_final %>% filter(depart_criteria == 'dept')

for(i in 1:nrow(dept)){
  d = SRzones_final %>% 
    filter(datetime_utc %within% interval(dept$datetime_utc[i]- days(20), dept$datetime_utc[i]),
           transmitter == dept$transmitter[i]) 
  
  #if d dept zone contains mid or upper,set the values that are = say y if migrant and n if not. match conditions from dept dataset so it is exact
}
  
  

#---------------------------------------------------------------------------------
#need to filter for some dead fish and need to modify the full POR to be able to plot similarly
#---------------------------------------------------------------------------------
summary(SRzones_final) #MORE RETURNS THAN DEPARTS - GO THRU AND ID WHY / WHERE INCONSISTENT
glimpse(SRzones_final)
glimpse(SRdetzone)

# plot <- ggplot() +
#   geom_line(data = SRzones_final, aes(x = datetime_utc, y = distance), size = 1) +
#   # scale_x_continuous(breaks = breaks, labels = labels) +
#   #make point detections as the depart criteria 4
#   geom_point(data = SRzones_final %>% 
#                filter(depart_criteria != "detection") %>% 
#                droplevels(), aes(x = datetime_utc, y = distance, color = depart_criteria2, shape = depart_criteria2),
#              size = 2, stroke = 2) +
#   geom_hline(yintercept =15, linetype ="dashed", color ="purple", linewidth =1) +
#   geom_hline(yintercept =23, linetype ="dashed", color ="purple", linewidth =1) +
#   scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 35)) +
#   theme_classic() +
#   facet_grid(transmitter~.) +
#   #labs(title = f, #f = ID
#   #     x = 'datetime_utc',
#   #     y = 'distance') +
#   #scale_color_brewer(palette = "Set1") + 
#   scale_color_manual(values = c("orange", "green", "red")) +
#   scale_shape_manual(values = c(1, 2, 13)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         panel.background = element_rect(fill = "white"),
#         axis.line = element_line("black"),
#         axis.text = element_text(face = "bold"),
#         axis.title = element_text(face = "bold"))
# plot
#can now put this back into the function and map over everything
# now try mack's movement plots -------------------------------------------

#subset of fish rq
fish <- SRzones_final %>%
  select(transmitter) %>%
  distinct() %>%
  slice(1:10)

SRzones_final2 <- SRzones_final %>% 
  filter(transmitter %in% fish$transmitter) 


movements <- function(f) {
  SRzones_final |> #dataset
    filter(transmitter == f) |> #transmitter
    ggplot() +
    geom_line(aes(x = datetime_utc, y = distance), size = 1) +
    # scale_x_continuous(breaks = breaks, labels = labels) +
    #make point detections as the depart criteria 4
    geom_point(data = . %>% 
                 filter(depart_criteria != "detection") %>% 
                 droplevels(), aes(x = datetime_utc, y = distance, color = depart_criteria, shape = depart_criteria),
               size = 2, stroke = 1) +
    geom_point(data = . %>% filter(detection_zone == "departure") %>%
                 filter(depart_criteria == "detection"), aes(x = datetime_utc, y = distance), color = "black", size = 1) +
    geom_hline(yintercept =15, linetype ="dashed", color ="purple", linewidth =1) +
    geom_hline(yintercept =23, linetype ="dashed", color ="purple", linewidth =1) +
    scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 35)) +
    theme_classic() +
    #facet_grid(transmitter~.) +
    labs(title = f, #f = ID
        x = 'datetime_utc',
        y = 'distance') +
    #scale_color_brewer(palette = "Set1") +
    scale_color_manual(values = c("orange", "green", "red"),
                       breaks = c("dept", "return", "finaldept")) +
    scale_shape_manual(values = c(1,2,13),
                       breaks = c("dept", "return", "finaldept")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

SRzones_mapped <- map(unique(SRzones_final$transmitter), movements)

library(gridExtra)

ggsave(
  filename = "V_SR_OOS_movementplots_withMackandCody3_aug23.pdf",
  # path = "migration_test",
  plot = marrangeGrob(SRzones_mapped, nrow = 1, ncol = 1),
  width = 15, height = 9
)


#FIGURING OUT TROUBLE FISH
#WANT FULL DATAFRAME, go back and overwrite depart criteria with depart criteria 2 for this
glimpse(SRzones_final)

check <- SRzones_final %>%
  filter(depart_criteria != "detection") %>%
  group_by(transmitter, depart_criteria) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = depart_criteria, values_from = count) %>%
  mutate(no_good = factor(return > dept)) %>%
  filter(no_good == "TRUE")
#can now subset using those transmitters like we did before and look at those plots specifically to see what the deal is

glimpse(check)

#-------------------------------------------------------------------------------------------------------------------
#NEXT STEP: For the file with the OOS detections, create an additional river zone for "out" with dist = -5km
#and plot using the same movement plots, per fish over full track, to visualize when they were detected out
#can color-code by array to ID where it was



#after that, can pull out all the info we want / create summary plots, etc








#--------------------------------------------------------------------------------------------------------------------
# movements <- function(f) {
#   SRdetzone |> #dataset
#     filter(transmitter == f) |> #transmitter
#     ggplot() +
#     geom_line(aes(x = datetime_utc, y = distance), size = 1) +
#     # scale_x_continuous(breaks = breaks, labels = labels) +
#       #make point detections as the depart criteria 4
#     geom_point(aes(x = datetime_utc, y = distance, color = depart_criteria4)) +
#     geom_hline(yintercept =15, linetype ="dashed", color ="red", linewidth =1) +
#     geom_hline(yintercept =23, linetype ="dashed", color ="red", linewidth =1) +
#     scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30), limits = c(0, 35)) +
#     theme_classic() +
#     labs(title = f, #f = ID
#          x = 'datetime_utc',
#          y = 'distance') +
#     scale_color_brewer(palette = "Set1") + 
#     theme(axis.text.x = element_text(angle = 45, hjust = 1),
#           panel.background = element_rect(fill = "white"),
#           axis.line = element_line("black"),
#           axis.text = element_text(face = "bold"),
#           axis.title = element_text(face = "bold"))
# }
#add indicators for the spawning seasons

#may need to constrain x axis ie in calendar year and do per year, etc - create new column where you do id + yr via paste function like mutate
#do for each individ+yr combo


# Generate boxplot figures for each fish, all years
# mvmt_test <- map(unique(SRdetzone$transmitter), movements)
# 
# mvmt_test <- map((SRdetzone %>%
#                     select(transmitter) %>%
#                     distinct() %>%
#                     filter(transmitter %in% c("A69−1303−58406", "A69−1303−51310")) %>%
#                     droplevels()), movements)


#install.packages("gridExtra")
#library(gridExtra)

# ggsave(
#   filename = "V_SR_OOS_movementplots_withCody_aug22.pdf",
#   # path = "migration_test",
#   plot = marrangeGrob(mvmt_test, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

#now do another but per year per fish, add the vertical lines for spawning april 1 to nov 15
#first, need to make a unique fish_year column
SR_ID_yr <- SRdetzone %>% mutate(transmitter_yr = paste(transmitter, year, sep = "_"))

# movements2 <- function(f) {
#   SR_ID_yr |> #dataset
#     filter(transmitter_yr == f) |> #transmitter
#     ggplot() +
#     geom_line(aes(x = datetime_utc, y = distance, color = "black"), linewidth = 1) +
#     #scale_x_continuous(breaks = breaks, labels = labels) +
#     geom_hline(yintercept =15, linetype ="dashed", color ="red", linewidth =1) +
#     geom_hline(yintercept =23, linetype ="dashed", color ="red", linewidth =1) +
#     geom_vline(xintercept =91, linetype ="dashed", color ="blue", linewidth =1) +
#     geom_vline(xintercept =319, linetype ="dashed", color ="blue", linewidth =1) +
#     scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
#     theme_classic() +
#     labs(title = f, #f = ID
#          x = 'datetime_utc',
#          y = 'distance') +
#     scale_fill_brewer(palette = "Set1") + 
#     theme(axis.text.x = element_text(angle = 45, hjust = 1),
#           panel.background = element_rect(fill = "white"),
#           axis.line = element_line("black"),
#           axis.text = element_text(face = "bold"),
#           axis.title = element_text(face = "bold"))
# }
# 
# 
# # Generate boxplot figures for each fish, all years
# mvmt_2 <- map(unique(SR_ID_yr$transmitter_yr), movements2)
# 
# ggsave(
#   filename = "V_SR_OOS_movementplots_perfish_peryear_Aug15.pdf",
#   # path = "migration_test",
#   plot = marrangeGrob(mvmt_2, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

#instead, mack sent me this:
librarian::shelf(tidyverse, readxl, gridExtra, glmmTMB, MuMIn, sjPlot, lme4, corrplot, performance, ggeffects, ggpubr, parameters)


# generate breaks + labels for plotting -----------------------------------
# breaks <- yday(as.Date(paste0("2023-", 1:12, "-01")))
# labels <- month.name[1:12]

# SR_ID_yr |> 
#   filter(id_year =='A69-1303-32892 2014') |> 
#   ggplot(aes(x = dayofyear, y = distance)) +
#   geom_line(size = 0.5) +
#   geom_point(size = 1.5) +
#   geom_hline(yintercept = c(15,23), linetype = 'dashed', color = 'red') +
#   geom_vline(xintercept = c(91,320), linetype = 'dashed', color = 'black') +
#   scale_x_continuous(breaks = breaks, labels = labels) +
#   scale_y_continuous(limits = c(0,35), breaks = seq(0,35, by = 5)) +
#   theme_classic() +
#   labs(title = 'f',
#        x = 'date',
#        y = 'distance') +
#   scale_fill_brewer(palette = "Set1") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         panel.background = element_rect(fill = "white"),
#         axis.line = element_line("black"),
#         axis.text = element_text(face = "bold"),
#         axis.title = element_text(face = "bold"))


#######

breaks <- yday(as.Date(paste0("2023-", 1:12, "-01")))
labels <- month.name[1:12]


#need to make: dayofyear element
SR_ID_yr <- SR_ID_yr %>% mutate(dayofyear = yday(datetime_utc))
  
  
movement_plot <- function(f) {
  SR_ID_yr |> 
    filter(transmitter_yr == f) |> 
    ggplot(aes(x = dayofyear, y = distance)) +
    geom_line(size = 0.5) +
    geom_point(size = 1.5) +
    geom_hline(yintercept = c(15,23), linetype = 'dashed', color = 'red') +
    geom_vline(xintercept = c(91,320), linetype = 'dashed', color = 'black') +
    scale_x_continuous(breaks = breaks, labels = labels) +
    scale_y_continuous(limits = c(0,35), breaks = seq(0,35, by = 5)) +
    theme_classic() +
    labs(title = f,
         x = 'date',
         y = 'distance') +
    scale_fill_brewer(palette = "Set1") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

# Generate boxplot figures for each projecthabitat
movements2 <- map(unique(SR_ID_yr$transmitter_yr), movement_plot)

ggsave(
  filename = "V_SR_OOS_movementplots_perfish_peryear_Aug15.pdf",
  # path = "migration_test",
  plot = marrangeGrob(movements2, nrow = 1, ncol = 1),
  width = 15, height = 9
)


#mack said scale the y with: scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30))--------------


#step 3: create movement graphs



#--------------------------------------------------------------------------------------------------
#get distance for full, clean POR with distance they traveled
#ex create a POR movement plot and make a visual to show whether they were picked up oos or not
#make y axis river zones and make an extra one for oos and level in the correct order, color code by array

#for now: make oos zones dist = -1