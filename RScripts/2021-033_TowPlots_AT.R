#=====================================================================================================
#
# 2021-033_TowPlots.R
# Erika Anderson
# 2021-06-24
# Modified from Ben Snow's r script from Aug 2018
# R Version: 4.0.4
#
# Use to graph RBR duet depth sensors 
# integrated with GFBioField bridge log table information
# Make sure RBR sensors are synced before deployment 
# use local time to match bridge log
#
#=====================================================================================================

# libraries
require(dplyr)
require(lubridate)
require(readxl)
require(data.table)
require(ggplot2)

# survey specific variables
survey <- "2019-124-Survey" # for testing
# survey <- "2021-033-Survey" 

Day1 <- "29"
Month1 <- "06"
Year1 <- "2019"

GFBio_input <- paste0(survey, "/GFBIO/FISHING_EVENT-",
                      Year1, "-", Month1, "-", Day1,
                      ".xlsx")
Headrope_input <- paste0(survey, "/HEADROPE")
Footrope_input <- paste0(survey, "/FOOTROPE")

#Set a path for saving net opening plots that are created at the last stage of this script
#plot_path <- paste0(survey, "/Plots", as.character(Sys.Date()))
#dir.create(plot_path)

plot_path <- paste0(survey, "/Plots_", 
                    Year1, "-", Month1, "-", Day1)
dir.create(plot_path)


#########################################
#Read in GFBio Fishing Event Export, including FISIHING_EVENT_ID, FE_MAJOR_ID, DATE, FE_END_DEPLOYMENT_TIME, FE_BEGIN_RETRIEVAL
#GEAR_CODE, TRLSP_WARP_LENGTH, TRLSP_DOORSPREAD. Remove unnessecary columns.

event_num <- read_xlsx(GFBio_input) 
event_num <- subset(event_num, select = c("FISHING_EVENT_ID",
                                          "FE_MAJOR_LEVEL_ID",
                                          "DATE",
                                          "FE_END_DEPLOYMENT_TIME",
                                          "FE_BEGIN_RETRIEVAL_TIME",
                                          "TRLSP_WARP_LENGTH",
                                          "TRLSP_DOORSPREAD"
))                       

# Read in excel file exports from RBR duet loggers on headrope and footrope; loop through all .xlsx files in the headrope and
# footrope directories, and merge them into one file for each of the footrop end headrope datasets.

merged_foot <- data.frame()
merged_head <- data.frame()

# load all headrope files
head_files <- list.files(path = Headrope_input, pattern = ".xlsx")

for (i in 1:length(head_files))
{
  name <- as.character(i)
  assign(name, read_xlsx(path = paste0(Headrope_input, "/", head_files[i]), sheet = 3, skip = 1))
  if (name == "1")
  {merged_head <- get(name)
  }else merged_head <- rbind(merged_head, get(name))
  rm(list = c(i))
}

# load foot rope files
foot_files <- list.files(path = Footrope_input, pattern = ".xlsx")

for (k in 1:length(foot_files))
{
  name <- as.character(k)
  assign(name, read_xlsx(path = paste0(Footrope_input, "/", foot_files[k]), sheet = 3, skip = 1))
  if (name == "1")
  {merged_foot <- get(name)
  }else merged_foot <- rbind(merged_foot, get(name))
  rm(list = c(k))
}

# Match foot and headrope record times, merge them into a new data frame with both foot and headrope depth readings.
net_calcs <- merge(merged_foot, merged_head, by.x = 'Time', by.y = "Time", suffixes = c('_foot','_head'))

#Keep only Time, and foot and headrope depth columns
net_calcs <- subset(net_calcs, select = c(1,5,9))

# plot depth sensor depths
ggplot(data = net_calcs) +
  geom_line(aes(x = Time, y = Depth_foot)) +
  scale_y_reverse(lim = c(60,0)) +
  labs(x = "Depth in feet",
       title = "RBR results") +
  theme_bw()

ggsave(paste0(path = plot_path, "/rbrRaw.png"))

# create tow time from GFBio data
event_num_display <- event_num %>%
  mutate(tow_time = as.numeric(round(FE_BEGIN_RETRIEVAL_TIME - FE_END_DEPLOYMENT_TIME, 2)))

# plot events to compare to depth sensors depths
ggplot(data = event_num_display, aes(x = FE_END_DEPLOYMENT_TIME, y = tow_time)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(title = "GFBio Events",
       y = "Time",
       y = "Tow Time in minutes")

ggsave(paste0(path = plot_path, "/GFBioRaw.png"))

###################################
# add minutes in order to see the doors reaching target depth
#minute(event_num$FE_END_DEPLOYMENT_TIME) <- minute(event_num$FE_END_DEPLOYMENT_TIME) - 3

# Clip RBR data to readings where the head rope is greater than 1 m deep.
net_calcs <- filter(net_calcs, Depth_head > 1)

#Search the set intervals  in the event_num dataframe, 
#(i.e. time between FE_END_DEPLOYMENT_TIME and FE_BEGIN_RETRIEVAL_TIME, for each event)
#and match the timestamp of each RBR reading to the correct intervals in the net_calcs dataframe.

# Create vector of RBR timestamps
rbr_times <- net_calcs$Time

#Coerce to a event_num to a data.table object, then search within intervals.
plotting_DF <- setDT(event_num)[data.table(rbr_times), on = .(FE_END_DEPLOYMENT_TIME <= rbr_times, FE_BEGIN_RETRIEVAL_TIME >= rbr_times)]

#Remove unused columns, and rename for ease of merging.
plotting_DF <- subset(plotting_DF, select = c(FE_MAJOR_LEVEL_ID, FE_END_DEPLOYMENT_TIME,TRLSP_WARP_LENGTH, TRLSP_DOORSPREAD))
colnames(plotting_DF) <- c("Event","Time","Warp_out","Door_spread")

#Merge the plotting_DF and net_calcs data frames using a common time stampvalue.
plotting_DF <- merge(plotting_DF, net_calcs, by.x = "Time", by.y = "Time")

#Filter out and Events that are NA (outside the trawl set periods)
plotting_DF <- filter(plotting_DF, !is.na(Event))

#Calculate net opening (footrope - headrope)
plotting_DF$Opening <- plotting_DF$Depth_foot - plotting_DF$Depth_head

# Clip RBR data to readings where the foot rope and head rope are greater than 0 m deep.
net_calcs <- filter(net_calcs, Depth_foot > 0)
net_calcs <- filter(net_calcs, Depth_head > 0)

#Plots for each Event  

for (j in unique(plotting_DF$Event))
{
  opening_data <- filter(plotting_DF, Event == j)
  mean <- paste("Mean Net Opening:",as.character(round(mean(opening_data$Opening),1)), sep = " ")
  median <- paste("Median Net Opening:",as.character(round(median(opening_data$Opening),1)), sep = " ")
  warp <- paste("Warp Out:", as.character(mean(opening_data$Warp_out)), sep = " ")
  spread <- paste("Door Spread:", as.character(mean(opening_data$Door_spread)), sep = " ")
  
  ggplot(opening_data) + 
    geom_line(aes(x = Time, y = Depth_head, color = "Headrope Depth")) + 
    geom_line(aes(x = Time, y = Depth_foot, color = "Footrope Depth")) + 
    scale_x_datetime(date_labels = "%b-%d %H:%M") +
    geom_hline(yintercept = 15) +
    scale_y_reverse(lim = c(50,0)) + labs(y = "Depth (m)", x = "Time") + 
    ggtitle(paste("Event", j, sep = " "), 
            subtitle = paste(mean, "\n", median, "\n", warp, "\n", spread, sep = " ")) +
    theme_bw()
  
  ggsave(filename = paste("Net Plot", as.character(j), ".png"), plot = last_plot(), path = plot_path, width = 8.5, 
         height = 5.33, units = c('in'))
}

# check for negative mean net openings
plotting_DF_Mean <- plotting_DF %>%
  group_by(Event) %>%
  summarise(., meanOpen = mean(Opening)) %>%
  ungroup()

# change to 
plotting_DF_Mean <- as_tibble(plotting_DF_Mean)

# graph and look for negatives
ggplot(plotting_DF_Mean, aes(x = Event, y = meanOpen)) +
  geom_point() +
  labs(title = "Mean Openning by Event") +
  theme_bw()

ggsave(paste0(path = plot_path, "/AnyNegativeOpenning.png"))
