dat <- read.csv(file = "BBL2022_Req_828_enc_3640_20230209_120001 (2).csv")

# clean the data set

dim(dat)
uni_band_code <- unique(dat$band)
length(uni_band_code)

band_ct <- table(dat$band)

dat_sub <- data.frame()
for (i in uni_band_code) {                        # loop through each band
  tmp <- subset(dat, band == i)                   # subset to just that band
  if (sum(tmp$event_type %in% c('B', 'E')) >= 2) { # make sure B & E present
    if (all(tmp$other_bands %in% "")) {           # make sure no other band codes
      banding_event <- subset(tmp, event_type == 'B') # pull banding row
      encounter_event <- subset(tmp, event_type == 'E') # pull encounter row
      # remove how obtained codes that should be excluded
      encounter_gd <- !(encounter_event$how_obtained_code %in% c(56, 97,98))
      encounter_event <- encounter_event[encounter_gd, ]
      if (nrow(encounter_event) > 0) {       # make sure there is still some encounters 
        tmp <- rbind(banding_event, encounter_event) # create output
        dat_sub <- rbind(dat_sub, tmp)              # save output to big object
      }  
    }
  }
}

dim(dat_sub)
#' dat_sub$how_obtained_code %in% c('')


# group how obtained codes
natural_codes <- c('2', '12', '13', '15', '17', '24', '30', '36', '61', '64', '7', '9', '11', '20', '31', '34')
human_direct_codes <- c('1', '4', '10', '44')
human_indirect_codes <- c('21', '23', '26', '27', '39', '42', '54', '60', '62', '63', '64', '25', '45')
unknown_codes <- c('0', '50', '57', '3')

# create column for how obtained codes
code_table <- data.frame(how_obtained_code = c(natural_codes, human_direct_codes, human_indirect_codes, unknown_codes),
                         code = c(rep('natural', length(natural_codes)),
                                  rep('human_direct', length(human_direct_codes)),
                                  rep('human_indirect', length(human_indirect_codes)),
                                  rep('unknown', length(unknown_codes))))

# merge column and how obtained codes
dat_sub <- merge(dat_sub, code_table, all.x = TRUE)

# rename 
labels <- c(natural = "Natural", 
            human_direct = "Human Direct", 
            human_indirect = "Human Indirect", 
            unknown = "Unknown")

# Create bar plot
library(ggplot2)
library(tidyverse)
library(scales)

ggplot(subset(dat_sub, !is.na(code)), aes(y = after_stat(count)/sum(after_stat(count)), x = code, fill = code)) +
  geom_bar( ) +
  ggtitle(" ") +
  xlab(" ") +
  ylab(" ") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(labels = labels) +
  scale_fill_manual(values = c("tomato","tan2", "forestgreen", "grey")) +
  theme(legend.position="none")


# separate by year 1991
dat_sub$pre1991 <- ifelse(dat_sub$event_year < 1991, 'pre1991', 'post1991')
dat_sub$pre1991 <- factor(dat_sub$pre1991, levels = c('pre1991', 'post1991'))
levels(dat_sub$pre1991)

code_ct <- table(dat_sub$code)
code_ct_pre1991 <- table(dat_sub$code[dat_sub$event_year < 1991])
code_ct_post1991 <- table(dat_sub$code[dat_sub$event_year >= 1991])

# create bar plot for pre and post 1991 (I want statistics for this)
ggplot(subset(dat_sub, !is.na(code)), aes(x = code)) +
  geom_bar(aes (y = after_stat(count)/sum(after_stat(count)), fill = pre1991), position='dodge') +
  ggtitle(" ") +
  xlab(" ") +
  ylab(" ") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(labels = labels) +
  scale_fill_manual(values = c("indianred", "cadetblue")) +
  labs(fill = " ")



# create bar plot for main ten known mortality causes

# create subset for codes
codes <- c('1', '2', '3', '4', '10', '12', '13', '15', '17', '21', '23', '24', '26', '27', '30', '39', '42', '44', '54', '57', '60', '61', '62', '63', '64', '7', '9', '11', '14', '20', '25', '31', '34', '45')
dat_sub_subset <- dat_sub[dat_sub$how_obtained_code %in% codes, ]

# group codes
road_casualty <- c('45', '14', '60')
taken_by_animal <- c('34', '31', '7', '11', '9', '64', '12')
Poisoning <- c('25', '62')
Disease <- c('20', '61')
Striking <- c('63', '54', '42', '39', '13', '27')
Entanglement <- c('57', '26')
control_operations <- c('44')
Exhaustion <- c('36')
nest_mortality <- c('30', '24')
oil_or_tar <- c('23')
found_in_building <- c('21')
Drowned <- c('17')
weather_conditions <- c('15')
banding_mortality <- c('10')
traps_or_snares <- c('4')
Injury <- c('3')
Starvation <- c('2')
Shot <- c('1')

# create a new column, name it category, put it in the data frame
library(dplyr)
dat_sub_subset <- dat_sub_subset %>%
  mutate(category = case_when(
    how_obtained_code %in% road_casualty ~ "Road Casualty",
    how_obtained_code %in% taken_by_animal ~ "Taken by Animal",
    how_obtained_code %in% Poisoning ~ "Poisoning",
    how_obtained_code %in% Disease ~ "Disease",
    how_obtained_code %in% Striking ~ "Striking",
    how_obtained_code %in% Entanglement ~ "Entanglement",
    how_obtained_code %in% control_operations ~ "Control Operations",
    how_obtained_code %in% Exhaustion ~ "Exhaustion",
    how_obtained_code %in% nest_mortality ~ "Nest Mortality",
    how_obtained_code %in% oil_or_tar ~ "Oil or Tar",
    how_obtained_code %in% found_in_building ~ "Found in Building",
    how_obtained_code %in% Drowned ~ "Drowned",
    how_obtained_code %in% weather_conditions ~ "Weather Conditions",
    how_obtained_code %in% banding_mortality ~ "Banding Mortality",
    how_obtained_code %in% traps_or_snares ~ "Traps or Snares",
    how_obtained_code %in% Injury ~ "Injury",
    how_obtained_code %in% Starvation ~ "Starvation",
    how_obtained_code %in% Shot ~ "Shot",
    TRUE ~ "Other"
  ))
table(dat_sub_subset$category)

# add color 
library(RColorBrewer)
nb.cols <- 17
mycolors <- colorRampPalette(brewer.pal(8, "Blues"))(nb.cols)
mycolors

# make the bar graph
# I cannot get this to sort properly
ggplot(data = dat_sub_subset) +
  geom_bar(mapping = aes(x = category, y = after_stat(count)/sum(after_stat(count)), fill = category)) +
  coord_flip() +
  ggtitle(" ") +
  xlab(" ") +
  ylab(" ") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = mycolors) +
  theme(legend.position="none")



# separate by year 1991
dat_sub_subset$pre1991 <- ifelse(dat_sub_subset$event_year < 1991, 'pre1991', 'post1991')
dat_sub_subset$pre1991 <- factor(dat_sub_subset$pre1991, levels = c('pre1991', 'post1991'))
levels(dat_sub_subset$pre1991)

code_ct_sub <- table(dat_sub_subset$codes)
code_pre1991 <- table(dat_sub_subset$codes[dat_sub_subset$event_year < 1991])
code_post1991 <- table(dat_sub_subset$codes[dat_sub_subset$event_year >= 1991])

# make plot for pre and post
# I only want the top five mortality causes in each section
ggplot(subset(dat_sub_subset, !is.na(code)), aes(x = category)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = pre1991), position='dodge') +
  coord_flip() +
  ggtitle(" ") +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~ pre1991, ncol = 1) +
  scale_y_continuous(labels = percent_format()) +
  theme(legend.position="none")


# calculate probability of mortality
# mortality over time
# mortality throughout the year (seasons)
# mortality & age

# assign alive 0 and dead 1 and create a new column
'0' <- c('29', '33', '52', '53', '59', '66')
'1' <- c('0', '1', '2', '3', '4', '10', '12', '13', '15', '16', '17', '21', '23', '24', '26', '27', '30', '36', '39', '42', '44', '54', '57', '60', '61', '62', '63', '64', '7', '9', '11', '14', '20', '25', '31', '34', '45')

mort <- data.frame(how_obtained_code = c('0', '1'),
                   codes = c(rep('0', length('0')),
                             rep('1', length('1'))))

# merge column and how obtained codes
dat_sub <- merge(dat_sub, mort, all.x = TRUE, by = 'how_obtained_code')

head(dat_sub)

# list cannot be changed to vector
mortality <- as.integer(mort)

# plot, the y axis needs to be a scale..
ggplot(dat_sub, aes(x = event_year, y = code.y)) +
  method = "glm" +
  xlab("Year") +
  ylab("Mortality") +
  ggtitle("Probability of mortality over time")
