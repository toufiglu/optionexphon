#load packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(afex)
library(emmeans)
library(ggplot2)
library(ggpubr)


#load demographics data for presentation in methodology.
demographics <- read_csv('demographics.csv')
summary(demographics$Age)
sd(demographics$Age)


#draw plots for experiment design.
# Create the plot with the circles and background/axis elements
# Create a data frame for the specified circles
circles1 <- data.frame(
  VOT = c(1, 2, 2, 2, 3, 4, 4, 4, 5, 6, 6, 6, 7),
  F0 = c(250, 240, 250, 260, 250, 230, 260, 290, 270, 260, 270, 280, 270),
  Shape = c("Hollow", "Hollow", "Hollow", "Hollow", "Hollow", "Solid", "Solid", "Solid", "Hollow", "Hollow", "Hollow", "Hollow", "Hollow")
)

plot1 <- ggplot(data = circles1, aes(x = VOT, y = F0)) +
  geom_point(shape = 1, size = 3, color = "black") +
  geom_point(data = subset(circles1, VOT == 4), shape = 16, size = 3, fill = "black") +
  labs(x = "VOT", y = "F0") +
  scale_x_continuous(breaks = 1:7, limits = c(1, 7)) +
  scale_y_continuous(limits = c(220, 300)) +
  theme_bw()
plot1

# Create a data frame for the specified circles
circles2 <- data.frame(
  VOT = c(1, 2, 2, 2, 3, 4, 4, 4, 5, 6, 6, 6, 7),
  F0 = c(230, 220, 230, 240, 230, 230, 260, 290, 290, 280, 290, 300, 290),
  Shape = c("Hollow", "Hollow", "Hollow", "Hollow", "Hollow", "Solid", "Solid", "Solid", "Hollow", "Hollow", "Hollow", "Hollow", "Hollow")
)

# Create the plot with the circles and background/axis elements
plot2 <- ggplot(data = circles2, aes(x = VOT, y = F0, shape = Shape)) +
  geom_point(size = 3, color = "black") +
  scale_shape_manual(values = c(1, 16), guide = FALSE) +
  labs(x = "VOT", y = "F0") +
  scale_x_continuous(breaks = 1:7, limits = c(1, 7)) +
  scale_y_continuous(limits = c(220, 300)) +
  theme_bw()

plot2
# Save the plot as a PNG image
ggsave(filename = "plot.png", plot = plot, width = 6, height = 4, dpi = 300)

# Create a data frame for the specified circles
circles3 <- data.frame(
  VOT = c(3, 4, 4, 4, 5, 4, 4, 4, 3, 4, 4, 4, 5),
  F0 = c(230, 220, 230, 240, 230, 230, 260, 290, 290, 280, 290, 300, 290),
  Shape = c("Hollow", "Hollow", "Hollow", "Hollow", "Hollow", "Solid", "Solid", "Solid", "Hollow", "Hollow", "Hollow", "Hollow", "Hollow")
)

# Create the plot with the circles and background/axis elements
plot3 <- ggplot(data = circles3, aes(x = VOT, y = F0, shape = Shape)) +
  geom_point(size = 3, color = "black") +
  scale_shape_manual(values = c(1, 16), guide = FALSE) +
  labs(x = "VOT", y = "F0") +
  scale_x_continuous(breaks = 1:7, limits = c(1, 7)) +
  scale_y_continuous(limits = c(220, 300)) +
  theme_bw()
plot3

# data processing
# draw demos for the stimuli of the three stages
# Create a data frame for the specified circles
circles1 <- data.frame(VOT = c(1, 2, 2, 2, 3, 4, 4, 4, 5 ,6, 6, 6, 7),
                       F0 = c(250, 240, 250, 260, 250, 230, 260, 290, 270, 260, 270, 280, 270))

#load performance data
wholefile <- read_csv('VOT.csv')
head(wholefile)

#pre-processing
#remove all unnecessary data in three steps.
displays_to_exclude <- c("WelcomeScreens", "ExerciseTrials", "StartStudy", "", "Rest1", "Rest2", "Finish")
first_clean <- wholefile %>% filter(!(is.na(Display) | Display %in% displays_to_exclude))

first_clean  

#remove all rows that records the starting of the audio files
second_clean <- first_clean %>% filter(Response != "audio started")
second_clean

#collapse all trail names into a new column 
third_clean <- second_clean %>%
  mutate_at(vars(matches("1[0-9]|2[0-9]|3[0-9]")), ~ifelse(is.na(.), "", .)) %>%
  unite(trials, matches("1[0-9]|2[0-9]|3[0-9]"))

third_clean$trials <- gsub("_", "", third_clean$trials)
third_clean

#remove training trails
Testtrails <- c("TestBPF02VOT4", "TestBPF08VOT4", "TestDTF02VOT4", "TestDTF08VOT4")
VOT <- third_clean[third_clean$trials %in% Testtrails, ]
VOT

#re-code data
VOT <- VOT %>%
  mutate(trials = case_when(
    trials == "TestBPF02VOT4" ~ "BPlow",
    trials == "TestBPF08VOT4" ~ "BPhigh",
    trials == "TestDTF02VOT4" ~ "DTlow",
    trials == "TestDTF08VOT4" ~ "DThigh",
    TRUE ~ trials
  ))

#calculate error rate for each participant to remove outliers.
colnames(VOT)[colnames(VOT) == "Participant External Session ID"] <- "Participants"
VOT
participant_counts <- table(VOT$Participants)
print(participant_counts)
participant_counts

# Create a new column based on conditions
VOT$errorcount <- ifelse(
  (VOT$Response == "beer" | VOT$Response == "pier") & grepl("^BP", VOT$trials), 
  0,
  ifelse(
    (VOT$Response == "beer" | VOT$Response == "pier") & grepl("^DT", VOT$trials),
    1,
    ifelse(
      (VOT$Response == "deer" | VOT$Response == "tear") & grepl("^DT", VOT$trials),
      0,
      ifelse(
        (VOT$Response == "deer" | VOT$Response == "tear") & grepl("^BP", VOT$trials),
        1,
        NA  # Value to assign if none of the conditions are met
      )
    )
  )
)
VOT

# Sum the error count for each participant
participant_error_sum <- aggregate(errorcount ~ Participants, data = VOT, FUN = sum)
participant_error_sum

table_participant_counts <- as.data.frame(participant_counts)
names(table_participant_counts) <- c("Participants", "Total")

table_participant_counts
table_participant_counts$Total <- 120 - table_participant_counts$Total

merged_table <- merge(participant_error_sum, table_participant_counts, by = "Participants")
merged_table
merged_table$errorsum <- merged_table$errorcount + merged_table$Total
merged_table

#identified 6 participants whose error rate is higher than 10%

excluded_participants <- c('6487060bd4a971f11be9992a', '6487070e55af62d92fd5ea2f', 
             '64870894b83d7e75727b31d2', '64870b1613e3e4dccbc42b0a', 
             '64870b408577c3486ad12bb9','64870d031fcf260e8986f9fd')

mean (merged_table$errorsum)
sd(merged_table$errorsum)
VOT
excluded_table <- merged_table[!(merged_table$Participants %in% excluded_participants), ]
excluded_table
summary(excluded_table$errorsum)
sd(excluded_table$errorsum)
filtered_VOT <- VOT[!(VOT$Participants %in% excluded_participants), ]
filtered_VOT
filtered_VOT <- filtered_VOT[, !colnames(filtered_VOT) %in% "errorcount"]
filtered_VOT$errorcount <- ifelse(
  (filtered_VOT$Response == "beer" | filtered_VOT$Response == "pier") & grepl("^BP", filtered_VOT$trials), 
  0,
  ifelse(
    (filtered_VOT$Response == "beer" | filtered_VOT$Response == "pier") & grepl("^DT", filtered_VOT$trials),
    1,
    ifelse(
      (filtered_VOT$Response == "deer" | filtered_VOT$Response == "tear") & grepl("^DT", filtered_VOT$trials),
      0,
      ifelse(
        (filtered_VOT$Response == "deer" | filtered_VOT$Response == "tear") & grepl("^BP", filtered_VOT$trials),
        1,
        NA  # Value to assign if none of the conditions are met
      )
    )
  )
)
filtered_VOT
final_table <- filtered_VOT[filtered_VOT$errorcount != 1, ]
final_table
final_table <- final_table[, !colnames(final_table) %in% "errorcount"]

write.csv(final_table, file = "filtered.csv", row.names = FALSE)

final_table

BP <- final_table[final_table$Response %in% c("beer", "pier"), ]
DT <- final_table[final_table$Response %in% c("deer", "tear"), ]

BP$trials <- gsub("^BP", "", BP$trials)
DT$trials <- gsub("^DT", "", DT$trials)

BP
DT

BP$Response <- ifelse(BP$Response == "beer", 0, ifelse(BP$Response == "pier", 1, BP$Response))
DT$Response <- ifelse(DT$Response == "deer", 0, ifelse(DT$Response == "tear", 1, DT$Response))

BP$Block <- as.numeric(substr(BP$Display, 1, 1))
DT$Block <- as.numeric(substr(DT$Display, 1, 1))

BP <- BP[, c("Participants", "Display", "Block", "trials", "Response")]
DT <- DT[, c("Participants", "Display", "Block", "trials", "Response")]

BP$Response <- as.numeric(BP$Response)
DT$Response <- as.numeric(DT$Response)

colnames(BP)[colnames(BP) == "trials"] <- "F0"
colnames(DT)[colnames(DT) == "trials"] <- "F0"



#now, let`s compute the average response for voiceless sounds collapsed for all speakers
#BP and exclude the 5 outliers
BP_clean <- BP %>%
  filter(Participants != "64872c98214642b52e4eb12d" &
           Participants != "64870cd2e9ea8c9e102c52be" &
           Participants != "64870c0ddfe4f0fad2e69bad" &
           Participants != "648706e955af62d92fd5ea14" &
           Participants != "64870cb78577c3486ad12c32")

# View the filtered data
BP_clean
# Filter the data for Block starting with 1
BP_1_h <- BP_clean %>%
  filter(str_starts(Block, "1"), F0 == "high")
overall_probability_p_1_h <- mean(BP_1_h$Response == 1)
overall_probability_p_1_h

BP_1_l <- BP_clean %>%
  filter(str_starts(Block, "1"), F0 == "low")
overall_probability_p_1_l <- mean(BP_1_l$Response == 1)
overall_probability_p_1_l

# Group the data by Participants and calculate the probability of Response being 1
participant_probability_p_1_h <- BP_1_h %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
participant_probability_p_1_h$Block <- 1
participant_probability_p_1_h$F0 <- "high"
# Print the resulting probability data
print(participant_probability_p_1_h)

participant_probability_p_1_l <- BP_1_l %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
participant_probability_p_1_l$Block <- 1
participant_probability_p_1_l$F0 <- "low"
# Print the resulting probability data
print(participant_probability_p_1_l)

#identify outliers for this Block 1-l
upper_p_1_l = mean(participant_probability_p_1_l$Probability) + 2 * sd(participant_probability_p_1_l$Probability)
lower_p_1_l = mean(participant_probability_p_1_l$Probability) - 2 * sd(participant_probability_p_1_l$Probability)
upper_p_1_l
lower_p_1_l
#64870cb78577c3486ad12c32 is the outlier participant

# Filter the data for Block starting with 2
BP_2_h <- BP_clean %>%
  filter(str_starts(Block, "2"), F0 == "high")
overall_probability_p_2_h <- mean(BP_2_h$Response == 1)
overall_probability_p_2_h

BP_2_l <- BP_clean %>%
  filter(str_starts(Block, "2"), F0 == "low")
overall_probability_p_2_l <- mean(BP_2_l$Response == 1)
overall_probability_p_2_l

# Group the data by Participants and calculate the probability of Response being 1
participant_probability_p_2_h <- BP_2_h %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
participant_probability_p_2_h$Block <- 2
participant_probability_p_2_h$F0 <- "high"
# Print the resulting probability data
print(participant_probability_p_2_h)

participant_probability_p_2_l <- BP_2_l %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
participant_probability_p_2_l$Block <- 2
participant_probability_p_2_l$F0 <- "low"
# Print the resulting probability data
print(participant_probability_p_2_l)

#identify outliers for this Block 1-l
mean(participant_probability_p_2_l$Probability)
upper_p_2_l = mean(participant_probability_p_2_l$Probability) + 2 * sd(participant_probability_p_2_l$Probability)
lower_p_2_l = mean(participant_probability_p_2_l$Probability) - 2 * sd(participant_probability_p_2_l$Probability)
upper_p_2_l
lower_p_2_l

# Filter the data for Block starting with 3
BP_3_h <- BP_clean %>%
  filter(str_starts(Block, "3"), F0 == "high")
overall_probability_p_3_h <- mean(BP_3_h$Response == 1)
overall_probability_p_3_h

BP_3_l <- BP_clean %>%
  filter(str_starts(Block, "3"), F0 == "low")
overall_probability_p_3_l <- mean(BP_3_l$Response == 1)
overall_probability_p_3_l

# Group the data by Participants and calculate the probability of Response being 1
participant_probability_p_3_h <- BP_3_h %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
participant_probability_p_3_h$Block <- 3
participant_probability_p_3_h$F0 <- "high"
# Print the resulting probability data
print(participant_probability_p_3_h)


participant_probability_p_3_l <- BP_3_l %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
participant_probability_p_3_l$Block <- 3
participant_probability_p_3_l$F0 <- "low"
# Print the resulting probability data
print(participant_probability_p_3_l)

# add the probability of [p] responses for h and l F0 together in one table
Prob_p_h <- rbind(participant_probability_p_1_h, participant_probability_p_2_h, participant_probability_p_3_h)
Prob_p_h
Prob_p_l <- rbind(participant_probability_p_1_l, participant_probability_p_2_l, participant_probability_p_3_l)
Prob_p_l
Prob_p <- rbind(Prob_p_h, Prob_p_l)
Prob_p
print(Prob_p, n=Inf)


#RM ANOVA for the probability of p response
anova_resultProb_p <- aov_ez(data = Prob_p, 
                         id = "Participants",
                         dv = "Probability",
                         within = c("Block", "F0"),
                         detailed = TRUE)


anova_resultProb_p
summary_main_p <- summary(anova_resultProb_p)

#pair-wise sample t test
pw_Prob_p_Block <- pairwise.t.test(Prob_p$Probability, Prob_p$Block, paired = TRUE, p.adjust.method = "bonferroni")
pw_Prob_p_Block

pw_Prob_p_F0 <- pairwise.t.test(Prob_p$Probability, Prob_p$F0, paired = TRUE, p.adjust.method = "bonferroni")
pw_Prob_p_F0
# Very significant effect for F0, as expected.

# Which of the two F0 levels are impacted by Blocks?
# 2,3
subset_p_data23 <- subset(Prob_p, Block %in% c(2, 3))
print (subset_p_data23, n=Inf)
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
subset_data_p_h_23 <- subset_p_data23[subset_p_data23$F0 == "high", ]
t_test_high23 <- t.test(subset_data_p_h_23$Probability ~ subset_data_p_h_23$Block, paired = TRUE)
t_test_high23

subset_data_p_l_23 <- subset_p_data23[subset_p_data23$F0 == "low", ]
t_test_low23 <- t.test(subset_data_p_l_23$Probability ~ subset_data_p_l_23$Block, paired = TRUE)
t_test_low23


#1,2
subset_data12 <- subset(Prob_p, Block %in% c(1, 2))
print (subset_data12, n=Inf)
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
subset_data_p_h_12 <- subset_data12[subset_data12$F0 == "high", ]
t_test_high12 <- t.test(subset_data_p_h_12$Probability ~ subset_data_p_h_12$Block, paired = TRUE)
t_test_high12

subset_data_p_l_12 <- subset_data12[subset_data12$F0 == "low", ]
t_test_low12 <- t.test(subset_data_p_l$Probability ~ subset_data_p_l$Block, paired = TRUE)
t_test_low12
summary(t_test_low12)

#1,3
subset_data13 <- subset(Prob_p, Block %in% c(1, 3))
print (subset_data13, n=Inf)
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
subset_data_p_h_13 <- subset_data13[subset_data13$F0 == "high", ]
t_test_high13 <- t.test(subset_data_p_h_13$Probability ~ subset_data_p_h_13$Block, paired = TRUE)
t_test_high13

subset_data_p_l_13 <- subset_data13[subset_data13$F0 == "low", ]
t_test_low13 <- t.test(subset_data_p_l_13$Probability ~ subset_data_p_l_13$Block, paired = TRUE)
t_test_low13



#visualization for high and low F0s.
High_p <- Prob_p[Prob_p$F0 == "high", ]
High_block1_mean <- mean(High_p$Probability[High_p$Block == 1])
High_block2_mean <- mean(High_p$Probability[High_p$Block == 2])
High_block3_mean <- mean(High_p$Probability[High_p$Block == 3])
High_block1_mean
High_block2_mean
High_block3_mean

Low_p <- Prob_p[Prob_p$F0 == "low", ]
Low_block1_mean <- mean(Low_p$Probability[Low_p$Block == 1])
Low_block2_mean <- mean(Low_p$Probability[Low_p$Block == 2])
Low_block3_mean <- mean(Low_p$Probability[Low_p$Block == 3])
Low_block1_mean
Low_block2_mean
Low_block3_mean

###########################################
DT_clean <- DT %>%
  filter(Participants != "64872c98214642b52e4eb12d" &
           Participants != "64870cd2e9ea8c9e102c52be" &
           Participants != "64870c0ddfe4f0fad2e69bad" &
           Participants != "648706e955af62d92fd5ea14" &
           Participants != "64870cb78577c3486ad12c32")

# View the filtered data
DT_clean

# Filter the data for Block starting with 1
DT_1_h <- DT_clean %>%
  filter(str_starts(Block, "1"), F0 == "high")
overall_probability_t_1_h <- mean(DT_1_h$Response == 1)
overall_probability_t_1_h

DT_1_l <- DT_clean %>%
  filter(str_starts(Block, "1"), F0 == "low")
overall_probability_t_1_l <- mean(DT_1_l$Response == 1)
overall_probability_t_1_l

# Group the data by Participants and calculate the probability of Response being 1
participant_probability_t_1_h <- DT_1_h %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
participant_probability_t_1_h$Block <- 1
participant_probability_t_1_h$F0 <- "high"
# Print the resulting probability data
print(participant_probability_t_1_h)

#calculate 2 sd and mean for high and low in block 1
upper_t_1_h = mean(participant_probability_t_1_h$Probability) + 2 * sd(participant_probability_t_1_h$Probability)
lower_t_1_h = mean(participant_probability_t_1_h$Probability) - 2 * sd(participant_probability_t_1_h$Probability)
upper_t_1_h
lower_t_1_h
#648706e955af62d92fd5ea14 is an outlier.

participant_probability_t_1_l <- DT_1_l %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
participant_probability_t_1_l$Block <- 1
participant_probability_t_1_l$F0 <- "low"
# Print the resulting probability data
print(participant_probability_t_1_l)

upper_t_1_l = mean(participant_probability_t_1_l$Probability) + 2 * sd(participant_probability_t_1_l$Probability)
lower_t_1_l = mean(participant_probability_t_1_l$Probability) - 2 * sd(participant_probability_t_1_l$Probability)
upper_t_1_l
lower_t_1_l
# 64870c0ddfe4f0fad2e69bad is an outlier.

# Filter the data for Block starting with 2
DT_2_h <- DT_clean %>%
  filter(str_starts(Block, "2"), F0 == "high")
overall_probability_t_2_h <- mean(DT_2_h$Response == 1)
overall_probability_t_2_h

DT_2_l <- DT_clean %>%
  filter(str_starts(Block, "2"), F0 == "low")
overall_probability_t_2_l <- mean(DT_2_l$Response == 1)
overall_probability_t_2_l

# Group the data by Participants and calculate the probability of Response being 1
participant_probability_t_2_h <- DT_2_h %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
participant_probability_t_2_h$Block <- 2
participant_probability_t_2_h$F0 <- "high"
# Print the resulting probability data
print(participant_probability_t_2_h)

upper_t_2_h = mean(participant_probability_t_2_h$Probability) + 2 * sd(participant_probability_t_2_h$Probability)
lower_t_2_h = mean(participant_probability_t_2_h$Probability) - 2 * sd(participant_probability_t_2_h$Probability)
upper_t_2_h
lower_t_2_h
# 64872c98214642b52e4eb12d and 64870cd2e9ea8c9e102c52be are outliers in the data.

participant_probability_t_2_l <- DT_2_l %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
participant_probability_t_2_l$Block <- 2
participant_probability_t_2_l$F0 <- "low"
# Print the resulting probability data
print(participant_probability_t_2_l)

# Filter the data for Block starting with 3
DT_3_h <- DT_clean %>%
  filter(str_starts(Block, "3"), F0 == "high")
overall_probability_t_3_h <- mean(DT_3_h$Response == 1)
overall_probability_t_3_h

DT_3_l <- DT_clean %>%
  filter(str_starts(Block, "3"), F0 == "low")
overall_probability_t_3_l <- mean(DT_3_l$Response == 1)
overall_probability_t_3_l

# Group the data by Participants and calculate the probability of Response being 1
participant_probability_t_3_h <- DT_3_h %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
participant_probability_t_3_h$Block <- 3
participant_probability_t_3_h$F0 <- "high"
# Print the resulting probability data
print(participant_probability_t_3_h)

participant_probability_t_3_l <- DT_3_l %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
participant_probability_t_3_l$Block <- 3
participant_probability_t_3_l$F0 <- "low"
# Print the resulting probability data
print(participant_probability_t_3_l)

#data distribution and identify outliers
bxp_Prob_t <- ggboxplot(
  Prob_t, x = "Block", y = "Probability",
  color = "F0", palette = "jco"
)
bxp_Prob_t
# discover outliers for block 1 high and low, block 2 high.

Prob_t_h <- rbind(participant_probability_t_1_h, participant_probability_t_2_h, participant_probability_t_3_h)
Prob_t_h
Prob_t_l <- rbind(participant_probability_t_1_l, participant_probability_t_2_l, participant_probability_t_3_l)
Prob_t_l
Prob_t <- rbind(Prob_t_h, Prob_t_l)
Prob_t
print(Prob_p, n=Inf)

# Anova for [t] responses
anova_resultProb_t <- aov_ez(data = Prob_t, 
                             id = "Participants",
                             dv = "Probability",
                             within = c("Block", "F0"),
                             detailed = TRUE)


anova_resultProb_t
summary(anova_resultProb_t)
#Post-hoc analysis for both the percentage of [t] sounds
emm_resultsProb_t <- emmeans(anova_resultProb_t, ~ Block * F0)
posthoc_resultsProb_t <- contrast(emm_resultsProb_t, method = "pairwise", adjust = "tukey")
posthoc_resultsProb_t
# Actually, these are not necessary, because no significant effect was found.

#visualization for high and low F0s.
High_t <- Prob_t[Prob_t$F0 == "high", ]
High_block1_mean_t <- mean(High_t$Probability[High_t$Block == 1])
High_block2_mean_t <- mean(High_t$Probability[High_t$Block == 2])
High_block3_mean_t <- mean(High_t$Probability[High_t$Block == 3])
High_block1_mean_t
High_block2_mean_t
High_block3_mean_t

Low_t <- Prob_t[Prob_t$F0 == "low", ]
Low_block1_mean_t <- mean(Low_t$Probability[Low_t$Block == 1])
Low_block2_mean_t <- mean(Low_t$Probability[Low_t$Block == 2])
Low_block3_mean_t <- mean(Low_t$Probability[Low_t$Block == 3])
Low_block1_mean_t
Low_block2_mean_t
Low_block3_mean_t

#t-test
#1,2
subset_t_data12 <- subset(Prob_t, Block %in% c(1, 2))
print (subset_t_data12, n=Inf)
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
subset_data_t_h_12 <- subset_t_data12[subset_t_data12$F0 == "high", ]
t_test_t_high12 <- t.test(subset_data_t_h_12$Probability ~ subset_data_t_h_12$Block, paired = TRUE)
t_test_t_high12

#1,3
subset_t_data13 <- subset(Prob_t, Block %in% c(1, 3))
print (subset_t_data13, n=Inf)
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
subset_data_t_h_13 <- subset_t_data13[subset_t_data13$F0 == "high", ]
t_test_t_high13 <- t.test(subset_data_t_h_13$Probability ~ subset_data_t_h_13$Block, paired = TRUE)
t_test_t_high13

#2,3
subset_t_data23 <- subset(Prob_t, Block %in% c(2, 3))
print (subset_t_data23, n=Inf)
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
subset_data_t_h_23 <- subset_t_data23[subset_t_data23$F0 == "high", ]
t_test_t_high23 <- t.test(subset_data_t_h_23$Probability ~ subset_data_t_h_23$Block, paired = TRUE)
t_test_t_high23

######################################################################
#examine the time course of the development for [p]
BP_clean
#trail 1
time_p_display_1 <- BP_clean[BP_clean$Display %in% c(10, 20, 30), ]
print(time_p_display_1, n=Inf)
# Filter the data for Block starting with 1
time_p_1_h_1 <- time_p_display_1 %>%
  filter(str_starts(Block, "1"), F0 == "high")

time_p_1_l_1 <- time_p_display_1 %>%
  filter(str_starts(Block, "1"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_1_probability_p_1_h <- time_p_1_h_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_1_probability_p_1_h$Block <- 1
time_1_probability_p_1_h$F0 <- "high"
# Print the resulting probability data
print(time_1_probability_p_1_h)

time_1_probability_p_1_l <- time_p_1_l_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_1_probability_p_1_l$Block <- 1
time_1_probability_p_1_l$F0 <- "low"
# Print the resulting probability data
print(time_1_probability_p_1_l)

# Filter the data for Block starting with 2
time_p_1_h_2 <- time_p_display_1 %>%
  filter(str_starts(Block, "2"), F0 == "high")

time_p_1_l_2 <- time_p_display_1 %>%
  filter(str_starts(Block, "2"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_1_probability_p_2_h <- time_p_1_h_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_1_probability_p_2_h$Block <- 2
time_1_probability_p_2_h$F0 <- "high"
# Print the resulting probability data
print(time_1_probability_p_2_h)

time_1_probability_p_2_l <- time_p_1_l_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_1_probability_p_2_l$Block <- 2
time_1_probability_p_2_l$F0 <- "low"
# Print the resulting probability data
print(time_1_probability_p_2_l)


# Filter the data for Block starting with 3
time_p_1_h_3 <- time_p_display_1 %>%
  filter(str_starts(Block, "3"), F0 == "high")

time_p_1_l_3 <- time_p_display_1 %>%
  filter(str_starts(Block, "3"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_1_probability_p_3_h <- time_p_1_h_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_1_probability_p_3_h$Block <- 3
time_1_probability_p_3_h$F0 <- "high"
# Print the resulting probability data
print(time_1_probability_p_3_h)

time_1_probability_p_3_l <- time_p_1_l_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_1_probability_p_3_l$Block <- 3
time_1_probability_p_3_l$F0 <- "low"
# Print the resulting probability data
print(time_1_probability_p_3_l)

# add the probability of [p] responses for h and l F0 together in one table
Prob_p_h_1 <- rbind(time_1_probability_p_1_h, time_1_probability_p_2_h, time_1_probability_p_3_h)
Prob_p_h_1
Prob_p_l_1 <- rbind(time_1_probability_p_1_l, time_1_probability_p_2_l, time_1_probability_p_3_l)
Prob_p_l_1
Prob_p_1 <- rbind(Prob_p_h_1, Prob_p_l_1)
Prob_p_1
print(Prob_p_1, n=Inf)

anova_resultProb_p_1 <- aov_ez(data = Prob_p_1, 
                             id = "Participants",
                             dv = "Probability",
                             within = c("Block", "F0"),
                             detailed = TRUE)


anova_resultProb_p_1
####################
#trail 1+2
time_p_display_2 <- BP_clean[BP_clean$Display %in% c(10, 11, 20, 21, 30, 31), ]
print(time_p_display_2, n=Inf)


# Filter the data for Block starting with 1
time_p_2_h_1 <- time_p_display_2 %>%
  filter(str_starts(Block, "1"), F0 == "high")
time_p_2_h_1

time_p_2_l_1 <- time_p_display_2 %>%
  filter(str_starts(Block, "1"), F0 == "low")
time_p_2_l_1

# Group the data by Participants and calculate the probability of Response being 1
time_2_probability_p_1_h <- time_p_2_h_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_2_probability_p_1_h$Block <- 1
time_2_probability_p_1_h$F0 <- "high"
# Print the resulting probability data
print(time_2_probability_p_1_h)

time_2_probability_p_1_l <- time_p_2_l_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_2_probability_p_1_l$Block <- 1
time_2_probability_p_1_l$F0 <- "low"
# Print the resulting probability data
print(time_2_probability_p_1_l)

# Filter the data for Block starting with 2
time_p_2_h_2 <- time_p_display_2 %>%
  filter(str_starts(Block, "2"), F0 == "high")

time_p_2_l_2 <- time_p_display_2 %>%
  filter(str_starts(Block, "2"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_2_probability_p_2_h <- time_p_2_h_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_2_probability_p_2_h$Block <- 2
time_2_probability_p_2_h$F0 <- "high"
# Print the resulting probability data
print(time_2_probability_p_2_h)

time_2_probability_p_2_l <- time_p_2_l_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_2_probability_p_2_l$Block <- 2
time_2_probability_p_2_l$F0 <- "low"
# Print the resulting probability data
print(time_2_probability_p_2_l)


# Filter the data for Block starting with 3
time_p_2_h_3 <- time_p_display_2 %>%
  filter(str_starts(Block, "3"), F0 == "high")

time_p_2_l_3 <- time_p_display_2 %>%
  filter(str_starts(Block, "3"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_2_probability_p_3_h <- time_p_2_h_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_2_probability_p_3_h$Block <- 3
time_2_probability_p_3_h$F0 <- "high"
# Print the resulting probability data
print(time_2_probability_p_3_h)

time_2_probability_p_3_l <- time_p_2_l_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_2_probability_p_3_l$Block <- 3
time_2_probability_p_3_l$F0 <- "low"
# Print the resulting probability data
print(time_2_probability_p_3_l)

# add the probability of [p] responses for h and l F0 together in one table
Prob_p_h_2 <- rbind(time_2_probability_p_1_h, time_2_probability_p_2_h, time_2_probability_p_3_h)
Prob_p_h_2
Prob_p_l_2 <- rbind(time_2_probability_p_1_l, time_2_probability_p_2_l, time_2_probability_p_3_l)
Prob_p_l_2
Prob_p_2 <- rbind(Prob_p_h_2, Prob_p_l_2)
Prob_p_2
print(Prob_p_2, n=Inf)

anova_resultProb_p_2 <- aov_ez(data = Prob_p_2, 
                               id = "Participants",
                               dv = "Probability",
                               within = c("Block", "F0"),
                               detailed = TRUE)


anova_resultProb_p_2
#found two-way interaction between Block and F0: next, I will do post-hoc analysis

# Which of the two F0 levels are impacted by Blocks?

# Filter the data for Block 1 and Block 3

time_subset_2_p_13 <- subset(Prob_p_2, Block %in% c(1, 3))
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
time_subset_2_p_13_h <- time_subset_2_p_13[time_subset_2_p_13$F0 == "high", ]
t_test_time_subset_2_p_13_high <- t.test(time_subset_2_p_13_h$Probability ~ time_subset_2_p_13_h$Block, paired = TRUE)
t_test_time_subset_2_p_13_high

time_subset_2_p_13_l <- time_subset_2_p_13[time_subset_2_p_13$F0 == "low", ]
t_test_time_subset_2_p_13_low <- t.test(time_subset_2_p_13_l$Probability ~ time_subset_2_p_13_l$Block, paired = TRUE)
t_test_time_subset_2_p_13_low

# Filter the data for Block 2 and Block 3

time_subset_2_p_23 <- subset(Prob_p_2, Block %in% c(2, 3))
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
time_subset_2_p_23_h <- time_subset_2_p_23[time_subset_2_p_23$F0 == "high", ]
t_test_time_subset_2_p_23_high <- t.test(time_subset_2_p_23_h$Probability ~ time_subset_2_p_23_h$Block, paired = TRUE)
t_test_time_subset_2_p_23_high

time_subset_2_p_23_l <- time_subset_2_p_23[time_subset_2_p_23$F0 == "low", ]
t_test_time_subset_2_p_23_low <- t.test(time_subset_2_p_23_l$Probability ~ time_subset_2_p_23_l$Block, paired = TRUE)
t_test_time_subset_2_p_23_low

#draw boxplot
bxp_Prob_t_time2 <- ggboxplot(
  Prob_p_2, x = "Block", y = "Probability",
  color = "F0", palette = "jco"
)
bxp_Prob_t_time2

#in time series 1 and 2, we find an interaction between block and f0, and the effect is on low F0. It seems that after the first 10 trails of training at least, 
#participants were more likely to assign [p] to a low-F0 sound file.

###trail 1+2+3
time_p_display_3 <- BP_clean[BP_clean$Display %in% c(10, 11, 12, 20, 21, 22, 30, 31, 32), ]
print(time_p_display_3, n=Inf)


# Filter the data for Block starting with 1
time_p_3_h_1 <- time_p_display_3 %>%
  filter(str_starts(Block, "1"), F0 == "high")
time_p_3_h_1

time_p_3_l_1 <- time_p_display_3 %>%
  filter(str_starts(Block, "1"), F0 == "low")
time_p_3_l_1

# Group the data by Participants and calculate the probability of Response being 1
time_3_probability_p_1_h <- time_p_3_h_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_3_probability_p_1_h$Block <- 1
time_3_probability_p_1_h$F0 <- "high"
# Print the resulting probability data
print(time_3_probability_p_1_h)

time_3_probability_p_1_l <- time_p_3_l_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_3_probability_p_1_l$Block <- 1
time_3_probability_p_1_l$F0 <- "low"
# Print the resulting probability data
print(time_3_probability_p_1_l)

# Filter the data for Block starting with 2
time_p_3_h_2 <- time_p_display_3 %>%
  filter(str_starts(Block, "2"), F0 == "high")

time_p_3_l_2 <- time_p_display_3 %>%
  filter(str_starts(Block, "2"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_3_probability_p_2_h <- time_p_3_h_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_3_probability_p_2_h$Block <- 2
time_3_probability_p_2_h$F0 <- "high"
# Print the resulting probability data
print(time_3_probability_p_2_h)

time_3_probability_p_2_l <- time_p_3_l_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_3_probability_p_2_l$Block <- 2
time_3_probability_p_2_l$F0 <- "low"
# Print the resulting probability data
print(time_3_probability_p_2_l)


# Filter the data for Block starting with 3
time_p_3_h_3 <- time_p_display_3 %>%
  filter(str_starts(Block, "3"), F0 == "high")

time_p_3_l_3 <- time_p_display_3 %>%
  filter(str_starts(Block, "3"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_3_probability_p_3_h <- time_p_3_h_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_3_probability_p_3_h$Block <- 3
time_3_probability_p_3_h$F0 <- "high"
# Print the resulting probability data
print(time_3_probability_p_3_h)

time_3_probability_p_3_l <- time_p_3_l_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_3_probability_p_3_l$Block <- 3
time_3_probability_p_3_l$F0 <- "low"
# Print the resulting probability data
print(time_3_probability_p_3_l)

# add the probability of [p] responses for h and l F0 together in one table
Prob_p_h_3 <- rbind(time_3_probability_p_1_h, time_3_probability_p_2_h, time_3_probability_p_3_h)
Prob_p_h_3
Prob_p_l_3 <- rbind(time_3_probability_p_1_l, time_3_probability_p_2_l, time_3_probability_p_3_l)
Prob_p_l_3
Prob_p_3 <- rbind(Prob_p_h_3, Prob_p_l_3)
Prob_p_3
print(Prob_p_3, n=Inf)

anova_resultProb_p_3 <- aov_ez(data = Prob_p_3, 
                               id = "Participants",
                               dv = "Probability",
                               within = c("Block", "F0"),
                               detailed = TRUE)


anova_resultProb_p_3

#found interaction again
# Filter the data for Block 1 and Block 3

time_subset_3_p_13 <- subset(Prob_p_3, Block %in% c(1, 3))
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
time_subset_3_p_13_h <- time_subset_3_p_13[time_subset_3_p_13$F0 == "high", ]
t_test_time_subset_3_p_13_high <- t.test(time_subset_3_p_13_h$Probability ~ time_subset_3_p_13_h$Block, paired = TRUE)
t_test_time_subset_3_p_13_high

time_subset_3_p_13_l <- time_subset_3_p_13[time_subset_3_p_13$F0 == "low", ]
t_test_time_subset_3_p_13_low <- t.test(time_subset_3_p_13_l$Probability ~ time_subset_3_p_13_l$Block, paired = TRUE)
t_test_time_subset_3_p_13_low

# Filter the data for Block 2 and Block 3

time_subset_3_p_23 <- subset(Prob_p_3, Block %in% c(2, 3))
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
time_subset_3_p_23_h <- time_subset_3_p_23[time_subset_3_p_23$F0 == "high", ]
t_test_time_subset_3_p_23_high <- t.test(time_subset_3_p_23_h$Probability ~ time_subset_3_p_23_h$Block, paired = TRUE)
t_test_time_subset_3_p_23_high

time_subset_3_p_23_l <- time_subset_3_p_23[time_subset_3_p_23$F0 == "low", ]
t_test_time_subset_3_p_23_low <- t.test(time_subset_3_p_23_l$Probability ~ time_subset_3_p_23_l$Block, paired = TRUE)
t_test_time_subset_3_p_23_low
# again, found effect between Block 1 and 3, 2 and 3, on low F0 only

###trail1+2+3+4
time_p_display_4 <- BP_clean[BP_clean$Display %in% c(10, 11, 12, 13, 20, 21, 22, 23, 30, 31, 32, 33), ]
print(time_p_display_4, n=Inf)


# Filter the data for Block starting with 1
time_p_4_h_1 <- time_p_display_4 %>%
  filter(str_starts(Block, "1"), F0 == "high")
time_p_4_h_1

time_p_4_l_1 <- time_p_display_4 %>%
  filter(str_starts(Block, "1"), F0 == "low")
time_p_4_l_1

# Group the data by Participants and calculate the probability of Response being 1
time_4_probability_p_1_h <- time_p_4_h_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_4_probability_p_1_h$Block <- 1
time_4_probability_p_1_h$F0 <- "high"
# Print the resulting probability data
print(time_4_probability_p_1_h)

time_4_probability_p_1_l <- time_p_4_l_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_4_probability_p_1_l$Block <- 1
time_4_probability_p_1_l$F0 <- "low"
# Print the resulting probability data
print(time_4_probability_p_1_l)

# Filter the data for Block starting with 2
time_p_4_h_2 <- time_p_display_4 %>%
  filter(str_starts(Block, "2"), F0 == "high")

time_p_4_l_2 <- time_p_display_4 %>%
  filter(str_starts(Block, "2"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_4_probability_p_2_h <- time_p_4_h_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_4_probability_p_2_h$Block <- 2
time_4_probability_p_2_h$F0 <- "high"
# Print the resulting probability data
print(time_4_probability_p_2_h)

time_4_probability_p_2_l <- time_p_4_l_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_4_probability_p_2_l$Block <- 2
time_4_probability_p_2_l$F0 <- "low"
# Print the resulting probability data
print(time_4_probability_p_2_l)


# Filter the data for Block starting with 3
time_p_4_h_3 <- time_p_display_4 %>%
  filter(str_starts(Block, "3"), F0 == "high")

time_p_4_l_3 <- time_p_display_4 %>%
  filter(str_starts(Block, "3"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_4_probability_p_3_h <- time_p_4_h_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_4_probability_p_3_h$Block <- 3
time_4_probability_p_3_h$F0 <- "high"
# Print the resulting probability data
print(time_4_probability_p_3_h)

time_4_probability_p_3_l <- time_p_4_l_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_4_probability_p_3_l$Block <- 3
time_4_probability_p_3_l$F0 <- "low"
# Print the resulting probability data
print(time_4_probability_p_3_l)

# add the probability of [p] responses for h and l F0 together in one table
Prob_p_h_4 <- rbind(time_4_probability_p_1_h, time_4_probability_p_2_h, time_4_probability_p_3_h)
Prob_p_h_4
Prob_p_l_4 <- rbind(time_4_probability_p_1_l, time_4_probability_p_2_l, time_4_probability_p_3_l)
Prob_p_l_4
Prob_p_4 <- rbind(Prob_p_h_4, Prob_p_l_4)
Prob_p_4
print(Prob_p_4, n=Inf)

anova_resultProb_p_4 <- aov_ez(data = Prob_p_4, 
                               id = "Participants",
                               dv = "Probability",
                               within = c("Block", "F0"),
                               detailed = TRUE)


anova_resultProb_p_4
#did not find interaction, but the p value for interaction is very close to 0.05, so still do it.
# Filter the data for Block 1 and Block 3

time_subset_4_p_13 <- subset(Prob_p_4, Block %in% c(1, 3))
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
time_subset_4_p_13_h <- time_subset_4_p_13[time_subset_4_p_13$F0 == "high", ]
t_test_time_subset_4_p_13_high <- t.test(time_subset_4_p_13_h$Probability ~ time_subset_4_p_13_h$Block, paired = TRUE)
t_test_time_subset_4_p_13_high

time_subset_4_p_13_l <- time_subset_4_p_13[time_subset_4_p_13$F0 == "low", ]
t_test_time_subset_4_p_13_low <- t.test(time_subset_4_p_13_l$Probability ~ time_subset_4_p_13_l$Block, paired = TRUE)
t_test_time_subset_4_p_13_low

# Filter the data for Block 2 and Block 3

time_subset_4_p_23 <- subset(Prob_p_4, Block %in% c(2, 3))
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
time_subset_4_p_23_h <- time_subset_4_p_23[time_subset_4_p_23$F0 == "high", ]
t_test_time_subset_4_p_23_high <- t.test(time_subset_4_p_23_h$Probability ~ time_subset_4_p_23_h$Block, paired = TRUE)
t_test_time_subset_4_p_23_high

time_subset_4_p_23_l <- time_subset_4_p_23[time_subset_4_p_23$F0 == "low", ]
t_test_time_subset_4_p_23_low <- t.test(time_subset_4_p_23_l$Probability ~ time_subset_4_p_23_l$Block, paired = TRUE)
t_test_time_subset_4_p_23_low
# again, found effect between Block 1 and 3, 2 and 3, on low F0 only, but the p value has inflated

####trails1+2+3+4+5
time_p_display_5 <- BP_clean[BP_clean$Display %in% c(10, 11, 12, 13, 14, 20, 21, 22, 23, 24, 30, 31, 32, 33, 34), ]
print(time_p_display_5, n=Inf)


# Filter the data for Block starting with 1
time_p_5_h_1 <- time_p_display_5 %>%
  filter(str_starts(Block, "1"), F0 == "high")
time_p_5_h_1

time_p_5_l_1 <- time_p_display_5 %>%
  filter(str_starts(Block, "1"), F0 == "low")
time_p_5_l_1

# Group the data by Participants and calculate the probability of Response being 1
time_5_probability_p_1_h <- time_p_5_h_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_5_probability_p_1_h$Block <- 1
time_5_probability_p_1_h$F0 <- "high"
# Print the resulting probability data
print(time_5_probability_p_1_h)

time_5_probability_p_1_l <- time_p_5_l_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_5_probability_p_1_l$Block <- 1
time_5_probability_p_1_l$F0 <- "low"
# Print the resulting probability data
print(time_5_probability_p_1_l)

# Filter the data for Block starting with 2
time_p_5_h_2 <- time_p_display_5 %>%
  filter(str_starts(Block, "2"), F0 == "high")

time_p_5_l_2 <- time_p_display_5 %>%
  filter(str_starts(Block, "2"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_5_probability_p_2_h <- time_p_5_h_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_5_probability_p_2_h$Block <- 2
time_5_probability_p_2_h$F0 <- "high"
# Print the resulting probability data
print(time_5_probability_p_2_h)

time_5_probability_p_2_l <- time_p_5_l_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_5_probability_p_2_l$Block <- 2
time_5_probability_p_2_l$F0 <- "low"
# Print the resulting probability data
print(time_5_probability_p_2_l)


# Filter the data for Block starting with 3
time_p_5_h_3 <- time_p_display_5 %>%
  filter(str_starts(Block, "3"), F0 == "high")

time_p_5_l_3 <- time_p_display_5 %>%
  filter(str_starts(Block, "3"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_5_probability_p_3_h <- time_p_5_h_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_5_probability_p_3_h$Block <- 3
time_5_probability_p_3_h$F0 <- "high"
# Print the resulting probability data
print(time_5_probability_p_3_h)

time_5_probability_p_3_l <- time_p_5_l_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_5_probability_p_3_l$Block <- 3
time_5_probability_p_3_l$F0 <- "low"
# Print the resulting probability data
print(time_5_probability_p_3_l)

# add the probability of [p] responses for h and l F0 together in one table
Prob_p_h_5 <- rbind(time_5_probability_p_1_h, time_5_probability_p_2_h, time_5_probability_p_3_h)
Prob_p_h_5
Prob_p_l_5 <- rbind(time_5_probability_p_1_l, time_5_probability_p_2_l, time_5_probability_p_3_l)
Prob_p_l_5
Prob_p_5 <- rbind(Prob_p_h_5, Prob_p_l_5)
Prob_p_5
print(Prob_p_5, n=Inf)

anova_resultProb_p_5 <- aov_ez(data = Prob_p_5, 
                               id = "Participants",
                               dv = "Probability",
                               within = c("Block", "F0"),
                               detailed = TRUE)


anova_resultProb_p_5
#no interaction effect, and p value is way beyond the alpha level

# Filter the data for Block 1 and Block 2

time_subset_5_p_12 <- subset(Prob_p_5, Block %in% c(1, 2))
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
time_subset_5_p_12_h <- time_subset_5_p_12[time_subset_5_p_12$F0 == "high", ]
t_test_time_subset_5_p_12_high <- t.test(time_subset_5_p_12_h$Probability ~ time_subset_5_p_12_h$Block, paired = TRUE)
t_test_time_subset_5_p_12_high

time_subset_5_p_12_l <- time_subset_5_p_12[time_subset_5_p_12$F0 == "low", ]
t_test_time_subset_5_p_12_low <- t.test(time_subset_5_p_12_l$Probability ~ time_subset_5_p_12_l$Block, paired = TRUE)
t_test_time_subset_5_p_12_low

# Filter the data for Block 1 and Block 3

time_subset_5_p_13 <- subset(Prob_p_5, Block %in% c(1, 3))
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
time_subset_5_p_13_h <- time_subset_5_p_13[time_subset_5_p_13$F0 == "high", ]
t_test_time_subset_5_p_13_high <- t.test(time_subset_5_p_13_h$Probability ~ time_subset_5_p_13_h$Block, paired = TRUE)
t_test_time_subset_5_p_13_high

time_subset_5_p_13_l <- time_subset_5_p_13[time_subset_5_p_13$F0 == "low", ]
t_test_time_subset_5_p_13_low <- t.test(time_subset_5_p_13_l$Probability ~ time_subset_5_p_13_l$Block, paired = TRUE)
t_test_time_subset_5_p_13_low

# Filter the data for Block 2 and Block 3

time_subset_5_p_23 <- subset(Prob_p_5, Block %in% c(2, 3))
# Perform separate paired t-tests for each level of F0 for block 2 and 3.
time_subset_5_p_23_h <- time_subset_5_p_23[time_subset_5_p_23$F0 == "high", ]
t_test_time_subset_5_p_23_high <- t.test(time_subset_5_p_23_h$Probability ~ time_subset_5_p_23_h$Block, paired = TRUE)
t_test_time_subset_5_p_23_high

time_subset_5_p_23_l <- time_subset_5_p_23[time_subset_5_p_23$F0 == "low", ]
t_test_time_subset_5_p_23_low <- t.test(time_subset_5_p_23_l$Probability ~ time_subset_5_p_23_l$Block, paired = TRUE)
t_test_time_subset_5_p_23_low
#This time, no interaction between 2 and 3, but still between Block 1 and 3, still only on low F0.


########################################################
#examine the time course for [t]
DT_clean
#trail 1
time_t_display_1 <- DT_clean[DT_clean$Display %in% c(10, 20, 30), ]
print(time_t_display_1, n=Inf)
# Filter the data for Block starting with 1
time_t_1_h_1 <- time_t_display_1 %>%
  filter(str_starts(Block, "1"), F0 == "high")

time_t_1_l_1 <- time_t_display_1 %>%
  filter(str_starts(Block, "1"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_1_probability_t_1_h <- time_t_1_h_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_1_probability_t_1_h$Block <- 1
time_1_probability_t_1_h$F0 <- "high"
# Print the resulting probability data
print(time_1_probability_t_1_h)

time_1_probability_t_1_l <- time_t_1_l_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_1_probability_t_1_l$Block <- 1
time_1_probability_t_1_l$F0 <- "low"
# Print the resulting probability data
print(time_1_probability_t_1_l)

# Filter the data for Block starting with 2
time_t_1_h_2 <- time_t_display_1 %>%
  filter(str_starts(Block, "2"), F0 == "high")

time_t_1_l_2 <- time_t_display_1 %>%
  filter(str_starts(Block, "2"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_1_probability_t_2_h <- time_t_1_h_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_1_probability_t_2_h$Block <- 2
time_1_probability_t_2_h$F0 <- "high"
# Print the resulting probability data
print(time_1_probability_t_2_h)

time_1_probability_t_2_l <- time_t_1_l_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_1_probability_t_2_l$Block <- 2
time_1_probability_t_2_l$F0 <- "low"
# Print the resulting probability data
print(time_1_probability_t_2_l)


# Filter the data for Block starting with 3
time_t_1_h_3 <- time_t_display_1 %>%
  filter(str_starts(Block, "3"), F0 == "high")

time_t_1_l_3 <- time_t_display_1 %>%
  filter(str_starts(Block, "3"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_1_probability_t_3_h <- time_t_1_h_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_1_probability_t_3_h$Block <- 3
time_1_probability_t_3_h$F0 <- "high"
# Print the resulting probability data
print(time_1_probability_t_3_h)

time_1_probability_t_3_l <- time_t_1_l_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_1_probability_t_3_l$Block <- 3
time_1_probability_t_3_l$F0 <- "low"
# Print the resulting probability data
print(time_1_probability_t_3_l)

# add the probability of [p] responses for h and l F0 together in one table
Prob_t_h_1 <- rbind(time_1_probability_t_1_h, time_1_probability_t_2_h, time_1_probability_t_3_h)
Prob_t_h_1
Prob_t_l_1 <- rbind(time_1_probability_t_1_l, time_1_probability_t_2_l, time_1_probability_t_3_l)
Prob_t_l_1
Prob_t_1 <- rbind(Prob_t_h_1, Prob_t_l_1)
Prob_t_1
print(Prob_t_1, n=Inf)

anova_resultProb_t_1 <- aov_ez(data = Prob_t_1, 
                               id = "Participants",
                               dv = "Probability",
                               within = c("Block", "F0"),
                               detailed = TRUE)


anova_resultProb_t_1


####################
#trail 1+2
time_t_display_2 <- DT_clean[DT_clean$Display %in% c(10, 11, 20, 21, 30, 31), ]
print(time_t_display_2, n=Inf)


# Filter the data for Block starting with 1
time_t_2_h_1 <- time_t_display_2 %>%
  filter(str_starts(Block, "1"), F0 == "high")
time_t_2_h_1

time_t_2_l_1 <- time_t_display_2 %>%
  filter(str_starts(Block, "1"), F0 == "low")
time_t_2_l_1

# Group the data by Participants and calculate the probability of Response being 1
time_2_probability_t_1_h <- time_t_2_h_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_2_probability_t_1_h$Block <- 1
time_2_probability_t_1_h$F0 <- "high"
# Print the resulting probability data
print(time_2_probability_t_1_h)

time_2_probability_t_1_l <- time_t_2_l_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_2_probability_t_1_l$Block <- 1
time_2_probability_t_1_l$F0 <- "low"
# Print the resulting probability data
print(time_2_probability_t_1_l)

# Filter the data for Block starting with 2
time_t_2_h_2 <- time_t_display_2 %>%
  filter(str_starts(Block, "2"), F0 == "high")

time_t_2_l_2 <- time_t_display_2 %>%
  filter(str_starts(Block, "2"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_2_probability_t_2_h <- time_t_2_h_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_2_probability_t_2_h$Block <- 2
time_2_probability_t_2_h$F0 <- "high"
# Print the resulting probability data
print(time_2_probability_t_2_h)

time_2_probability_t_2_l <- time_t_2_l_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_2_probability_t_2_l$Block <- 2
time_2_probability_t_2_l$F0 <- "low"
# Print the resulting probability data
print(time_2_probability_t_2_l)


# Filter the data for Block starting with 3
time_t_2_h_3 <- time_t_display_2 %>%
  filter(str_starts(Block, "3"), F0 == "high")

time_t_2_l_3 <- time_t_display_2 %>%
  filter(str_starts(Block, "3"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_2_probability_t_3_h <- time_t_2_h_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_2_probability_t_3_h$Block <- 3
time_2_probability_t_3_h$F0 <- "high"
# Print the resulting probability data
print(time_2_probability_t_3_h)

time_2_probability_t_3_l <- time_t_2_l_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_2_probability_t_3_l$Block <- 3
time_2_probability_t_3_l$F0 <- "low"
# Print the resulting probability data
print(time_2_probability_t_3_l)

# add the probability of [p] responses for h and l F0 together in one table
Prob_t_h_2 <- rbind(time_2_probability_t_1_h, time_2_probability_t_2_h, time_2_probability_t_3_h)
Prob_t_h_2
Prob_t_l_2 <- rbind(time_2_probability_t_1_l, time_2_probability_t_2_l, time_2_probability_t_3_l)
Prob_t_l_2
Prob_t_2 <- rbind(Prob_t_h_2, Prob_t_l_2)
Prob_t_2
print(Prob_t_2, n=Inf)

anova_resultProb_t_2 <- aov_ez(data = Prob_t_2, 
                               id = "Participants",
                               dv = "Probability",
                               within = c("Block", "F0"),
                               detailed = TRUE)


anova_resultProb_t_2


###trail 1+2+3
time_t_display_3 <- DT_clean[DT_clean$Display %in% c(10, 11, 12, 20, 21, 22, 30, 31, 32), ]
print(time_t_display_3, n=Inf)


# Filter the data for Block starting with 1
time_t_3_h_1 <- time_t_display_3 %>%
  filter(str_starts(Block, "1"), F0 == "high")
time_t_3_h_1

time_t_3_l_1 <- time_t_display_3 %>%
  filter(str_starts(Block, "1"), F0 == "low")
time_t_3_l_1

# Group the data by Participants and calculate the probability of Response being 1
time_3_probability_t_1_h <- time_t_3_h_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_3_probability_t_1_h$Block <- 1
time_3_probability_t_1_h$F0 <- "high"
# Print the resulting probability data
print(time_3_probability_t_1_h)

time_3_probability_t_1_l <- time_t_3_l_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_3_probability_t_1_l$Block <- 1
time_3_probability_t_1_l$F0 <- "low"
# Print the resulting probability data
print(time_3_probability_t_1_l)

# Filter the data for Block starting with 2
time_t_3_h_2 <- time_t_display_3 %>%
  filter(str_starts(Block, "2"), F0 == "high")

time_t_3_l_2 <- time_t_display_3 %>%
  filter(str_starts(Block, "2"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_3_probability_t_2_h <- time_t_3_h_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_3_probability_t_2_h$Block <- 2
time_3_probability_t_2_h$F0 <- "high"
# Print the resulting probability data
print(time_3_probability_t_2_h)

time_3_probability_t_2_l <- time_t_3_l_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_3_probability_t_2_l$Block <- 2
time_3_probability_t_2_l$F0 <- "low"
# Print the resulting probability data
print(time_3_probability_t_2_l)


# Filter the data for Block starting with 3
time_t_3_h_3 <- time_t_display_3 %>%
  filter(str_starts(Block, "3"), F0 == "high")

time_t_3_l_3 <- time_t_display_3 %>%
  filter(str_starts(Block, "3"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_3_probability_t_3_h <- time_t_3_h_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_3_probability_t_3_h$Block <- 3
time_3_probability_t_3_h$F0 <- "high"
# Print the resulting probability data
print(time_3_probability_t_3_h)

time_3_probability_t_3_l <- time_t_3_l_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_3_probability_t_3_l$Block <- 3
time_3_probability_t_3_l$F0 <- "low"
# Print the resulting probability data
print(time_3_probability_t_3_l)

# add the probability of [p] responses for h and l F0 together in one table
Prob_t_h_3 <- rbind(time_3_probability_t_1_h, time_3_probability_t_2_h, time_3_probability_t_3_h)
Prob_t_h_3
Prob_t_l_3 <- rbind(time_3_probability_t_1_l, time_3_probability_t_2_l, time_3_probability_t_3_l)
Prob_t_l_3
Prob_t_3 <- rbind(Prob_t_h_3, Prob_t_l_3)
Prob_t_3
print(Prob_t_3, n=Inf)

anova_resultProb_t_3 <- aov_ez(data = Prob_t_3, 
                               id = "Participants",
                               dv = "Probability",
                               within = c("Block", "F0"),
                               detailed = TRUE)


anova_resultProb_t_3


###trail1+2+3+4
time_t_display_4 <- DT_clean[DT_clean$Display %in% c(10, 11, 12, 13, 20, 21, 22, 23, 30, 31, 32, 33), ]
print(time_t_display_4, n=Inf)


# Filter the data for Block starting with 1
time_t_4_h_1 <- time_t_display_4 %>%
  filter(str_starts(Block, "1"), F0 == "high")
time_t_4_h_1

time_t_4_l_1 <- time_t_display_4 %>%
  filter(str_starts(Block, "1"), F0 == "low")
time_t_4_l_1

# Group the data by Participants and calculate the probability of Response being 1
time_4_probability_t_1_h <- time_t_4_h_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_4_probability_t_1_h$Block <- 1
time_4_probability_t_1_h$F0 <- "high"
# Print the resulting probability data
print(time_4_probability_t_1_h)

time_4_probability_t_1_l <- time_t_4_l_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_4_probability_t_1_l$Block <- 1
time_4_probability_t_1_l$F0 <- "low"
# Print the resulting probability data
print(time_4_probability_t_1_l)

# Filter the data for Block starting with 2
time_t_4_h_2 <- time_t_display_4 %>%
  filter(str_starts(Block, "2"), F0 == "high")

time_t_4_l_2 <- time_t_display_4 %>%
  filter(str_starts(Block, "2"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_4_probability_t_2_h <- time_t_4_h_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_4_probability_t_2_h$Block <- 2
time_4_probability_t_2_h$F0 <- "high"
# Print the resulting probability data
print(time_4_probability_t_2_h)

time_4_probability_t_2_l <- time_t_4_l_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_4_probability_t_2_l$Block <- 2
time_4_probability_t_2_l$F0 <- "low"
# Print the resulting probability data
print(time_4_probability_t_2_l)


# Filter the data for Block starting with 3
time_t_4_h_3 <- time_t_display_4 %>%
  filter(str_starts(Block, "3"), F0 == "high")

time_t_4_l_3 <- time_t_display_4 %>%
  filter(str_starts(Block, "3"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_4_probability_t_3_h <- time_t_4_h_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_4_probability_t_3_h$Block <- 3
time_4_probability_t_3_h$F0 <- "high"
# Print the resulting probability data
print(time_4_probability_t_3_h)

time_4_probability_t_3_l <- time_t_4_l_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_4_probability_t_3_l$Block <- 3
time_4_probability_t_3_l$F0 <- "low"
# Print the resulting probability data
print(time_4_probability_t_3_l)

# add the probability of [p] responses for h and l F0 together in one table
Prob_t_h_4 <- rbind(time_4_probability_t_1_h, time_4_probability_t_2_h, time_4_probability_t_3_h)
Prob_t_h_4
Prob_t_l_4 <- rbind(time_4_probability_t_1_l, time_4_probability_t_2_l, time_4_probability_t_3_l)
Prob_t_l_4
Prob_t_4 <- rbind(Prob_t_h_4, Prob_t_l_4)
Prob_t_4
print(Prob_t_4, n=Inf)

anova_resultProb_t_4 <- aov_ez(data = Prob_t_4, 
                               id = "Participants",
                               dv = "Probability",
                               within = c("Block", "F0"),
                               detailed = TRUE)


anova_resultProb_t_4

####trails1+2+3+4+5
time_t_display_5 <- DT_clean[DT_clean$Display %in% c(10, 11, 12, 13, 14, 20, 21, 22, 23, 24, 30, 31, 32, 33, 34), ]
print(time_t_display_5, n=Inf)


# Filter the data for Block starting with 1
time_t_5_h_1 <- time_t_display_5 %>%
  filter(str_starts(Block, "1"), F0 == "high")
time_t_5_h_1

time_t_5_l_1 <- time_t_display_5 %>%
  filter(str_starts(Block, "1"), F0 == "low")
time_t_5_l_1

# Group the data by Participants and calculate the probability of Response being 1
time_5_probability_t_1_h <- time_t_5_h_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_5_probability_t_1_h$Block <- 1
time_5_probability_t_1_h$F0 <- "high"
# Print the resulting probability data
print(time_5_probability_t_1_h)

time_5_probability_t_1_l <- time_t_5_l_1 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_5_probability_t_1_l$Block <- 1
time_5_probability_t_1_l$F0 <- "low"
# Print the resulting probability data
print(time_5_probability_t_1_l)

# Filter the data for Block starting with 2
time_t_5_h_2 <- time_t_display_5 %>%
  filter(str_starts(Block, "2"), F0 == "high")

time_t_5_l_2 <- time_t_display_5 %>%
  filter(str_starts(Block, "2"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_5_probability_t_2_h <- time_t_5_h_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_5_probability_t_2_h$Block <- 2
time_5_probability_t_2_h$F0 <- "high"
# Print the resulting probability data
print(time_5_probability_t_2_h)

time_5_probability_t_2_l <- time_t_5_l_2 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_5_probability_t_2_l$Block <- 2
time_5_probability_t_2_l$F0 <- "low"
# Print the resulting probability data
print(time_5_probability_t_2_l)


# Filter the data for Block starting with 3
time_t_5_h_3 <- time_t_display_5 %>%
  filter(str_starts(Block, "3"), F0 == "high")

time_t_5_l_3 <- time_t_display_5 %>%
  filter(str_starts(Block, "3"), F0 == "low")

# Group the data by Participants and calculate the probability of Response being 1
time_5_probability_t_3_h <- time_t_5_h_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_5_probability_t_3_h$Block <- 3
time_5_probability_t_3_h$F0 <- "high"
# Print the resulting probability data
print(time_5_probability_t_3_h)

time_5_probability_t_3_l <- time_t_5_l_3 %>%
  group_by(Participants) %>%
  summarise(Probability = mean(Response == 1))
time_5_probability_t_3_l$Block <- 3
time_5_probability_t_3_l$F0 <- "low"
# Print the resulting probability data
print(time_5_probability_t_3_l)

# add the probability of [p] responses for h and l F0 together in one table
Prob_t_h_5 <- rbind(time_5_probability_t_1_h, time_5_probability_t_2_h, time_5_probability_t_3_h)
Prob_t_h_5
Prob_t_l_5 <- rbind(time_5_probability_t_1_l, time_5_probability_t_2_l, time_5_probability_t_3_l)
Prob_t_l_5
Prob_t_5 <- rbind(Prob_t_h_5, Prob_t_l_5)
Prob_t_5
print(Prob_t_5, n=Inf)

anova_resultProb_t_5 <- aov_ez(data = Prob_t_5, 
                               id = "Participants",
                               dv = "Probability",
                               within = c("Block", "F0"),
                               detailed = TRUE)


anova_resultProb_t_5


#calculate the mean difference scores for voiceless responses across high versus low F0 stimulus
#for p response
overall_probability_p_1_h
overall_probability_p_1_l
overall_probability_p_2_h
overall_probability_p_2_l
overall_probability_p_3_h
overall_probability_p_3_l

block <- 1:3
p_h <- c(overall_probability_p_1_h, overall_probability_p_2_h, overall_probability_p_3_h)
p_l <- c(overall_probability_p_1_l, overall_probability_p_2_l, overall_probability_p_3_l)

plot(block, p_h, type = "l", ylim = c(0, 1), xlab = "Block", ylab = "Percentage of [p] response", col = "blue", lwd = 2, xaxt = "n")
lines(block, p_l, col = "red", lwd = 2)
axis(1, at = block, labels = block)

legend("right", legend = c("high F0", "low F0"), col = c("blue", "red"), lwd = 2, cex = 0.7)

title("Percentage of [p] Response by Block")

#########for t response
overall_probability_t_1_h
overall_probability_t_1_l
overall_probability_t_2_h
overall_probability_t_2_l
overall_probability_t_3_h
overall_probability_t_3_l

block <- 1:3
t_h <- c(overall_probability_t_1_h, overall_probability_t_2_h, overall_probability_t_3_h)
t_l <- c(overall_probability_t_1_l, overall_probability_t_2_l, overall_probability_t_3_l)

plot(block, t_h, type = "l", ylim = c(0, 1), xlab = "Block", ylab = "Percentage of [p] response", col = "blue", lwd = 2, xaxt = "n")
lines(block, t_l, col = "red", lwd = 2)
axis(1, at = block, labels = block)


legend("right", legend = c("high F0", "low F0"), col = c("blue", "red"), lwd = 2, cex = 0.7)

title("Percentage of [t] Response by Block")


##hypothetical situation for 3-stage hypothesis
Stage <- 1:3
hp1_h <- c(0.7, 0.9, 0.9)
hp1_l <- c(0.3, 0.1, 0.1)

plot(Stage, hp1_h, type = "l", ylim = c(0, 1), xlab = "Stage", ylab = "Percentage of voiceless response", col = "blue", lwd = 2, xaxt = "n")
lines(Stage, hp1_l, col = "red", lwd = 2)
axis(1, at = Stage, labels = Stage)

legend("right", legend = c("high F0", "low F0"), col = c("blue", "red"), lwd = 2, cex = 0.7)

title("Percentage of voiceless response by Stage")


##hypothetical situation for 2-stage hypothesis
Stage <- 1:3
hp2_h <- c(0.7, 0.7, 0.9)
hp2_l <- c(0.3, 0.3, 0.1)

plot(Stage, hp2_h, type = "l", ylim = c(0, 1), xlab = "Stage", ylab = "Percentage of voiceless response", col = "blue", lwd = 2, xaxt = "n")
lines(Stage, hp2_l, col = "red", lwd = 2)
axis(1, at = Stage, labels = Stage)

legend("right", legend = c("high F0", "low F0"), col = c("blue", "red"), lwd = 2)

title("Percentage of voiceless response by Stage")

citation()
