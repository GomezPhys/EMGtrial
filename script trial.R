### Required packages
library(readxl) ## To load excel sheet
library(dplyr) # Data grammar and manipulation
library(rstatix) # Shapiro Wilk and effect size
library(psych) #descriptives
library(kableExtra) #tables
library(lme4) #linear mixed effects models (LMM)
library(lmerTest) #anova like output for LMM
library(ggplot2) #data visualization
library(ggpubr)#data visualization
library(ggprism)##makes plots look like graphad
library(table1) #for descriptives
library(signal)
install.packages("signal")
library(signal)

Df <- read_excel("~/F285.xlsx",
                           sheet = "Sheet 1")
View(Df)


smooth_signal <- function(signal, window_size) {
  filter(signal, rep(1/window_size, window_size), sides = 2)
}


Df <- Df %>%
  mutate(
    CP = smooth_signal(CP, 50),
    HDM = smooth_signal(HDM, 50)
  )

normalize_signal <- function(signal) {
  (signal - min(signal)) / (max(signal) - min(signal))
}


Df <- Df %>%
  mutate(
    EMG1_normalized = normalize_signal(CP),
    EMG2_normalized = normalize_signal(HDM)
  )



# Plot the processed EMG signals
ggplot(Df, aes(x = Time)) +
  geom_line(aes(y = CP, color = "CP")) +
  geom_line(aes(y = HDM, color = "HDM")) +
  labs(title = "Processed EMG Signals", x = "Time (s)", y = "Normalized EMG") +
  theme_prism()
# full-wave rectification
rectified_emg <- abs(Df$CP)

low_freq <- 15  
high_freq <- 100  
sampling_rate <- 1000

# filter set up
lowpass_filter <- butter(2, high_freq / (0.5 * sampling_rate), type = "low")
highpass_filter <- butter(2, low_freq / (0.5 * sampling_rate), type = "high")

# apply filters
filtered_emg <- filter(lowpass_filter, filter(highpass_filter, rectified_emg))

