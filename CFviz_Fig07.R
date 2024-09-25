# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(scales)
library(extrafont) # For setting fonts

# Load the Excel file
file_path <- "C:/Users/sradfar/OneDrive - The University of Alabama/Georgios/Survey 1_Converted - Copy.xlsx" # Use the correct file path if needed
# Read the data from the first sheet
df <- read_excel(file_path, sheet = 1)

# Select the "Survey type" column along with the specified question columns with updated names
questions <- df %>%
  select("Survey type",
         "Correlation understandability (Numerical)", 
         "Correlation understandability (Angles)", 
         "Non-stationarity clarity for respondent (Scatterplot)", 
         "Non-stationarity clarity for audience (Scatterplot)", 
         "Non-stationarity clarity for respondent (Angles)", 
         "Non-stationarity clarity for audience (Angles)", 
         "Effectiveness in CCF risk communication", 
         "Likelihood of applying (in work/research)", 
         "Likelihood of applying (in public communication)")

# Convert the numeric responses to factors with corresponding labels and include the "Survey type" grouping
questions_long <- questions %>%
  pivot_longer(cols = -`Survey type`, names_to = "Question", values_to = "Response") %>%
  mutate(Response = factor(Response, 
                           levels = c(1, 2, 5, 4, 3),
                           labels = c("Disagree", "Neutral", "Slightly Agree", "Agree", "Strongly Agree")))

# Set the desired order of the questions based on new column names and reverse it
questions_long$Question <- factor(questions_long$Question, levels = rev(c(
  "Correlation understandability (Numerical)", 
  "Correlation understandability (Angles)", 
  "Non-stationarity clarity for respondent (Scatterplot)", 
  "Non-stationarity clarity for audience (Scatterplot)", 
  "Non-stationarity clarity for respondent (Angles)", 
  "Non-stationarity clarity for audience (Angles)", 
  "Effectiveness in CCF risk communication", 
  "Likelihood of applying (in work/research)", 
  "Likelihood of applying (in public communication)"
)))

# Count the responses for each question grouped by Survey type
response_count <- questions_long %>%
  group_by(`Survey type`, Question, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(`Survey type`, Question) %>%
  mutate(Percentage = Count / sum(Count) * 100) # Calculate percentages

# Split responses into positive and negative
response_count <- response_count %>%
  mutate(Position = case_when(
    Response %in% c("Disagree", "Neutral") ~ -Percentage, # Negative side
    TRUE ~ Percentage # Positive side
  ))

# Adjust the order manually within ggplot for correct display
response_count$Response <- factor(response_count$Response, 
                                  levels = c("Disagree", "Neutral", "Slightly Agree", "Agree", "Strongly Agree"))

# Manually set the facet labels with specific respondent counts
facet_labels <- c("Academic" = "Academic (n=44)", 
                  "Non-academic" = "Non-academic (n=47)")

# Plot using ggplot2 for a horizontal stacked bar chart split by Survey type
plot <- ggplot(response_count, aes(x = Question, y = Position, fill = Response)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("Disagree" = "#D73027", "Neutral" = "#FC8D59", 
                               "Slightly Agree" = "#313695", "Agree" = "#4575B4", 
                               "Strongly Agree" = "#91BFDB")) + 
  geom_text(aes(label = ifelse(abs(Percentage) >= 5, paste0(round(abs(Percentage), 1), "%"), NA)), 
            position = position_stack(vjust = 0.5), size = 3, check_overlap = FALSE) +
  labs(x = NULL, y = "Percentage", fill = "Response") +  # Removed title and y-axis label
  facet_wrap(~`Survey type`, ncol = 2, labeller = labeller(`Survey type` = facet_labels)) +  # Use manual facet labels
  theme_minimal() +
  theme(
    text = element_text(family = "Times", size = 14), # Increase font size globally
    axis.text.y = element_text(angle = 0, hjust = 1, family = "Times", size = 12), # Increase axis text size
    plot.margin = margin(5, 5, 5, 5), # Adjust margins for a tight layout
    strip.text = element_text(size = 14) # Increase facet label text size
  ) # Adjust margins for a tight layout

# Display the plot
print(plot)

# Save the plot as a PDF with a tight layout
ggsave("Survey responses.pdf", plot = plot, device = "pdf", 
       width = 14, height = 7, units = "in", dpi = 300, family = "Times")
