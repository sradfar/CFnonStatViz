# Load required libraries
library(ggcorrplot)
library(dplyr)
library(readxl)
library(tidyr)
library(corrplot)
library(extrafont) # For handling fonts

# Load fonts if not already done
loadfonts(device = "pdf") # Load fonts for use with PDF

# Load the data
file_path <- "C:/Users/sradfar/OneDrive - The University of Alabama/Georgios/Survey 1_Converted - Copy.xlsx" # Update with your file path
df <- read_excel(file_path)

# Split the data into Academic and Non-academic based on Survey type
academic_data <- df %>%
  filter(`Survey type` == "Academic") %>%
  select(-`Survey type`) %>%
  mutate(across(everything(), as.numeric))

non_academic_data <- df %>%
  filter(`Survey type` == "Non-academic") %>%
  select(-`Survey type`) %>%
  mutate(across(everything(), as.numeric))

# Calculate correlation matrices
cor_matrix_academic <- cor(academic_data, use = "complete.obs")
cor_matrix_non_academic <- cor(non_academic_data, use = "complete.obs")

# Save Plot 1: Correlation heatmap for Academic data using ggcorrplot
pdf("Correlation_Heatmap_Academic.pdf", family = "Times", width = 14, height = 10)
ggplot_academic <- ggcorrplot(cor_matrix_academic, 
                              method = "circle", 
                              type = "lower", 
                              lab = TRUE, 
                              title = "Correlation Heatmap: Academic",
                              colors = c("red", "white", "blue")) +
  theme(plot.title = element_text(family = "serif"),
        text = element_text(family = "serif"))
print(ggplot_academic)
dev.off()

# Save Plot 2: Correlation plot for Academic data using corrplot
pdf("Correlation_Plot_Academic.pdf", family = "Times", width = 14, height = 10)
par(family = "serif")
corrplot(cor_matrix_academic, 
         method = "color", 
         type = "lower", 
         tl.col = "black", 
         tl.srt = 45, 
         title = "Correlation Plot: Academic")
dev.off()

# Save Plot 3: Correlation heatmap for Non-academic data using ggcorrplot
pdf("Correlation_Heatmap_Non_Academic.pdf", family = "Times", width = 14, height = 10)
ggplot_non_academic <- ggcorrplot(cor_matrix_non_academic, 
                                  method = "circle", 
                                  type = "lower", 
                                  lab = TRUE, 
                                  title = "Correlation Heatmap: Non-academic",
                                  colors = c("red", "white", "blue")) +
  theme(plot.title = element_text(family = "serif"),
        text = element_text(family = "serif"))
print(ggplot_non_academic)
dev.off()

# Save Plot 4: Correlation plot for Non-academic data using corrplot
pdf("Correlation_Plot_Non_Academic.pdf", family = "Times", width = 14, height = 10)
par(family = "serif")
corrplot(cor_matrix_non_academic, 
         method = "color", 
         type = "lower", 
         tl.col = "black", 
         tl.srt = 45, 
         title = "Correlation Plot: Non-academic")
dev.off()
