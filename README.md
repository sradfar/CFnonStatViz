# CFnonStatViz

This repository contains the data analysis scripts and datasets accompanying the manuscript:

> Radfar, S., et al. (2025). *The value of visualization in improving compound flood hazard communication: a complementary perspective through a Euclidean geometry lens*. [Journal name, volume, pages].

The project provides reproducible R code and datasets to generate the figures and results presented in the paper, focusing on correlation analysis, survey visualization, and case studies for Washington, DC and Houston, TX.

---

## Repository Structure

### R Scripts
- **CF_viz_Analysis.R**  
  Performs statistical correlation and angle-based analysis of compound flooding:  
  - Washington, DC case study (discharge vs surge)  
  - Houston, TX case study (discharge vs still water level)  
  - Computes Pearson, Spearman, Kendall correlations  
  - Generates vector and angular visualizations of correlation across time periods  

- **CFviz_Fig06.R**  
  Creates Likert-style stacked bar charts (Figure 6 of the manuscript).  
  - Visualizes survey responses regarding familiarity, relevance, experience, and non-stationarity awareness  
  - Splits results by **Academic** vs **Non-academic** groups  

- **CFviz_Fig07.R**  
  Generates survey response visualizations (Figure 7).  
  - Covers understandability of correlation measures, clarity of non-stationarity communication, effectiveness, and likelihood of application  
  - Uses stacked bar plots with percentage labels  

- **CFviz_Fig08.R**  
  Produces correlation heatmaps and correlation plots for survey responses (Figure 8).  
  - Separate outputs for Academic and Non-academic groups  
  - Outputs both ggcorrplot and corrplot visualizations  

### Data Files
- **Washington-Q_S.csv**  
  Discharge–surge dataset for Washington, DC case study  

- **Houston-Q_SWL.csv**  
  Discharge–still water level dataset for Houston, TX case study  

- **Survey 1_Converted - Copy.xlsx**  
  Survey dataset used to generate Figures 6–8. Contains responses from Academic and Non-academic participants  

### Other Files
- **LICENSE**  
  Apache License 2.0  

---

## Requirements

All scripts are written in **R**. Required packages include:

- dplyr  
- ggplot2  
- tidyr  
- readxl  
- scales  
- extrafont  
- geometry  
- matlib  
- ggcorrplot  
- corrplot  

Install them with:

    install.packages(c("dplyr", "ggplot2", "tidyr", "readxl",
                       "scales", "extrafont", "geometry",
                       "matlib", "ggcorrplot", "corrplot"))

---

## Usage

1. Clone the repository:

       git clone https://github.com/sradfar/CFnonStatViz.git
       cd CFnonStatViz

2. Open any of the R scripts in R or RStudio.

3. Update the path to the survey Excel file if needed (currently set to a local path). Example in CFviz_Fig06.R:

       file_path <- "Survey 1_Converted - Copy.xlsx"

4. Run the script to reproduce figures.

---

## Figures Reproduced

- **CF_viz_Analysis.R** → Case study visualizations (Washington & Houston)  
- **CFviz_Fig06.R** → Figure 6 (Survey familiarity, relevance, experience, non-stationarity awareness)  
- **CFviz_Fig07.R** → Figure 7 (Understandability, clarity, effectiveness, likelihood of application)  
- **CFviz_Fig08.R** → Figure 8 (Correlation heatmaps for Academic vs Non-academic)  

---

## Citation

If you use this repository or its data/code, please cite:

Radfar, S., et al. (2025). *The value of visualization in improving compound flood hazard communication: a complementary perspective through a Euclidean geometry lens*. [Journal Name, Volume(Issue), Pages].  
DOI: [insert DOI]

---

## License

This repository is licensed under the [Apache License 2.0](LICENSE).
