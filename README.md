# Suicide Attacks Analysis

This project was developed as part of the "Introduction to Data Science" course at Ben-Gurion University.  
The main objective was to explore, clean, and analyze a real-world dataset using R, and to derive insights through data visualization and statistical methods.

## Dataset

The dataset contains information about global suicide attacks, including:
- Date and location
- Attacking group
- Target type
- Weapon type
- Fatalities and injuries
- Claim responsibility

We performed data cleaning to unify organization names, remove duplicate records, and handle missing values.

## Tools & Libraries Used

- Language: R  
- Data Wrangling: `dplyr`, `tidyverse`  
- Statistical Analysis: `boot`, `webr`  
- Data Visualization: `ggplot2`, `plotly`, `plotrix`, `maps`, `mapproj`

## Key Steps in the Project

### Data Cleaning
- Unified naming conventions (e.g., combining "Islamic State Iraq", "ISIL", etc. into "Islamic State")
- Removed duplicate attacks using location and date
- Filtered and grouped data by terror organizations

### Exploratory Data Analysis
- Identified top 10 deadliest organizations by average deaths per attack
- Analyzed types of weapons used by the Islamic State
- Visualized target types using pie charts

### Bootstrapping & Inference
- Used bootstrapping to estimate confidence intervals for average fatalities
- Compared distributions between male and female suicide attackers

### Mapping
- Created world maps showing number of attacks by country
- Zoomed-in maps for specific groups (e.g., Islamic State, Taliban, Al-Qaeda)

### Time Series Analysis
- Monthly breakdown of attacks per year
- Heatmaps showing temporal patterns

## Insights

- The Islamic State was responsible for the highest number of suicide attacks in the dataset.
- Certain weapons resulted in significantly more casualties on average.
- Distinct patterns exist in the timing and geographical spread of attacks.
- Male attackers had higher average fatalities than female attackers based on bootstrapping results.

---
