NLA Project Materials
Overview
This repository contains code and supporting information used in our manuscript analyzing cyanotoxins and environmental drivers using data from the National Lakes Assessment (NLA) for the years 2012, 2017, and 2022.
The goal of this project is to evaluate patterns in toxin occurrence and environmental factors across U.S. lakes using standardized, large-scale monitoring data. We examined whether the physicochemical predictors of conventional biomass indicators, such as chlorophyll-a and total cyanobacterial biovolume, differ from those influencing the biovolumes of key toxin-producing taxa, including Microcystis and Cylindrospermopsis. We further assessed what limnological factors are most strongly associated with microcystins (MICX) and cylindrospermopsins (CYLSPER). We used statistical models to quantify relationships and rank predictor importance across chlorophyll-a, total cyanobacterial biovolume, and the biovolumes of dominant toxin-producing genera, and main factors driving the two major toxins.
Data Source
All raw datasets were downloaded directly from the U.S. EPA National Aquatic Resource Surveys:
https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys
For detailed descriptions of variables and abbreviations, refer to the metadata .txt files provided by the EPA.
Few key Variables and Abbreviations
•	MICX = Microcystin (water column sample)
•	CYLSPER = Cylindrospermopsin
Note: Only “X” site samples (open water) were used in this analysis.
Littoral samples (e.g., MICL) were excluded to maintain consistency across lakes.
•	NTL = Total Nitrogen
•	PTL = Total Phosphorus
•	INDEX_SITE_DEPTH ≈ Maximum lake depth (Zmax)
This variable is used as a proxy for lake depth in analyses.
Cyanotoxin measurements below detection limits (BDL) were handled using a half detection limit substitution approach, a common method in environmental data analysis.
Method Detection Limits (MDL)
•	Microcystin (MICX): 0.1 µg/L
•	Cylindrospermopsin (CYLSPER): 0.05 µg/L
•	Values reported as non-detects were replaced with:
o	MICX → 0.05 µg/L
o	CYLSPER → 0.025 µg/L
Data Processing Notes
•	Relevant variables were selected for analysis.
•	Data were filtered to include:
o	Open water sampling sites (“X” sites)
o	Relevant toxin and environmental variables
•	Log transformations and scaling were applied where appropriate (see scripts for details).
Repository Structure 
data_raw/          # Original NLA datasets 
scripts/               # R scripts for data cleaning, analysis, and visualization
figures/               # Output plots and figures
results/               # Model outputs and summaries
README.md    # Project documentation
Reproducibility
All analyses were conducted in R. Scripts are organized to allow:
1.	Data import and cleaning
2.	Data transformation
3.	Statistical analysis and modeling
4.	Figure generation
Users should be able to reproduce the main results by running scripts in sequence.
Notes
•	Interpretation of toxin patterns should consider detection limits and sampling design.
•	Differences between toxin types, Microcystin vs. Cylindrospermopsin) are explicitly accounted for in the analysis workflow.
Contact
For questions or collaboration inquiries, please reach out to the repository maintainer.
Yusuf Olaleye (Yusuf_Olaleye1@baylor.edu)
