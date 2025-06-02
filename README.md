# TIPS Yield Analysis & Inflation Expectation Inference

This repository contains the code and data used for an empirical research project analyzing inflation expectations using Treasury Inflation-Protected Securities (TIPS) and macroeconomic indicators.

## Project Structure

- 📁 **Raw data**/ # Folder containing source datasets from FRED, BEA, and financial market databases
- 📄 data_cleaning.py # Python script for data preprocessing (cleaning, transforming CSVs)
📄 def_term_inf.csv # Final processed CSV of derived inflation and term premium
📄 dow_jones.csv # Market index data used as a control variable in the analysis
📄 Rep_Code.R # R script for empirical analysis and econometric modeling


## Tools & Techniques

- **Python (Pandas, NumPy):** Data collection, cleaning, transformation
- **R (tidyverse, sandwich, lmtest):** Econometric analysis including Newey-West regressions and Monte Carlo simulations
- **CSV files:** Preprocessed datasets used as inputs for the final model

## Summary

This project replicates and extends findings from academic literature on inflation expectation extraction from TIPS. It showcases a complete pipeline from raw data processing to empirical validation.

---
