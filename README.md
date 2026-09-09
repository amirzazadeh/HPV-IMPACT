# HPV-IMPACT Study

## Household and School-Based Survey Methods to Assess HPV Vaccination Coverage Among Adolescent Girls in Liberia, Rwanda, and Senegal

This repository provides comprehensive materials supporting the HPV-IMPACT Study methods paper, including study example data, analysis R code, and survey instruments used to assess HPV vaccination coverage among adolescent girls across three African countries.

---

## Study Overview

The HPV-IMPACT Study employs both household and school-based survey methods to evaluate HPV vaccination coverage among adolescent girls in:
- **Liberia**
- **Rwanda**
- **Senegal**

---

## Repository Contents

### Study Materials

- **[De-identified Study Data](Data/)** — Cleaned, de-identified dataset ready for analysis
  - [`Liberia_HH_GirlsSelection.xlsx`](Data/Liberia_HH_GirlsSelection.xlsx) — Data required to calculate sampling probabilities at each stage of the survey and for post-stratification.
  - [`Liberia_HPV_Caregivers_ShortData.xlsx`](Data/Liberia_HPV_Caregivers_ShortData.xlsx) — Household survey data containing caregiver responses related to HPV vaccination of their adolescent girls.
- **[R Analysis Code](R/)** — Complete R scripts for data processing, analysis, and visualization
  - [`Rcode_RW_HH_Survey_Sampling_Weights_Calculation.R`](R/Rcode_RW_HH_Survey_Sampling_Weights_Calculation.R) — Calculates sampling weights for the Household (HH) survey.
  - [`Rcode_RW_School_Survey_Sampling_Weights_Calculation.R`](R/Rcode_RW_School_Survey_Sampling_Weights_Calculation.R) — Calculates sampling weights for the School survey.
- **[Household Survey Questionnaire - Adolescents](Materials/Household_Survey_Questionnaire_Used_to_Interview_Adolescents.docx)** — Questionnaire used to interview adolescents
- **[Household Survey Questionnaire - Caregivers](Materials/Household_Survey_Questionnaire_Used_to_Interview_Caregivers.docx)** — Questionnaire used to interview caregivers
- **[School-Level Data Collection Form](Materials/School_Level_Data_Collection_Form.docx)** — Form used to collect school-level data
- **[School-Based Survey Questionnaire](Materials/School_Based_Survey_Questionnaire_Used_to_Interview_Students.docx)** — Questionnaire used to interview students

## How to Use This Repository

### For Researchers
- Review the survey questionnaires to understand data collection methodology
- Access de-identified data for secondary analysis
- Examine R code to understand analytical approaches

### For Reproduction
- All analysis code is provided to enable full reproducibility
- Follow the R scripts in order to replicate figures, tables, and statistical analyses
- De-identified data can be used with provided code to regenerate all results

### For Citations
- Use the study citation provided below when referencing this work
- Cite specific materials (e.g., questionnaires) when applicable

---

## Citation

**Household and School-Based Survey Methods to Assess HPV Vaccination Coverage Among Adolescent Girls in Liberia, Rwanda, and Senegal: The HPV-IMPACT Study**

*Citation and DOI will be added upon publication.*

---

## Contact

For questions about the study, methodology, or materials, please contact ali.mirzazadeh@ucsf.edu.

---

## Version History

- **Version 1.0** — Initial repository release with study materials

---

**Last Updated:** 2026-09-08
