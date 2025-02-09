# Understanding-Factors-Contribute-to-Fatalities-Rate-in-Auto-Accidents

The research aims to identify factors contributing to fatal crash rates includes crash characteristics, driver demographics, environmental conditions, and more. The study spans data collected from 2012 to 2022 and uses both descriptive analysis and regression modeling to explore the relationships between these factors and fatality rates.

# Descriptive Analysis Summary
The dataset indicates that fatal crashes occur consistently over the years, with an average of slightly more than one fatality per crash. Environmental factors, such as weather and light conditions, show significant variability suggest diverse scenarios under which crashes occur. Substance use and speeding appear in nearly one-third of crashes which points to their critical role in accident severity. Demographic data highlights the high involvement of younger drivers (ages 15–24) and their notable impact on fatality rates. Other factors, like unlicensed drivers and wrong-way incidents, are less common but remain important due to their severe implications.

# Regression Model Findings
A regression model was used to identify significant predictors of fatal crash rates. Key findings include the following:

- Substance Impairment: Alcohol involvement (dralc_inv) and overall driver impairment (did_inv) significantly increase the expected number of fatalities.
- Young Drivers: The involvement of drivers aged 15–20 and 21–24 shows a strong, positive association with fatality rates, likely reflecting their inexperience and risk-taking behavior.
- Older Drivers: Drivers aged 65 and above also contribute to higher fatality rates, though the impact is less pronounced than for younger drivers.
- Non-Significant Factors: Variables such as weather, intersection type, hit-and-run involvement, and the day of the week did not show a significant relationship with fatality rates indicate their limited influence in this dataset.

# Data Source: 
- Data is provided by the Washington Traffic Safety Commission.
- Data is processed, merged, and selected for usable variables in the model. 

# Description Summary Table

| **Statistic**      | **N**   | **Mean**  | **St. Dev.** | **Min** | **Max** |
|---------------------|---------|-----------|--------------|---------|---------|
| year               | 5,620   | 2017.473  | 3.163        | 2012    | 2022    |
| Crash.Month        | 5,620   | 6.85      | 3.296        | 1       | 12      |
| weather            | 5,620   | 4.013     | 11.597       | 1       | 99      |
| lightcond          | 5,620   | 1.982     | 1.295        | 1       | 9       |
| numfatal           | 5,620   | 1.077     | 0.319        | 1       | 5       |
| intersectiontype   | 5,619   | 1.441     | 2.386        | 1       | 99      |
| wk_zone            | 5,620   | 0.008     | 0.089        | 0       | 1       |
| spdlim             | 5,619   | 49.434    | 19.987       | 0       | 99      |
| hitrun_inv         | 5,620   | 0.057     | 0.232        | 0       | 1       |
| dralc_inv          | 5,620   | 0.323     | 0.468        | 0       | 1       |
| aid_inv            | 5,620   | 0.276     | 0.447        | 0       | 1       |
| did_inv            | 5,620   | 0.348     | 0.477        | 0       | 1       |
| id_inv             | 5,620   | 0.487     | 0.5          | 0       | 1       |
| impnm_inv          | 5,620   | 0.105     | 0.306        | 0       | 1       |
| nmalc_inv          | 5,620   | 0.057     | 0.231        | 0       | 1       |
| ainm_inv           | 5,620   | 0.049     | 0.216        | 0       | 1       |
| dinm_inv           | 5,620   | 0.08      | 0.272        | 0       | 1       |
| speed_inv          | 5,620   | 0.321     | 0.467        | 0       | 1       |
| unlic_inv          | 5,620   | 0.204     | 0.403        | 0       | 1       |
| drowsy_inv         | 5,620   | 0.024     | 0.152        | 0       | 1       |
| dr1517_inv         | 5,620   | 0.035     | 0.185        | 0       | 1       |
| dr1820_inv         | 5,620   | 0.09      | 0.286        | 0       | 1       |
| dr1520_inv         | 5,620   | 0.124     | 0.329        | 0       | 1       |
| dr2124_inv         | 5,620   | 0.143     | 0.35         | 0       | 1       |
| dr1524_inv         | 5,620   | 0.258     | 0.437        | 0       | 1       |
| dr65_inv           | 5,620   | 0.191     | 0.393        | 0       | 1       |
| dr70_inv           | 5,620   | 0.131     | 0.337        | 0       | 1       |
| dr75_inv           | 5,620   | 0.086     | 0.28         | 0       | 1       |
| wrongway           | 5,620   | 0.019     | 0.137        | 0       | 1       |
| oppdir             | 5,620   | 0.124     | 0.329        | 0       | 1       |


# Regression Model

| **Statistic**              | **Value**                              |
|----------------------------|-----------------------------------------|
| **Residuals**              |                                         |
| Minimum                    | -0.2213                                |
| 1st Quartile (1Q)          | -0.097                                 |
| Median                     | -0.0664                                |
| 3rd Quartile (3Q)          | -0.0449                                |
| Maximum                    | 3.8977                                 |
| **Coefficients**           |                                         |
| (Intercept)                | 1.0265 (SE: 0.0122, t: 84.461, p < 2e-16) *** |
| dat$weather                | 0.0003 (SE: 0.0004, t: 0.740, p = 0.4594) |
| dat$intersectiontype       | -0.0013 (SE: 0.0018, t: -0.727, p = 0.4670) |
| dat$hitrun_inv             | -0.0259 (SE: 0.0185, t: -1.395, p = 0.1631) |
| dat$dralc_inv              | 0.0209 (SE: 0.0095, t: 2.205, p = 0.0275) * |
| dat$did_inv                | 0.0377 (SE: 0.0091, t: 4.136, p = 3.59e-05) ** |
| dat$dr1520_inv             | 0.0848 (SE: 0.0130, t: 6.523, p = 7.48e-11) *** |
| dat$dr2124_inv             | 0.0321 (SE: 0.0122, t: 2.626, p = 0.0087) ** |
| dat$dr65_inv               | 0.0218 (SE: 0.0111, t: 1.959, p = 0.0502) . |
| dat$dayweekMonday          | 0.0004 (SE: 0.0153, t: 0.023, p = 0.9813) |
| dat$dayweekSaturday        | 0.0129 (SE: 0.0146, t: 0.880, p = 0.3787) |
| dat$dayweekSunday          | 0.0192 (SE: 0.0152, t: 1.263, p = 0.2067) |
| dat$dayweekThursday        | 0.0230 (SE: 0.0158, t: 1.452, p = 0.1467) |
| dat$dayweekTuesday         | 0.0191 (SE: 0.0158, t: 1.209, p = 0.2269) |
| dat$dayweekWednesday       | 0.0274 (SE: 0.0157, t: 1.748, p = 0.0806) . |
| **Model Summary**          |                                         |
| Residual Std. Error        | 0.3167 (on 5604 df)                    |
| R-squared                  | 0.0146                                 |
| Adjusted R-squared         | 0.0122                                 |
| F-statistic                | 5.934 (on 14 and 5604 df, p = 9.489e-12) |
