# README

## About

Supporting code and images for:
Sonabend, R. E. B. (2021). A Theoretical and Methodological Framework for Machine Learning in Survival Analysis: Enabling Transparent and Accessible Predictive Modelling on Right-Censored Time-to-Event Data. Unpublished doctoral thesis. UCL.

## Structure

Images and code are separated into folders of the same name. Each is structured with sub-folders that reflect the corresponding chapter number/name in the thesis as follows:

| Folder | Chapter |
|:----------|:----------|
| c2_set | Chapter 2: Survival Analysis and Machine Learning |
| c3_mod | Chapter 3: A Critical Survey of Survival Analysis Models |
| c4_eval | Chapter 4: Model Evaluation |
| c5_car | Chapter 5: Composition and Reduction |
| c6_tools | Chapter 6: Software Packages |
| c7_bench | Chapter 7: A Benchmark Experiment of Survival Models |


## Images

All images included in the above PhD thesis are in the "images" folder. Images may be copied and reproduced if appropriately cited.

## Code

All examples and benchmark experiments in the above PhD thesis are in the "code" folder. All code should include seeds for reproducibility. Code was written across varying package versions and R versions. Job scripts in "c7_bench" and related R scripts can largely be ignored as they will not be applicable to other clusters.

## Datasets and results

Datasets and results for the real experiments in Chapter 7 are inside "code/c7_bench/real_jobs". Simulated datasets can be generated with code in "code/c7_bench/sim_jobs" with results in the same folder.

## Issues

Please open an [issue](https://github.com/RaphaelS1/thesis_code_images/issues) to report:

1. Bugs in the code
2. Reproducibility issues between the code and examples in the thesis
3. Missing images or code
