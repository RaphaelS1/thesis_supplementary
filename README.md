# README

## About

Supporting code, images, datasets, and results for:

_Sonabend, R. E. B. (2021). A Theoretical and Methodological Framework for Machine Learning in Survival Analysis: Enabling Transparent and Accessible Predictive Modelling on Right-Censored Time-to-Event Data. Unpublished doctoral thesis. UCL._

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

The `results` folder contains results from the two experiments from Chapter 7.

## Images

All images included in the thesis are in the "images" folder. Images may be reproduced if appropriately cited.
Graphics such as UML diagrams and pipelines are not included in this repo, these can be requested by opening an issue.

## Code

All examples and benchmark experiments in the thesis are in the "code" folder. All code should include seeds for reproducibility. Code was written across varying package versions and R versions. All code is licensed under an MIT licence.

## Results

The results from the two benchmark experiments from Chapter 7 are in this folder. "real_results.csv" contains the results for the real experiments and "sim_results.csv" is the results of the simulation experiments. These are the 'raw' results before any modifications to the data. Each row represents the aggregated score (over all folds) for a data/model pairing.

## Datasets

Datasets for the real experiments in Chapter 7 are inside "code/c7_bench/real_jobs/data". Simulated datasets can be generated with the code from "code/c7_bench/sim_jobs/benchmark/gen_simulated_data.R".

## Issues

Please open an [issue](https://github.com/RaphaelS1/thesis_code_images/issues) to report:

1. Bugs in the code
2. Reproducibility issues between the code and examples in the thesis
3. Missing images or code
4. Requesting an image not in this repo such as UML diagrams or pipelines
