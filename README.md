# Project 1 S040 - Analysis of Brazilian Higher Education Course Quality (ENADE CPC)

This repository contains an analysis of the quality of higher education courses in Brazil, comparing online ("Educação a Distância") and in-person ("Educação Presencial") modalities. The project uses data from the national student performance exam (ENADE) and the Preliminary Course Concept (CPC), which is a key quality indicator calculated by the Brazilian Ministry of Education.

## About the Data
The Preliminary Course Concept (CPC) is a comprehensive quality indicator that evaluates undergraduate courses in Brazil. It is calculated annually and considers several factors, including:
* Student performance on the ENADE exam.
* The faculty's qualifications (e.g., percentage of professors with master's and doctoral degrees).
* Students' perception of the course, gathered through questionnaires.

The CPC is graded on a 1 to 5 scale, where scores of 1 and 2 are considered unsatisfactory and were put together in this project.

## How to Use This Repository

### Prerequisites

To run the analysis script, you will need R and RStudio installed, along with the following R packages:
* `dplyr`
* `ggplot2`
* `purrr`

You can install them by running:
```r
install.packages(c("dplyr", "ggplot2", "purrr"))
