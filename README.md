Tools and data for Agricultural Productivity Analaysis in Ghana
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ftsiboe/GHAgricProductivityLab/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ftsiboe/USFarmSafetyNetLab/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/ftsiboe/GHAgricProductivityLab/graph/badge.svg?token=KIE05NKNU8)](https://codecov.io/gh/ftsiboe/GHAgricProductivityLab)
![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)
![R \>= 4.0](https://img.shields.io/badge/R-%3E=4.0-blue) [![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)
<!-- badges: end -->

*Collaborations with enthusiastic and committed graduate students are
welcomed. For more details see the wiki on [Graduate Student Opportunity
to Lead Research on Agricultural Productivity in
Ghana](https://github.com/ftsiboe/GHAgricProductivityLab/wiki/Graduate-Student-Opportunity-to-Lead-Research-on-Agricultural-Productivity-in-Ghana)*

## 📘 Overview

The primary objective of this project is to determine whether
agricultural production shortfalls in Ghana are driven by farmer
technical inefficiency, technology gaps, or a combination of both.

It compiles a series of research articles that assess farmer-specific
and institutional factors, exploring how they influence technical
inefficiency and the adoption of advanced agricultural technologies.

By identifying the root causes of production shortfalls, this project
provides empirical evidence crucial for policy discussions aimed at
enhancing agricultural productivity in contexts with limited access to
new technologies.

## 📊 Data

The project utilizes farm-level data from the Ghana Living Standards
Surveys (GLSS), which are population-based surveys conducted
periodically, approximately every five years, in Ghana:
[GLSS1](https://microdata.statsghana.gov.gh/index.php/catalog/7),
[GLSS2](https://microdata.statsghana.gov.gh/index.php/catalog/4),
[GLSS3](https://microdata.statsghana.gov.gh/index.php/catalog/12),
[GLSS4](https://microdata.statsghana.gov.gh/index.php/catalog/14),
[GLSS5](https://microdata.statsghana.gov.gh/index.php/catalog/5),
[GLSS6](https://microdata.statsghana.gov.gh/index.php/catalog/72), and
[GLSS7](https://microdata.statsghana.gov.gh/index.php/catalog/97)

## 🧪 Methods

- The Meta-Stochastic Frontier (MSF) model, as introduced by [Huang,
  Huang, & Liu (2014)](https://doi.org/10.1007/s11123-014-0402-2), is
  employed to capture technological heterogeneity and technical
  inefficiencies across farmers.
- To minimize self-selection bias and to reduce model dependency, a
  matching approach is also utilized where applicable ([Abadie and
  Imbens 2006](https://doi.org/10.1111/j.1468-0262.2006.00655.x);
  [Abadie and Imbens 2016](https://doi.org/10.3982/ECTA11293)).

## 📄 Published Studies

- [Technology and managerial performance of farm operators by age in
  Ghana](https://doi.org/10.1007/s11123-023-00679-y) \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/releases/tag/legacy_studies)

- [Taking stock of gender gaps in crop production technology adoption
  and technical efficiency in
  Ghana](https://doi.org/10.1080/03031853.2022.2150664) \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/releases/tag/legacy_studies)

- [Starchy Staples Production Shortfall in Ghana is influenced by
  Technical Inefficiency than by Ecological Technology
  Gaps](https://doi.org/10.1371/journal.pone.0284780) \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/releases/tag/legacy_studies)

- [Production Technology, Efficiency, and Productivity of Cereal
  Farms](https://doi.org/10.1017/age.2022.16) \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/releases/tag/legacy_studies)

- [Chronic Sources of Low Cocoa Production in
  Ghana](https://doi.org/10.1017/age.2021.3) \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/releases/tag/legacy_studies)

- [Effect of Fertilizer Subsidy on Household Level Cereal Production in
  Ghana](https://doi.org/10.1016/j.sciaf.2021.e00916) \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/releases/tag/legacy_studies)

- [Spatiotemporal Evaluation of Dry Beans and Groundnut Production
  Technology and Inefficiency in
  Ghana](https://ageconsearch.umn.edu/record/310316/?ln=en&v=pdf) \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/releases/tag/legacy_studies)

- [Vegetable Production Technical Efficiency and Technology Gaps in
  Ghana](https://ageconsearch.umn.edu/record/301046/?ln=en&v=pdf) \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/releases/tag/legacy_studies)

- [Nationally Representative Farm/Household Level Dataset on Crop
  Production in Ghana from
  1987-2017](http://dx.doi.org/10.2139/ssrn.4134518) \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/releases/tag/legacy_studies)

## 🛠 Work in Progress

- [Technology Gaps Drive Inequality in Production for Farmers with
  Disabilities](https://github.com/ftsiboeGHAgricProductivityLabb/blob/master/replications/tech_inefficiency_disability/DisabilityAgricProdGapGhana_public_version.pdf)
  \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/tree/main/replications/disability)
- [Heterogeneous Impacts of Farmer Education on Technology Access and
  Technical Efficiency in
  Ghana](https://github.com/ftsiboeGHAgricProductivityLabb/tree/master/replications/tech_inefficiency_education#heterogeneous-impacts-of-farmer-education-on-technology-access-and-technical-efficiency-in-ghana)
  \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/tree/main/replications/education)
- [Digging into the Linkages Between Resource Extraction and Farm
  Performance](https://github.com/ftsiboeGHAgricProductivityLabb/tree/master/replications/tech_inefficiency_resource_extract#digging-into-the-linkages-between-resource-extraction-and-farm-performance)
  \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/tree/main/replications/resource_extract)
- Societal Peace and Cohesion Improve Farm Technology and Technical
  Efficiency in Ghana \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/tree/main/replications/conflict)
- Financial Inclusion and Credit Impacts on Crop Production in Ghana \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/tree/main/replications/financial_inclusion)
- Land Ownership Appears Neither Necessary nor Sufficient for Superior
  Agricultural Performance in Ghana \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/tree/main/replications/land_tenure)
- The Association Between Income Transfers and Crop Production in Ghana
  \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/tree/main/replications/income_transfer)

## 📅 Scheduled Studies

- The link between extension services and Farm Technology Endowment and
  its Efficient Use \|
  [🗂️⚙️](https://github.com/ftsiboe/GHAgricProductivityLab/tree/main/replications/extension)

## 🔓 Data and Code Availability

The datasets and code used in this project are openly available to
facilitate replication and further research. They may also be used
freely for related studies. To help track the usage and impact of these
resources, please inform us if you intend to use them beyond direct
replication.

## 📬 Contact

Constructive feedback is highly appreciated, and collaborations using
the available codes and data are actively encouraged. Please reach out
by sending emails to Francis Tsiboe (<ftsiboe@hotmail.com>).

## 💰 Funding

This project did not receive any external funding.

## 📚 Citation

If you find this repository useful, please star this project and cite
our papers listed above.

See the [LICENSE](../LICENSE) file in the repository’s root for details.

*Maintained by [ftsiboe](https://github.com/ftsiboe)*
