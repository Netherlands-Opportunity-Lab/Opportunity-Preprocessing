# README and Guidance OpportunityMap Preprocessing

This readme follows the template created by Social Science Data Editors. See section References.

## Overview

The code in this replication package generates the dataasets used to produce the results results shown on the interactive websites OpportunityMap.nl (for the Dutch version see KansenKaart.nl) and OpportunityGap.nl (for the Dutch version see KansenKloof.nl).

## What are OpportunityMap NL and OpportunityGap NL?

[OpportunityMap](https://opportunitymap.nl/) and [OpportunityGap](https://opportunitygap.nl/) form part of a research project by the [Netherlands Opportunity Lab](https://github.com/Netherlands-Opportunity-Lab/) which studies intergenerational mobility in the Netherlands. The aim of this research project is to document to what extent the circumstances in which a child grows up are associated with outcomes from birth through adulthood. Child sex, migration background parental income, wealth, education, the number of parents in the household, and the place where a child grows up, are considered predictor variables, which measure circumstances beyond a child’s control.

The code in this replication package creates the sample datasets used for the [OpportunityMap](https://opportunitymap.nl/) and [OpportunityGap](https://opportunitygap.nl/) analyses which are described [here](https://github.com/sodascience/kansenkaart_analysis) and [here](https://github.com/Netherlands-Opportunity-Lab/Kansenkloof-NL-Analysis), respectively.

In total we study 91 (including 20 correlates) outcomes that are measured at birth and in the final (eighth) grade of primary school, and at ages 16, 21, and 34-35. These data can be used to provide granular insights into how, when, and where the opportunity gap in the Netherlands opens up.

## Data Availability and Provenance Statements

The analysis combines several Dutch administrative data registers. The data use is subject to the European Union’s General Data Protection Regulation (GDPR) and Dutch law. The research is conducted within the secure remote access microdata CCenvironment of Statistics Netherlands.

Researchers interested in obtaining access to the data employed in this analysis are required to submit a research proposal, a publication plan, a GDPR-ground and purpose, and an application form, Access will be granted only to institutions with a valid authorization by Statistics Netherlands. All relevant information on how to obtain data access through Statistics Netherlands and institutional authorization can be found [here](https://www.cbs.nl/en-gb/our-services/customised-services-microdata/microdata-conducting-your-own-research/applying-for-access-to-microdata).

### Summary of Availability

-   Some data **cannot be made** publicly available.

### License for Code

The code is licensed under a GPL v3 license. See LICENSE.txt for details.

Over and above the legal restrictions imposed by this license, if you use this software for an academic publication then you are obliged to provide proper attribution. This can be to this code directly,

XXXX (Zenodo)

## Data and Sources

All the results in the paper use confidential microdata from the Statistics Netherlands (CBS).

To gain access to the microdata, follow the directions in section Data Availability and Provenance Statements.

### Microdata files (Confidential)

| Category | Dataset Name | Notes |
|------------------------|------------------------|------------------------|
| Sample Construction | `gbapersoontab` | Personal characteristics from GBA |
|  | `gbaadresobjectbus` | Residential address history |
|  | `kindoudertab` | Parent-child relationships |
|  | `vslpostcodebus` | Neighborhood-level socioeconomic data |
|  | `vslgwbtab` | Neighborhood classification |
| Predictors | `IPI` | Integrated Personal Income |
|  | `IHI` | Integrated Household Income |
|  | `inpatab` | Personal income |
|  | `inhatab` | Household income |
|  | `inharmatab` | Household income compared to poverty line |
|  | `vehtab` | Assets and debts |
|  | `gbahuishoudensbus` | Household composition |
| Outcomes | `hoogsteopltab` | Highest obtained education level |
|  | `spolisbus` | Insurance data |
|  | `secmbus` | Secondary education outcomes |
|  | `zvwzorgkostentab` | Health care costs |
|  | `eigendomtab` | Home ownership |
|  | `schtab` | School-level data |
|  | `levcyclwoonnietwoonbus` | Life course and housing transitions |
|  | `jdgbeschermbus` | Youth protection measures |
|  | `inschrwpotab` | Enrollment in secondary vocational education (MBO) |
|  | `doodoorztab` | Causes of death |
|  | `Doodsoorzaken (DO)` | Causes of death (old) |
|  | `gbaoverlijdentab` | Date of death and demographics at death |

These files correspond to the ones that can be found in [CBS microdata catalogue](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/catalogus-microdata) (only available in Dutch).

### Auxiliary Data Files (Provided)

| Location | File name | Description | Source |
|------------------|------------------|------------------|------------------|
| /resources | Consumentenprijzen\_\_prijsindex_2015_100_07012021_123946.csv | Consumer price index | CBS |
|  | Gemeenten en COROP vanaf 1981.xlsx | Historical list of municipalities and COROP regions | CBS |
|  | Hoftiezer_Geboortegewicht curves.xlsx | Birth weight reference curves by Hoftiezer et al. | Hoftiezer, L. et al. American Journal of Obstetrics and Gynecology (2018) doi: 10.1016/j.ajog.2018.12.023 & Hoftiezer, L., Hukkelhoven, C.W.P.M., Hogeveen, M. et al. Eur J Pediatr (2016) 175: 1047-57. doi: 10.1007/s00431-016-2740-8 |
|  | LANDAKTUEELREF10.sav | CBS country classification | CBS |
|  | r_packages_cbs.csv | List of R packages available in the secure environment | CBS |
|  | r_version_cbs.txt | R version used for the analysis | Created by us |
|  | sources.txt | Text file listing data sources | Created by us |
|  | vo_advisering.xlsx | Data on secondary school advising (VO advisering) | Created by us |

## Description of Samples

The Opportunity Map Preprocessing generates multiple samples, each defined by the age at which outcomes are measured. This is reflected in the sample names.

### Summary

| Sample | Birth Years | Outcome Age / Timing | Address Reference Age | Parental Income/Wealth Period | Parental Education Timing | Outcome Timing | Child Residency | ParentalResidency |
|--------|--------|--------|--------|--------|--------|--------|--------|--------|
| `newborns (perinatal and mortality)` | 2005–2014 (a), 2015–2024 (b) | At birth | First known address/mother’s address for still births | Year of birth and the year before | At birth | At birth and 365 days after birth |  | In the years parental income and wealth are measured |
| `prim8` | 2003–2006 (a), 2009–2012 (b) | Grade 8 (around age 12) | Age 10 | 2013–2016 (a), 2019–2022 (b) | At age 10 | In the year before taking the CITO test the last time and the year before |  | In the years parental income and wealth are measured |
| `age16` | 2004–2008 | Age 16 | Age 15 | 2019–2023 | \- | Age 15-16 | In the years the outcomes are measured | In the years parental income and wealth are measured |
| `age21` | 1999–2003 | Age 21 | Age 15 | 2014–2018 | \- | Age 20-21 | In the years the outcomes are measured | In the years parental income and wealth are measured |
| `age35` | 1985–1989 | Age 34–35 | Age 15 | 2003–2007 | \- | Age 34–35 | In the years the outcomes are measured | In the years parental income and wealth are measured |

### Samples and Outcomes

#### 1. Perinatal sample

The perinatal cohort comprises approximately 1.5 million Dutch children who were born between 2008 and 2016. We assign these children to their home address based on where they reside at their date of birth. Parental income is measured between 2014 and 2018. The outcomes in the perinatal cohort are measured from 2008 to 2016.

-   c00_sga (small-for-gestational age): Indicates children with a birth weight below the 10th percentile adjusted for gestational age and gender, according to the [Hoftiezer curve](https://github.com/sodascience/kansenkaart_preprocessing/blob/cbs_updated/resources/Hoftiezer_Geboortegewicht%20curves.xlsx).
-   c00_preterm_birth: Indicates children with a gestational age before 37 completed weeks of gestation.

#### 2. Child mortality Sample

The child mortality cohort comprises approximately 1.5 million Dutch children who were born between 2008 and 2016. We assign these children to their home address based on where they reside at birth. Parental income is measured between 2014 and 2018. The outcomes in the child mortality cohort are measured from 2008 to 2016.

-   c00_perinatal_mortality: Represents the death of children that occurs between 24 completed weeks of gestation and up to 7 days after birth.
-   c00_neonatal_mortality: Represents the death of children that occurs between 24 completed weeks of gestation and up to 28 days after birth.
-   c00_infant_mortality: Represents the death of children that occurs between 24 completed weeks of gestation and up to 365 days after birth.

#### 3. Prim8 sample

The prim8 samples consist of students in the final (8th) grade of primary school who were born between 2003 and 2006 (sample a), and 2009 and 2012 (sample b). We assign these students to their home address based on the address on their 10th birthday. Parental income is measured between 2013 and 2016 (sample a), and 2019 and 2022 (sample b). Parental education is defined on the child’s 10th birthday. The outcomes are measured the year the child took the CITO test for the last time and the year before that.

-   c11_math: Indicates whether the student has achieved at least the required level of mathematics in grade 8 of elementary school.
-   c11_reading: Indicates whether the student has achieved at least the required level of reading in grade 8 of elementary school.
-   c11_language: Indicates whether the student has achieved at least the required level of language proficiency in grade 8 of elementary school.
-   c11_vmbo_gl_test: Indicates whether the student has received a test advice of at least VMBO-high or a higher level of high school education.
-   c11_havo_test: Indicates whether the student has received a test advice of at least HAVO or a higher level of high school education.
-   c11_vwo_test: Indicates whether the student has received a test advice of VWO level of high school education.
-   c11_vmbo_gl_final: Indicates whether the student has received a final school advice of at least VMBO-high plus or a higher level of high school education.
-   c11_havo_final: Indicates whether the student has received a final school advice of at least HAVO or a higher level of high school education.
-   c11_vwo_final: Indicates whether the student has received a final school advice of VWO level of high school education.
-   c11_under_advice: Indicates whether the student has received a final school advice lower than the test school advice. It is determined using the advice table we have created, which can be found [here.](https://github.com/sodascience/kansenkaart_preprocessing/blob/main/resources/vo_advisering.xlsx)
-   c11_over_advice: Indicates whether the student has received a final school advice higher than the test school advice. It is determined using the advice table we have created, which can be found [here.](https://github.com/sodascience/kansenkaart_preprocessing/blob/main/resources/vo_advisering.xlsx)
-   c11_diff_test_school_advice: Indicates the difference between the teacher and test track advice. It is determined using the advice table we have created, which can be found [here.](https://github.com/sodascience/kansenkaart_preprocessing/blob/main/resources/vo_advisering.xlsx)
-   c11_youth_health_costs: Represents the total health care costs covered by the Health Insurance Act for students in grade 8 of elementary school.
-   c11_youth_protection: Indicates whether the student has a youth protection measure in place while in grade 8 of elementary school.
-   c11_living_space_pp: Represents the average living space per household member for students in grade 8 of elementary school.
-   c11_class_vmbo_gl_test: Represents the percentage of individual’s classmates in grade 8 of elementary school who receive a final test advice of VMBO-GL or higher.
-   c11_class_havo_test: Represents the percentage of individual’s classmates in grade 8 of elementary school who receive a final test advice of HAVO or higher.
-   c11_class_vwo_test: Represents the percentage of individual’s classmates in grade 8 of elementary school who receive a final test advice of VWO.
-   c11_class_size: Represents the individual’s class size in grade 8 of elementary school.
-   c11_class_math: Represents the percentage of individual’s classmates in grade 8 of elementary school who meet or exceed the math target level.
-   c11_class_language: Represents the percentage of individual’s classmates in grade 8 of elementary school who meet or exceed the language target level.
-   c11_class_reading: Represents the percentage of individual’s classmates in grade 8 of elementary school who meet or exceed the reading target level.
-   c11_class_foreign_born_parents: Represents the percentage of individual’s classmates in grade 8 of elementary school whose both parents were born abroad.
-   c11_class_income_below_25th: Represents the percentage of individual’s classmates in grade 8 of elementary school whose parents’ income is below the 25th percentile of the parental income distribution.
-   c11_class_income_below_50th: Represents the percentage of individual’s classmates in grade 8 of elementary school whose parents’ income is below the 50th percentile of the parental income distribution.
-   c11_class_income_above_75th: Represents the percentage of individual’s classmates in grade 8 of elementary school whose parents’ income is above 75th percentile of the parental income distribution.
-   c11_primary_neighborhood_income_below_25th: Represents the percentage of individual’s neighbors whose parents’ income is below the 25th percentile of the parental income distribution.
-   c11_primary_neighborhood_income_below_50th: Represents the percentage of individual’s neighbors whose parents’ income is below the 50th percentile of the parental income distribution.
-   c11_primary_neighborhood_income_above_75th: Represents the percentage of individual’s neighbors whose parents’ income is above the 75th percentile of the parental income distribution.
-   c11_primary_neighborhood_foreign_born_parents: Represents the percentage of individual’s neighbors whose parents are foreign born.

#### 4. Age16 sample

The age16 sample consists of individuals who are 16 years old and were born between 2004 and 2008. We assign these students to their home address based on where they lived on their 15th birthday. Parental income is measured between 2019 and 2023. Outcomes are measured at ages 15 and 16.

-   c16_vmbo_gl: Indicates whether the individual has completed at least a VMBO-high high school education by the age of 16.
-   c16_havo: Indicates whether the individual has completed at least a HAVO high school education by the age of 16.
-   c16_vwo: Indicates whether the individual has completed at least a VWO high school education by the age of 16.
-   c16_youth_health_costs: Represents the average total health care costs covered by the Health Insurance Act at the age of 15.
-   c16_youth_protection: Indicates whether an individual has a youth protection measure in place at the age of 16.
-   c16_living_space_pp: Represents the average living space per household member at the age of 16.
-   c16_primary_class_foreign_born_parents: Represents the percentage of individual’s classmates in grade 8 of primary school whose both parents were born abroad.
-   c16_primary_class_income_below_25th: Represents the percentage of individual’s classmates in grade 8 of primary school whose parents’ income is below the 25th percentile of the parental income distribution.
-   c16_primary_class_income_below_50th: Represents the percentage of individual’s classmates in grade 8 of primary school whose parents’ income is below the 50th percentile of the parental income distribution.
-   c16_primary_class_income_above_75th: Represents the percentage of individual’s classmates in grade 8 of primary school whose parents’ income is above 75th percentile of the parental income distribution.
-   c16_secondary_class_foreign_born_parents: Represents the percentage of individual’s classmates whose both parents were born abroad in the year the child turned 15.
-   c16_secondary_class_income_below_25th: Represents the percentage of individual’s classmates whose parents’ income is below the 25th percentile of the parental income distribution in the year the child turned 15.
-   c16_secondary_class_income_below_50th: Represents the percentage of individual’s classmates whose parents’ income is below the 50th percentile of the parental income distribution in the year the child turned 15.
-   c16_secondary_class_income_above_75th: Represents the percentage of individual’s classmates whose parents’ income is above 75th percentile of the parental income distribution in the year the child turned 15.
-   c16_age12_neighborhood_income_below_25th: Represents the percentage of individual’s neighbors at age 12 whose parents’ income is below the 25th percentile of the parental income distribution.
-   c16_age12_neighborhood_income_below_50th: Represents the percentage of individual’s neighbors at age 12 whose parents’ income is below the 50th percentile of the parental income distribution.
-   c16_age12_neighborhood_income_above_75th: Represents the percentage of individual’s neighbors at age 12 whose parents’ income is above 75th percentile of the parental income distribution.
-   c16_age12_neighborhood_foreign_born_parents: Represents the percentage of individual’s neighbors at age 12 whose parents are foreign born.
-   c16_neighborhood_income_below_25th: Represents the percentage of individual’s neighbors whose parents’ income is below the 25th percentile of the parental income distribution at age 16.
-   c16_neighborhood_income_below_50th: Represents the percentage of individual’s neighbors whose parents’ income is below the 50th percentile of the parental income distribution at age 16.
-   c16_neighborhood_income_above_75th: Represents the percentage of individual’s neighbors whose parents’ income is above 75th percentile of the parental income distribution at age 16.
-   c16_neighborhood_foreign_born_parents: Represents the percentage of individual’s neighbors whose parents are foreign born at age 16.

#### 5. Age21 sample

The age21 sample consists of individuals at age 21 born between 1998 and 2002. We assign individuals to their home address based on where they lived on their 15th birthday. Parental income is measured between 2014 and 2018. We measure the outcomes at age 21.

-   c21_high_school_attained: Indicates whether the individual has achieved a basic high school qualification (havo, vwo, or mbo-2) by the age of 21.

-   c21_hbo_followed: Indicates whether the individual has pursued higher professional education (HBO) or university education by the age of 21.

-   c21_uni_followed: Indicates whether the individual has pursued university education by the age of 21.

-   c21_young_parents: Indicates whether the individual had a child before the age of 20.

-   c21_living_with_parents: Indicates whether the individual lives at parental home.

-   c21_specialist_mhc: Indicates whether an individual utilized specialist care within the basic insurance at the age of 20.

-   c21_basic_mhc: Indicates whether an individual utilized generalist basic mental healthcare within the basic insurance at the age of 20.

-   c21_pharma: Indicates whether an individual utilized pharmacy services within the basic insurance at the age of 20.

-   c21_hospital: Indicates whether an individual utilized hospital care within the basic insurance at the age of 20.

-   c21_total_health_costs: Represents the sum of healthcare costs at the age of 20, measured in 2020 euros. c21_primary_class_foreign_born_parents: Represents the percentage of individual’s classmates in grade 8 of primary school whose both parents were born abroad.

-   c21_primary_class_income_below_25th: Represents the percentage of individual’s classmates in grade 8 of primary school whose parents’ income is below the 25th percentile of the parental income distribution.

-   c21_primary_class_income_below_50th: Represents the percentage of individual’s classmates in grade 8 of primary school whose parents’ income is below the 50th percentile of the parental income distribution.

-   c21_primary_class_income_above_75th: Represents the percentage of individual’s classmates in grade 8 of primary school whose parents’ income is above the 75th percentile of the parental income distribution.

-   c21_secondary_class_foreign_born_parents: Represents the percentage of individual’s classmates whose both parents were born abroad in grade 4 of secondary school.

-   c21_secondary_class_income_below_25th: Represents the percentage of individual’s classmates whose parents’ income is below the 25th percentile of the parental income distribution in grade 4 of secondary school.

-   c21_secondary_class_income_below_50th: Represents the percentage of individual’s classmates whose parents’ income is below the 50th percentile of the parental income distribution in grade 4 of secondary school.

-   c21_secondary_class_income_above_75th: Represents the percentage of individual’s classmates whose parents’ income is above the 75th percentile of the parental income distribution in grade 4 of secondary school.

-   c21_age12_neighborhood_income_below_25th: Represents the percentage of individual’s neighbors at age 12 whose parents’ income is below the 25th percentile of the parental income distribution.

-   c21_age12_neighborhood_income_below_50th: Represents the percentage of individual’s neighbors at age 12 whose parents’ income is below the 50th percentile of the parental income distribution.

-   c21_age12_neighborhood_income_above_75th: Represents the percentage of individual’s neighbors at age 12 whose parents’ income is above the 75th percentile of the parental income distribution.

-   c21_age12_neighborhood_foreign_born_parents: Represents the percentage of individual’s neighbors at age 12 whose parents are foreign born.

-   c21_age16_neighborhood_income_below_25th: Represents the percentage of individual’s neighbors whose parents’ income is below the 25th percentile of the parental income distribution at age 16.

-   c21_age16_neighborhood_income_below_50th: Represents the percentage of individual’s neighbors whose parents’ income is below the 50th percentile of the parental income distribution at age 16.

-   c21_age16_neighborhood_income_above_75th: Represents the percentage of individual’s neighbors whose parents’ income is above the 75th percentile of the parental income distribution at age 16.

-   c21_age16_neighborhood_foreign_born_parents: Represents the percentage of individual’s neighbors whose parents are foreign born at age 16.

#### 6. Age35 sample

-   The age35 sample consists of individuals around age of 35 born between 1984 and 1988. These individuals are assigned to their home addresses based on the address on their 15th birthday. Parental income is measured between 2003 and 2007. We measure their outcomes at the age of 34-35. c35_hbo_attained: Indicates whether an individual attained at least an HBO degree (higher professional education) by the age of 35.

-   c35_wo_attained: Indicates whether an individual attained at least a university degree by the age of 35.

-   c35_specialist_mhc: Indicates whether an individual utilized specialist care within the basic insurance at the age of 34.

-   c35_basic_mhc: Indicates whether an individual utilized generalist basic mental healthcare within the basic insurance at the age of 34.

-   c35_mhc: Indicates whether an individual utilized any mental healthcare services within the basic insurance at the age of 34.

-   c35_pharma: Indicates whether an individual utilized pharmacy services within the basic insurance at the age of 34.

-   c35_hospital: Indicates whether an individual utilized hospital care within the basic insurance at the age of 34.

-   c35_total_health_costs: Represents the sum of healthcare costs at the age of 34, measured in 2020 euros.

-   c35_household_income: Represents the average income at the household level when the child is aged 34–35.

-   c35_top_20_household_income: Indicates whether the individual’s household income is in the top 20th percentile of the household income distribution.

-   c35_bottom_20_household_income: Indicates whether the individual’s household income is in the bottom 20th percentile of the household income distribution.

-   c35_household_below_poverty: Indicates whether an individual’s income was below the poverty threshold at ages 34 and 35.

-   c35_disposable_household_income: Represents the average disposable income (gross income less transfers and taxes) at the household level when the child is aged 34–35.

-   c35_primary_household_income: Represents the average primary income at the household level when the child is aged 34–35.

-   c35_income: Represents the average pre-tax income between the ages of 34 and 35, measured in XXXX euros.

-   c35_top_20_individual_income: Indicates whether the individual’s income is in the top 20th percentile of the individual income distribution.

-   c35_bottom_20_individual_income: Indicates whether the individual’s income is in the bottom 20th percentile of the individual income distribution.

-   c35_earnings: Represents the average primary income between the ages of 34 and 35, measured in XXXX euros.

-   c35_employed: Indicates whether an individual was employed at the age of 35.

-   c35_disability: Indicates whether an individual received disability benefits at the age of 35.

-   c35_social_assistance: Indicates whether an individual received social benefits at the age of 35.

-   c35_wealth: Represents the total wealth of a household at the age of 35.

-   c35_wealth_no_home: Represents the total wealth of a household excluding the value of the house they own at age 35.

-   c35_home_wealth: Represents the wealth of the house that remains after deducting the mortgage debt at age 35.

-   c35_debt: Represents the total debt of a household at the age of 35.

-   c35_gifts_received: Indicates whether an individual received a gift between the ages of 26 and 35.

-   c35_sum_gifts: Represents the total sum of the gifts received between the ages of 26 and 35.

-   c35_hrs_work_pw: Represents the average total hours worked per week at the age of 35.

-   c35_permanent_contract: Indicates whether an individual had a permanent contract instead of a temporary contract at the age of 35.

-   c35_hourly_wage: Represents the average hourly wage earned at the age of 35, measured in 2020 euros.

-   c35_hourly_wage_max_16: Indicates whether an individual had an hourly wage of 16 euros or below at the age of 35.

-   c35_homeowner: Indicates whether an individual was a homeowner at the age of 35.

-   c35_living_space_pp: Represents the average living space per household member at the age of 35.

-   c35_age_left_parents: Represents the age at which an individual stopped living at the parental home.

### How to re-create

Code for the samples creation is provided as part of this replication package. It is available [here]((Zenodo%20link)). Description follows in section Description of code.

## Computational requirements

### Software Requirements

-   R 3.4.3

    -   renv (1.1.4): Run renv::restore() to install the right versions of all dependencies

### Memory and Runtime Requirements

Approximate time needed to reproduce the analyses on a standard (2025) desktop machine:

-   7 days

## Description of code

Here the code of this repository is described. There are 39 files in total: configuration `.yml`, `01_cohort.R`, `02_predictors.R`, `03_outcomes.R`, `04_postprocessing.R` files for each of the 7 samples, and 2 auxiliary files.

### Auxiliary code (Provided)

| Script Name | Purpose |
|------------------------------------|------------------------------------|
| `create_cbs_countries.R` | Generates CBS-style country classification tables |
| `set_cbs_versions.R` | Configures version control for CBS files |

### **Sample Creation**

To create a sample dataset use `create_cohort_data.R`, change the desired input configuration to one of the following:

-   `config/newborns.yml`

-   `config/prim8.yml`

-   `config/age16.yml`

-   `config/age21.yml`

-   `config/age35.yml`,

    and run the `create_cohort_data.R` script. The sample dataset will be written to the scratch folder.

### **Flowchart Sample Creation**

![](images/Opportunity%20Map%20Preprocessing%20Flowchart.png)

### 1. **Configuration**

The configuration files for each of the samples can be found in the `config` folder. Here sample-dependent values are defined, such as:

-   Sample name and birthdate range filters (`child_birth_date_min` and `child_birth_date_max`)

-   Residency rules, including age ranges and slack days for registration gaps (`child_live_age`, `child_live_slack_days`)

-   Measurement ages for childhood address and outcome definitions (`childhood_home_age`, `outcome_age`)

-   Reference years for parental income and wealth data

-   Other year ranges for sample-specific outcomes

-   CPI base year used for deflating income

-   File paths to raw CBS microdata and auxiliary resources such as price indices, migration codes, and regional mappings

### 2. **Pipeline**

Here the steps to create the sample data are described.

#### 1. **Define sample**

In this step, we select the individuals from the register that form our samples.

-   Define cohort which consists of individuals registered in the Netherlands and born within a specific birth date interval (`child_birth_date_min` - `child_birth_date_max`)\* (GBAPERSOONTAB).

    -   \*For samples for which we analyze a number of classroom composition outcomes, the birthdate range is increased by 2 years on both sides to include potential classmates.

-   Match individuals to their legal parents (KINDOUDERTAB). We exclude individuals who cannot be matched to their legal parents.

-   Link individuals to their childhood home address based on `childhood_home_age` (GBAADRESOBJECTBUS). We exclude individuals who cannot be linked to their childhood home address.

    -   In samples for which we analyze youth protection outcome, we additionally link individuals to their birth address

-   Add geographical area variables (`postcode3`, `postcode4`(VSLPOSTCODEBUS), `municipality_code`(VSLGWBTAB), `neighborhood_code`, `corop_code` ([`gemeenten_corop_1981.xlsx`](https://github.com/Netherlands-Opportunity-Lab/KansenKaart-Preprocessing-fork/blob/resources/gemeenten_corop_1981.xlsx))) based on childhood address. We exclude individuals whose childhood address cannot be linked to a municipality or 4-digit postcode.

    -   Geographical location definitions in the Netherlands can vary from year to year. Therefore, we use a specific target date (`postcode_target_date` & `gwb_target_date`) to determine the geographical location
    -   For samples that include youth protection outcomes we create geographical area variables measured at birth
    -   For samples that include neighborhood composition outcomes and correlates we measure 4-digit postcode at the age the neighborhood composition is measured

-   Write the resulting sample (`01_cohort.rds)` dataset to *the scratch folder.*

-   At each step we record the sample sizes.

#### 2. **Add predictors**

In this step, we incorporate variables from various microdata sets to the sample, which serve as predictors for our estimates: parental income/wealth/education\*, migration background, household composition

\*parental education is only added to newborns and prim8 samples due to data availability

-   We check whether the parents meet the residency requirement. We exclude the children whose parents did not live continuously in the Netherlands between the years in which we define parental income of the children allowing for a number of slack days defined by `child_live_slack_days`.

-   Parental income: We obtain information on parental income during individual’s childhood from IPI (\<2011) and INPATAB (≥2011). Parental income represents the average pretax income of the mother and/or father combined over a specific period interval (`parent_income_year_min` - `parent_income_year_max`). We exclude individuals whose parents do not have observable income within the specified interval.

    -   We apply several restrictions to the income definition:

        1\. Income values of 999999999 (indicating a household with no perceived income) or negative income values (below 0) are converted to NA (not available).

        2\. Parental income is adjusted for inflation using the consumer price index (CPI) from CBS Statline (cpi_base_year).

    -   We compute parental income rank (in percentile) in the parental income distribution.

-   \*For the samples for which we measure classroom and neighborhood composition: write classroom and neighborhood samples to the scratch folder.

-   Parental wealth: We obtain information on parental wealth during individual’s childhood from the VEHTAB. Parental wealth represents the average pretax wealth of the household’s head (if these are different for parents, we use the total) combined over a specific period interval (`parent_wealth_year_min` - `parent_wealth_year_max`).

    -   The same restrictions applied to parental income are also imposed on parental wealth.

    -   We compute parental wealth rank (in percentile) in the parental income distribution.

-   \*Parental education: We obtain information on the parents’ highest achieved level of education during individual’s childhood from HOOGSTOPLTAB. We categorize education into 3 levels: University, Higher Professional Education, and Other (including NA). The parental education level is determined by the highest educational attainment between the father and the mother.

    -   \*For newborns and prim8 samples

    -   This is due to the availability of data on parental education, which began in 1983 for WO, 1986 for HBO, and 2004 for MBO (intermediate vocational education).

-   We introduce a third generation to the variable `GBAGENERATIE` from the GBAPERSOONTAB microdata for individuals in the cohort. If a child is native (autochtoon) and at least one of their parents is a second-generation immigrant (tweede generatie allochtoon), we reclassify the child’s generation as a third-generation immigrant (derde generatie allochtoon). Essentially, we change the child’s generation from native to third generation if at least one parent is a second-generation immigrant and the child is native.

    -   For children who are third-generation immigrants, we recode their origin (variable `GBAHERKOMSTGROEPRING`) as follows:

        1.  If the child is third generation and the father’s origin is native, the child adopts the mother’s origin.
        2.  If the child is third generation and the mother’s origin is native, the child adopts the father’s origin.
        3.  If the child is third generation and both parents’ origins are non-native, the child adopts the mother’s origin.

-   Migration background: We obtain child’s migration background data from the GBAPERSOONTAB. We categorize migration background into 6 levels:

    1.  No Migration Background
    2.  Turkey
    3.  Morocco
    4.  Suriname
    5.  Dutch Caribbean
    6.  With Migration Background

-   Type of household: We obtain the type of household a child was growing up in from GBAHUISHOUDENSBUS. We categorize this into 2 levels: Single Parent and Two Parents.

-   Write the resulting sample (`02_predictors.rds)` dataset to *the scratch folder.*

-   At each step we record the sample sizes.

#### 3. **Add outcomes**

In this step, we connect sample-specific outcomes to the samples.

-   We check whether the individual meets the residency requirement. We exclude the children who did not live continuously in the Netherlands within a specified period (`child_live_start` - `child_live_end`) allowing for a number of slack days defined by `child_live_slack_days`.

-   We add sample-specific outcomes as described in [Samples and Outcomes](#samples-and-outcomes).

-   Write the resulting sample (`03_outcomes.rds)` dataset to *the scratch folder.*

-   At each step we record the sample sizes.

#### 4. **Post-processing**

In this step, we select variables from the created sample data sets that are relevant for our estimates.

-   Categorize individuals into income (wealth) groups based on parental income (wealth ) percentile:

    1.  15 ≤ Percentile ≤ 35: “Low”
    2.  40 ≤ Percentile ≤ 60: “Mid”
    3.  65 ≤ Percentile ≤ 85: “High”

-   Categorize individuals into income (wealth) distribution’s tail groups based on the same percentile distribution:

    1.  0 ≤ Percentile ≤ 20: “Very Low”
    2.  80 ≤ Percentile ≤ 100: “Very High”

-   Write sample dataset `sample-name_cohort.rds` to the scratch folder.

## Instructions to Replicators

-   Clone this repository to a folder on your machine
-   Edit the file locations in the `config` files to point to the right files
-   Open the `.Rproj` file, install the `renv` package
-   Run `renv::restore()` to install the right versions of all dependencies

# References

Lars Vilhuber, Connolly, M., Koren, M., Llull, J., & Morrow, P. (2022). A template README for social science replication packages (v1.1). Social Science Data Editors. <https://doi.org/10.5281/zenodo.7293838>
