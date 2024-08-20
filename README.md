# bFCA-model-ART-MWI

The Input files are synthetic data to exhibit how our code works.

**Input Files**

**Origin-Destination Matrix** (Example_OD_1.csv): This file contains the travel time in minutes between population centroids (rows) and healthcare facilities (columns).

**Population Centroids File** (PC_PLHIV.csv): This file contains data on population centroids, including their IDs and the number of people living with HIV (PLHIV).

**Healthcare Facility Supply File** (HCF_supply.csv): This file contains data on the ART supply available at each healthcare facility.

**Output Files**

The script outputs two data frames:
**1. Accessibility Index of Population Centroids**: A data frame with the following columns:

**population_centroid_code**: The ID of the population centroid.

**number_PLHIV**: The number of people living with HIV at the centroid.

**access**: The accessibility index for the centroid.

**2. Healthcare Facility Service Level**: A data frame with the following columns:

**HCF_code**: The ID of the healthcare facility.

**ART_supply**: The supply of ART at the facility.

**Lj**: The level of service (Lj) for each healthcare facility.

**Dj**: The demand vector (Dj) representing the total demand for ART from nearby population centroids.

**Acknowledgments**
This script was developed by Joan Ponce, Ph.D., as part of a project analyzing healthcare accessibility. Special thanks to the Blower Lab for providing the data and resources necessary for this analysis.
