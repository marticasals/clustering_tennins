If you use this software, please cite the manuscript:

Fernández, D., Epifanio, I., McMillan, L. Archetypal Analysis for Ordinal Data.

Fernández, D., Arnold, R., & Pledger, S. (2016). Mixture-based clustering for the ordered stereotype model. Computational Statistics & Data Analysis, 93, 46-75.

Matechou, E., Liu, I., Fernández, D., Farias, M., & Gjelsvik, B. (2016). Biclustering models for two-mode ordinal data. Psychometrika, 81(3), 611-624.



This code gives the results of Section "Questionnaires Responses of Patients Affected by Breast Cancer".

DATA:
dataqol_classif_imp.csv: original data set with imputed values
dataqol_classif_def.csv: data set used in the step 2

FUNCTIONS:
Step1_and_MI.R: it contains the functions to impute the missing data and computing the step 1.
ada_col.R: it contains the functions to compute ADA with the data set (step 2).

COMPARISON: it contains files for making the comparison with PAA and PAM
make_dummy.R: it converts data to be reading for PAA function. It returns the file Xd.txt
dataqol_paa_nom.m: it contains the functions to compute PAA. It needs the functions for PAA in https://github.com/aalab/naa/
cluster_pam.R: it contains the functions to compute PAM.


AUXILIARY: it contains files for computing ADA


