/*-----------------------------------------
  0. Setup & Import Data
-----------------------------------------*/
libname bhisdata  "/home/u64027551/SamplingTheory/data";
libname results   "/home/u64027551/SamplingTheory/data";

ods listing close;
ods listing gpath="/home/u64027551/SamplingTheory/figures";

ods graphics on / reset=all imagemap;

data bhisdata.bhis;
  set bhisdata.bmi_voeg;
run;

/*-----------------------------------------
  1. Variable Formats & Contrasts
-----------------------------------------*/
proc format;
  value EDU3fmt 1='Primary'      2='Secondary'      3='Higher';
  value SEXfmt  1='Male'         2='Female';
  value FA3fmt  1='<30000'       2='30000-40000'    3='>40000';
  value TA2fmt  1='Non-smoker'   2='Smoker';
  value SGPfmt  0='No'           1='Yes';
run;

/*-----------------------------------------
  2. Prepare Raw Data for MI 
-----------------------------------------*/
data bhis_prep;
  set bhisdata.bhis;
  keep GHQ12 BMI LNVOEG
       WFIN HH AGE7 EDU3 SEX FA3 TA2 SGP PROVINCE;
run;

/*-----------------------------------------
  3. Descriptive & Missing‐value Counts 
-----------------------------------------*/
proc means data=bhis_prep n mean std min max nmiss;
  var GHQ12 BMI LNVOEG;
  title "Continuous Variables (Raw) with Missing Counts";
run;

proc freq data=bhis_prep;
  tables EDU3 SEX AGE7 FA3 TA2 SGP / missing;
  format EDU3 EDU3fmt. SEX SEXfmt. FA3 FA3fmt. TA2 TA2fmt. SGP SGPfmt.;
  title "Categorical Variables (Raw) with Missing Counts";
run;

/*-----------------------------------------
  4. Multiple Imputation with FCS
-----------------------------------------*/
proc mi data=bhis_prep
        out=mi_bhis
        seed=123
        nimpute=10;
  class EDU3 SEX TA2 SGP AGE7 FA3;
  fcs nbiter=100;

  /* PMM for continuous */
  fcs regpmm(GHQ12 BMI LNVOEG);

  /* logistic for categorical */
  fcs logistic(SGP SEX TA2 EDU3 FA3);
	
  /* plots */
  fcs outiter=mi_bhis_out plots=trace(mean std); 
  var GHQ12 BMI LNVOEG
      WFIN HH AGE7 EDU3 SEX FA3 TA2 SGP PROVINCE;
run;

ods graphics off;
ods listing; 
/*-----------------------------------------
  5. Post‐Imputation: Standardize & Square
-----------------------------------------*/
/* 5a) Standardize GHQ12, BMI, LNVOEG across each imputation */
proc stdize
    data=mi_bhis
    out=mi_bhis_std
    method=std
    replace;
  var GHQ12 BMI LNVOEG;
run;

/* 5b) Rename, label, and compute quadratic terms */
data bhisdata.mi_bhis_final;
  set mi_bhis_std;

  /* rename standardized vars */
  GHQ12_scaled  = GHQ12;
  BMI_scaled    = BMI;
  LNVOEG_scaled = LNVOEG;

  /* compute squares */
  GHQ12_2 = GHQ12_scaled**2;
  BMI2    = BMI_scaled**2;

  /* labels & formats */
  label
    GHQ12_scaled  = 'GHQ-12 Score (standardized)'
    BMI_scaled    = 'BMI (standardized)'
    LNVOEG_scaled = 'Income (log, standardized)';
  format
    GHQ12_scaled  6.3
    BMI_scaled    6.3
    LNVOEG_scaled 6.3;

  keep
    _Imputation_
    GHQ12_scaled BMI_scaled LNVOEG_scaled
    GHQ12_2 BMI2
    WFIN HH AGE7 EDU3 SEX FA3 TA2 SGP PROVINCE;
run;
