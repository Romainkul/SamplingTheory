/*-----------------------------------------*/
/* 10. Set Up ODS and Graphics Options
   – close default listing, direct graphs to a folder,
     enable ODS Graphics with adjusted limits      */
/*-----------------------------------------*/
ods listing close;  
ods listing gpath="/home/u64027551/SamplingTheory/figures";  /* set output directory for figures */

ods graphics on 
    / reset=all               /* reset all graphics settings */
      imagemap                /* enable interactive image maps */
      loessmaxobs=10000       /* allow up to 10,000 points for LOESS smoothing */
      antialiasmax=9000       /* allow up to 9,000 points for antialiasing */
    ;

/*-----------------------------------------*/
/* 11. Mixed-Effects Model Diagnostics (Imputation 1)
   – plot raw and conditional residual panels
     to check model fit and assumptions         */
/*-----------------------------------------*/
proc mixed data=bhisdata.mi_bhis_final 
           method=REML 
           plots(maxpoints=none)=(residualpanel(marginal conditional));
  where _Imputation_ = 1;    /* limit to the first imputation for diagnostics */
  class HH SGP EDU3(ref='2') SEX TA2 AGE7(ref='3') FA3(ref='2');                  /* declare clustering variable for random intercept */
  model LNVOEG_scaled = 
        GHQ12_scaled 
        BMI_scaled 
        SGP 
        AGE7 
        SEX 
        TA2 
        EDU3 
        FA3 
        BMI2 
        GHQ12_2
      / solution                /* show fixed-effects estimates */
        residual                /* request raw residuals output for plots */
  ;
  random intercept / subject=HH;  /* include random intercept for household clustering */
run;

/*-----------------------------------------*/
/* 12. Survey-Weighted Regression (Imputation 1)
   – fit survey model, output predictions & residuals */
/*-----------------------------------------*/
proc surveyreg data=bhisdata.mi_bhis_final;
  where _Imputation_ = 1;    /* focus on the first imputation */
  cluster HH;                /* account for household clustering */
  weight WFIN;               /* apply survey weight */
  class SGP EDU3(ref='2') SEX TA2 AGE7(ref='3') FA3(ref='2');  /* categorical predictors */
  model LNVOEG_scaled = 
        GHQ12_scaled 
        BMI_scaled 
        SGP 
        AGE7
        SEX 
        TA2 
        EDU3 
        FA3
        BMI2 
        GHQ12_2
      / solution              /* display parameter estimates */
  ;
  output out=svy_out 
         predicted=pred       /* save fitted values as pred */
         residual=resid       /* save residuals as resid */
  ;
run;

/*-----------------------------------------*/
/* 13. Residuals vs. Fitted Values Plot
   – scatter raw residuals and add LOESS smoother  */
/*-----------------------------------------*/
proc sgplot data=svy_out;
  scatter x=pred y=resid;                     /* raw residuals vs predicted */
  loess   x=pred y=resid / nomarkers;         /* LOESS curve with no marker overlay */
  refline 0    / axis=y lineattrs=(pattern=shortdash);  /* horizontal zero line */
  xaxis label="Fitted Value";                 
  yaxis label="Residual";                     
  title "PROC SURVEYREG: Residuals vs Fitted (Imputation 1)";
run;

proc sgplot data=svy_out;
  scatter x=SEX y=resid; /* variable set to sex, but can change to have better view */
  loess   x=SEX y=resid / nomarkers;
  refline 0 / axis=y lineattrs=(pattern=shortdash);
  xaxis label="Variable";
  yaxis label="Residual";
  title "Residuals vs Variable (Imp 1)";
run;


/*-----------------------------------------*/
/* 14. QQ Plot of Residuals
   – assess normality of survey-weighted residuals   */
/*-----------------------------------------*/
proc univariate data=svy_out noprint;
  qqplot resid / normal(mu=est sigma=est);   /* QQ plot against estimated normal distribution */
  title "PROC SURVEYREG: QQ Plot of Residuals";
run;

/*-----------------------------------------*/
/* 15. Histogram & Density of Residuals
   – visualize distribution and overlay normal density */
/*-----------------------------------------*/
proc sgplot data=svy_out;
  histogram resid;                            /* histogram of residuals */
  density   resid / type=normal;              /* overlay normal density curve */
  title "PROC SURVEYREG: Histogram of Residuals (Imputation 1)";
run;

/*-----------------------------------------*/
/* 16. Reset ODS Graphics and Listing
   – turn off graphics destination and restore default listing */
/*-----------------------------------------*/
ods graphics off;  
ods listing;  

/*-----------------------------------------*/
/* 17. Breusch–Pagan Test (SurveyReg)      */
/*-----------------------------------------*/
/* 17a. Square residuals */
data bp_svy;
  set svy_out;
  resid2 = resid**2;
run;

/* 17b. Aux regression no intercept */
proc surveyreg data=bp_svy;
  cluster HH;
  weight WFIN;
  model resid2 = pred / noint;
  ods output FitStatistics=BP_svy_stats;
run;

/* 17c. Extract R-Square (from Label1/NValue1) */
data _null_;
  set BP_svy_stats;
  where upcase(Label1)="R-SQUARE";
  call symputx('R2_svy', NValue1);
run;

/* 17d. Count observations */
proc sql noprint;
  select count(*) into :N_svy from bp_svy;
quit;

/* 17e. Compute BP statistic & p-value */
data BP_svy_test;
  length Test $32;
  R2      = &R2_svy;
  N       = &N_svy;
  BP      = N * R2;
  df      = 1;
  p_value = 1 - probchi(BP, df);
  Test    = "BP (SurveyReg)";
  keep Test BP df p_value;
run;

proc print data=BP_svy_test noobs label;
  label
    BP      = "BP Statistic"
    df      = "df"
    p_value = "Pr > χ²";
  title "Breusch–Pagan Test for SurveyReg (Imp 1)";
run;