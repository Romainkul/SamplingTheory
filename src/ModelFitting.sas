/*-----------------------------------------*/
/* 5. Survey-Weighted Regression (BY Imputation)
/*    – Run regression separately within each imputed data set,
       accounting for the survey design (clusters & weights)    */
/*-----------------------------------------*/
proc surveyreg data=bhisdata.mi_bhis_final;
  by _Imputation_;                 /* loop over each imputation */
  cluster HH;                      /* account for household clustering */
  weight WFIN;                     /* apply survey weight */
  strata PROVINCE;
  class SGP EDU3(ref='2') SEX TA2 AGE7(ref='3') FA3(ref='2'); /* declare categorical predictors */
  /* fit model and request parameter estimates */
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
       / solution;
  ods output ParameterEstimates=PE_svy; /* save estimates for later pooling */
run;

/*-----------------------------------------*/
/* 5b. Clean up and reshape parameter estimates
   – create “safe” variable names, then
     transpose to wide form (one row per imputation) */
/*-----------------------------------------*/
data PE_svy_clean;
  set PE_svy;
  /* replace spaces with underscores, remove special chars */
  safe1 = translate(Parameter, '_', ' ');
  safe  = compress(safe1, , 'kw');  
  /* build E_ and SE_ variable names for transposing */
  varE  = cats('E_',  safe);
  varSE = cats('SE_', safe);
run;

/* transpose Estimates into columns E_<parm> */
proc transpose data=PE_svy_clean 
               out=PE_est(drop=_NAME_);
  by _Imputation_;
  var Estimate;
  id varE;
run;

/* transpose StdErr into columns SE_<parm> */
proc transpose data=PE_svy_clean
               out=PE_se(drop=_NAME_);
  by _Imputation_;
  var StdErr;
  id varSE;
run;

/* merge estimates and standard errors into one wide table */
data PE_svy_wide;
  merge PE_est PE_se;
  by _Imputation_;
run;

/*-----------------------------------------*/
/* 5c. Dynamically grab the list of E_ and SE_ columns
   – build macro variables &parmlist and &selist */
/*-----------------------------------------*/
proc sql noprint;
  /* all columns beginning with “E_” */
  select name into :parmlist separated by ' '
  from dictionary.columns
  where libname='WORK'
    and memname='PE_SVY_WIDE'
    and upcase(name) like 'E\_%' escape '\';

  /* all columns beginning with “SE_” */
  select name into :selist separated by ' '
  from dictionary.columns
  where libname='WORK'
    and memname='PE_SVY_WIDE'
    and upcase(name) like 'SE\_%' escape '\';
quit;

/* print for debugging */
%put &=parmlist;
%put &=selist;

/* pool the survey estimates across imputations */
proc mianalyze data=PE_svy_wide;
  modeleffects &parmlist;  /* parameter estimates */
  stderr     &selist;      /* standard errors */
run;


/*-----------------------------------------*/
/* 6. Mixed-Effects Model (BY Imputation)
/*    – include random intercept for HH, 
       request both estimates and covariance matrix */
/*-----------------------------------------*/
proc mixed data=bhisdata.mi_bhis_final method=REML;
  by _Imputation_;                 /* loop over imputations */
  class SGP EDU3(ref='2') SEX TA2 AGE7(ref='3') FA3(ref='2'); /* categorical predictors */
  /* fixed-effects model, also output covariance of estimates */
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
       / solution covb;
  random intercept / subject=HH;    /* random intercept by household */
  weight WFIN;                     /* apply survey weight */
  ods output 
    SolutionF = PE_mix      /* fixed-effects estimates */
    CovParms  = CP_mix      /* variance components */
    CovB      = PE_mix_cov; /* covariance matrices of estimates */
run;

data PE_mix_cov_non_sing;
  set PE_mix_cov;
  if Row in (5 12 14 16 19 22) then delete;
  drop Col5 Col12 Col14 Col16 Col19 Col22;
run;

data PE_mix_cov_final;
  set PE_mix_cov_non_sing;
  rename Col6=Col5 Col7=Col6 Col8=Col7 Col9=Col8 Col10=Col9 Col11=Col10 Col13=Col11 Col15=Col12 Col17=Col13 Col18=Col14 Col20=Col15 Col21=Col16 Col23=Col17 Col24=Col18;
  Row = mod(_N_ - 1, 18) + 1;
run;

data PE_mix_non_sing;
  set PE_mix;
  by _Imputation_;
  if first._Imputation_ then recnum = 0;
  recnum + 1;
  if recnum in (5,12,14,16,19,22) then delete;
  drop recnum;
run;

/* pool mixed-model estimates using MIANALYZE */
proc mianalyze parms=PE_mix_non_sing 
                 covb(effectvar=rowcol)=PE_mix_cov_final;
  class SGP EDU3 SEX TA2 AGE7 FA3; /* list any classification vars */
  modeleffects 
    Intercept
    GHQ12_scaled
    BMI_scaled
    SGP
    AGE7
    SEX
    TA2
    EDU3
    FA3
    BMI2
    GHQ12_2;
run;

/*-----------------------------------------*/
/* 7. ICC Computation (Empty vs. Full Models)
/*    – calculate intra-class correlation 
       from variance components                */
/*-----------------------------------------*/
/* 7a. Fit null (empty) model with only random intercept */
proc mixed data=bhisdata.mi_bhis_final method=REML;
  by _Imputation_;
  class HH;
  model LNVOEG_scaled = / solution;  /* no predictors */
  random intercept / subject=HH;      /* random intercept */
  ods output CovParms=CP_empty;      /* save null variance comps */
run;

/* 7b. Extract Tau2 (between-HH) and Sigma2 (residual) from full model */
proc sql;
  create table ICC_full as
  select _Imputation_,
         estimate as Tau2
    from CP_mix
   where CovParm='Intercept';

  create table ICC_resid as
  select _Imputation_,
         estimate as Sigma2
    from CP_mix
   where CovParm='Residual';

  /* extract same components from the null model */
  create table ICC_null as
  select e._Imputation_,
         e.estimate as Tau2_null,
         r.estimate as Sigma2_null
    from CP_empty e
    join CP_empty r
      on e._Imputation_ = r._Imputation_
   where e.CovParm='Intercept'
     and r.CovParm='Residual';
quit;

/* 7c. Compute ICCs for each imputation */
data ICC_compare;
  merge ICC_full ICC_resid ICC_null;
  by _Imputation_;
  /* ICC_full  = between / (between + within) for full model */
  ICC_full = Tau2 / (Tau2 + Sigma2);
  /* ICC_null  = same for empty model */
  ICC_null = Tau2_null / (Tau2_null + Sigma2_null);
run;

/* summarize the distribution of ICCs */
proc means data=ICC_compare n mean min max std;
  var ICC_full ICC_null;
  title "ICC: Null vs. Full Models";
run;


/*-----------------------------------------*/
/* 9. Save Results to Permanent Library
   – store all key output for reporting    */
/*-----------------------------------------*/
data results.ICC_compare; set ICC_compare; run;
data results.PE_svy;       set PE_svy;       run;
data results.PE_mix;       set PE_mix;       run;
data results.CP_mix;       set CP_mix;       run;
