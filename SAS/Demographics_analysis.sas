
/******************************************************************************
* PROGRAM      : demographics_table.sas
*
* PURPOSE      : Generate a Demographic and Baseline Characteristics Table
*                using an ADSL-style ADaM dataset.
*
* ANALYSIS SET : Full Analysis Set (FAS)
*
* AUTHOR       : Javan Mukunzi
*
* OUTPUT       : demographics_table.pdf

/******************************************************************************
* STEP 1: SET UP SAS ENVIRONMENT
*
* The input library should point to the location of the analysis datasets.
* For this public portfolio version, a relative path is used rather than
* an internal company/server directory.
******************************************************************************/

%let outpath = ./output;

/* Update this path when running the program in your own environment. */
libname adam './data' access=readonly;


/******************************************************************************
* STEP 2: PREPARE THE ANALYSIS POPULATION
*
* ADSL is the main ADaM subject-level dataset used for demographic tables.
*
* The Full Analysis Set flag (FASFL) is used to select subjects who belong
* to the predefined analysis population.
*
* Only variables required for this table are retained.
******************************************************************************/

data adsl_fas;

    set adam.adsl;

    /* Select subjects belonging to the Full Analysis Set */
    where fasfl = 'Y';

    /* Retain only variables required for the demographic table */
    keep usubjid
         trt01a
         age
         sex
         agegr1
         race
         ethnic;

    /* Apply descriptive labels for easier interpretation */
    label
        usubjid = 'Unique Subject ID'
        trt01a  = 'Actual Treatment'
        age     = 'Age (years)'
        agegr1  = 'Age Group (years)'
        sex     = 'Sex'
        race    = 'Race'
        ethnic  = 'Ethnicity';

run;


/******************************************************************************
* STEP 3: BASIC POPULATION QC
*
* Before generating the table, verify the number of subjects contributing
* to each treatment group.
*
* COUNT(DISTINCT USUBJID) is used to ensure that each subject is counted
* only once.
******************************************************************************/

proc sql;

    create table fas_counts as

    select
        trt01a as treatment,
        count(distinct usubjid) as subjectcount

    from adsl_fas

    group by trt01a

    order by trt01a;

quit;


/* Display the analysis population counts for QC */

title "Full Analysis Set Population Counts by Treatment";

proc print data=fas_counts;
run;

title;


/******************************************************************************
* STEP 4: DERIVE TREATMENT DENOMINATORS
*
* The treatment denominator represents the number of subjects in each
* treatment group.
*
* These denominators will later be used to calculate percentages for
* categorical demographic variables.
******************************************************************************/

proc sql;

    create table trt_denom as

    select
        trt01a,
        count(distinct usubjid) as denom

    from adsl_fas

    group by trt01a;

quit;


/******************************************************************************
* STEP 5: DERIVE OVERALL DENOMINATOR
*
* The overall denominator represents all subjects in the Full Analysis Set,
* regardless of treatment group.
******************************************************************************/

proc sql;

    select count(distinct usubjid)
        into :overall_n

    from adsl_fas;

quit;


/* Write the overall denominator to the SAS log for QC */

%put NOTE: Overall FAS N = &overall_n;


/******************************************************************************
* STEP 6: SUMMARIZE CONTINUOUS VARIABLE - AGE
*
* PROC MEANS is used to calculate:
*
* N
* Mean
* Standard deviation
* Median
* Minimum
* Maximum
*
* The CLASS statement produces these statistics separately for each
* treatment group.
******************************************************************************/

proc means data=adsl_fas
           nway
           noprint
           n
           mean
           std
           median
           min
           max;

    class trt01a;

    var age;

    output out=age_stats(drop=_freq_ _type_)
        n(age)      = n
        mean(age)   = mean_age
        std(age)    = sd_age
        median(age) = median_age
        min(age)    = min_age
        max(age)    = max_age;

run;


/******************************************************************************
* STEP 7: PREPARE AGE STATISTICS FOR REPORTING
*
* PROC MEANS produces one observation containing multiple statistics.
*
* For reporting, the data are converted into multiple rows:
*
* Age
*   N
*   Mean
*   SD
*   Median
*   Min
*   Max
*
* This long structure makes it easier to construct the final TLF.
******************************************************************************/

data age_reporting;

    set age_stats;

    length
        param     $40
        stat_type $15
        trt_value $20;

    /* Round continuous statistics to one decimal place */

    mean_age   = round(mean_age,   0.1);
    sd_age     = round(sd_age,     0.1);
    median_age = round(median_age, 0.1);


    /*---------------------------------------------------------------
      Reporting Row 1: N
    ----------------------------------------------------------------*/

    ord       = 2;
    param     = 'Age (years)';
    stat_type = 'N';
    trt_value = put(n, 3.0);

    output;


    /*---------------------------------------------------------------
      Reporting Row 2: Mean
    ----------------------------------------------------------------*/

    ord       = 3;
    stat_type = 'Mean';
    trt_value = put(mean_age, 5.1);

    output;


    /*---------------------------------------------------------------
      Reporting Row 3: Standard Deviation
    ----------------------------------------------------------------*/

    ord       = 4;
    stat_type = 'SD';
    trt_value = put(sd_age, 5.1);

    output;


    /*---------------------------------------------------------------
      Reporting Row 4: Median
    ----------------------------------------------------------------*/

    ord       = 5;
    stat_type = 'Median';
    trt_value = put(median_age, 5.1);

    output;


    /*---------------------------------------------------------------
      Reporting Row 5: Minimum
    ----------------------------------------------------------------*/

    ord       = 6;
    stat_type = 'Min';
    trt_value = put(min_age, 5.1);

    output;


    /*---------------------------------------------------------------
      Reporting Row 6: Maximum
    ----------------------------------------------------------------*/

    ord       = 7;
    stat_type = 'Max';
    trt_value = put(max_age, 5.1);

    output;


    /* Variables no longer required for reporting */

    drop
        n
        mean_age
        sd_age
        median_age
        min_age
        max_age;

run;


/******************************************************************************
* STEP 8: CREATE AGE GROUP TOTAL ROWS
*
* This section creates a total row showing the number and percentage of
* subjects represented within the treatment denominator.
******************************************************************************/

data agegrp_total;

    set trt_denom;

    length
        param     $40
        stat_type $25
        trt_value $20;

    param     = 'Age group (years)';
    stat_type = 'Total';

    ord = 14;

    trt_value =
        strip(put(denom, 3.0))
        || ' (100.0)';

    keep
        trt01a
        ord
        param
        stat_type
        trt_value;

run;


/******************************************************************************
* STEP 9: CALCULATE OVERALL AGE STATISTICS
*
* The previous PROC MEANS summarized age by treatment.
*
* A second PROC MEANS without a CLASS statement is used to obtain
* statistics across the complete Full Analysis Set.
******************************************************************************/

proc means data=adsl_fas
           noprint
           n
           mean
           std
           median
           min
           max;

    var age;

    output out=age_overall(drop=_freq_ _type_)
        n(age)      = n
        mean(age)   = mean_age
        std(age)    = sd_age
        median(age) = median_age
        min(age)    = min_age
        max(age)    = max_age;

run;


/******************************************************************************
* STEP 10: FORMAT OVERALL AGE STATISTICS
******************************************************************************/

data age_overall_reporting;

    set age_overall;

    length
        param     $40
        stat_type $15
        trt_value $20;

    /* Round statistics */

    mean_age   = round(mean_age,   0.1);
    sd_age     = round(sd_age,     0.1);
    median_age = round(median_age, 0.1);

    /* Use a generic label for the combined population */

    trt01a = 'OVERALL';


    /* N */

    ord       = 2;
    param     = 'Age (years)';
    stat_type = 'N';
    trt_value = put(n, 3.0);

    output;


    /* Mean */

    ord       = 3;
    stat_type = 'Mean';
    trt_value = put(mean_age, 5.1);

    output;


    /* Standard deviation */

    ord       = 4;
    stat_type = 'SD';
    trt_value = put(sd_age, 5.1);

    output;


    /* Median */

    ord       = 5;
    stat_type = 'Median';
    trt_value = put(median_age, 5.1);

    output;


    /* Minimum */

    ord       = 6;
    stat_type = 'Min';
    trt_value = put(min_age, 5.1);

    output;


    /* Maximum */

    ord       = 7;
    stat_type = 'Max';
    trt_value = put(max_age, 5.1);

    output;


    drop
        n
        mean_age
        sd_age
        median_age
        min_age
        max_age;

run;


/******************************************************************************
* STEP 11: DERIVE AGE GROUP ORDER
*
* Age groups are assigned an ordinal variable.
*
* The ordinal variable controls the order in which categories appear
* in the final report.
******************************************************************************/

data age_grp_1;

    set adsl_fas;

    length agegr1 $20;

    if not missing(age) then do;

        if age < 50 then
            ord = 8;

        else if 50 <= age < 65 then
            ord = 9;

        else if 65 <= age < 75 then
            ord = 10;

        else if 75 <= age < 80 then
            ord = 11;

        else if 80 <= age < 85 then
            ord = 12;

        else if age >= 85 then
            ord = 13;

    end;

run;


/******************************************************************************
* STEP 12: FREQUENCY COUNTS FOR CATEGORICAL VARIABLES
*
* PROC FREQ is used to count subjects within each category.
*
* Variables summarized:
*
* - Sex
* - Age group
* - Race
* - Ethnicity
*
* Percentages are calculated later using the treatment denominators.
******************************************************************************/

proc freq data=adsl_fas noprint;

    tables trt01a * sex /
        out=sex_freq(drop=percent);

run;


proc freq data=adsl_fas noprint;

    tables trt01a * agegr1 /
        out=agegrp_freq(drop=percent);

run;


proc freq data=adsl_fas noprint;

    tables trt01a * race /
        out=race_freq(drop=percent);

run;


proc freq data=adsl_fas noprint;

    tables trt01a * ethnic /
        out=ethnic_freq(drop=percent);

run;


/******************************************************************************
* STEP 13: AGE GROUP REPORTING DATASET
*
* Merge the frequency counts with the treatment denominator.
*
* Percentage calculation:
*
*       Percentage = Count / Denominator × 100
*
* The final display is constructed as:
*
*       n (%)
******************************************************************************/

proc sort data=agegrp_freq;
    by trt01a;
run;

proc sort data=trt_denom;
    by trt01a;
run;


data agegrp_reporting;

    merge
        agegrp_freq
        trt_denom;

    by trt01a;

    length
        param     $40
        stat_type $25
        trt_value $20;


    /* Calculate percentage */

    if count > 0 then
        pct = (count / denom) * 100;

    else
        pct = 0;


    /* Round percentage to one decimal place */

    pct   = round(pct, 0.1);
    count = round(count, 1.0);


    /* Assign reporting order */

    ord       = 14;
    param     = 'Age group (years)';
    stat_type = agegr1;


    /* Construct n (%) display */

    trt_value =
        trim(left(put(count, 3.0)))
        || ' ('
        || trim(left(put(pct, 5.1)))
        || ')';


    keep
        trt01a
        ord
        param
        stat_type
        trt_value;

run;


/******************************************************************************
* STEP 14: SEX REPORTING DATASET
******************************************************************************/

proc sort data=sex_freq;
    by trt01a;
run;

proc sort data=trt_denom;
    by trt01a;
run;


data sex_reporting;

    merge
        sex_freq
        trt_denom;

    by trt01a;

    length
        param     $40
        stat_type $25
        trt_value $20;


    /* Calculate percentage */

    if count > 0 then
        pct = (count / denom) * 100;

    else
        pct = 0;


    pct   = round(pct, 0.1);
    count = round(count, 1.0);


    ord       = 15;
    param     = 'Sex';
    stat_type = sex;


    /* Create n (%) display */

    trt_value =
        trim(left(put(count, 3.0)))
        || ' ('
        || trim(left(put(pct, 5.1)))
        || ')';


    keep
        trt01a
        ord
        param
        stat_type
        trt_value;

run;


/******************************************************************************
* STEP 15: RACE REPORTING DATASET
******************************************************************************/

proc sort data=race_freq;
    by trt01a;
run;

proc sort data=trt_denom;
    by trt01a;
run;


data race_reporting;

    merge
        race_freq
        trt_denom;

    by trt01a;

    length
        param     $40
        stat_type $50
        trt_value $30;


    /* Calculate percentage */

    if count > 0 then
        pct = (count / denom) * 100;

    else
        pct = 0;


    pct   = round(pct, 0.1);
    count = round(count, 1.0);


    ord       = 19;
    param     = 'Race';
    stat_type = race;


    /* Create n (%) display */

    trt_value =
        trim(left(put(count, 3.0)))
        || ' ('
        || trim(left(put(pct, 5.1)))
        || ')';


    keep
        trt01a
        ord
        param
        stat_type
        trt_value;

run;


/******************************************************************************
* STEP 16: ETHNICITY REPORTING DATASET
******************************************************************************/

proc sort data=ethnic_freq;
    by trt01a;
run;

proc sort data=trt_denom;
    by trt01a;
run;


data ethnic_reporting;

    merge
        ethnic_freq
        trt_denom;

    by trt01a;

    length
        param     $40
        stat_type $50
        trt_value $20;


    /* Calculate percentage */

    if count > 0 then
        pct = (count / denom) * 100;

    else
        pct = 0;


    pct   = round(pct, 0.1);
    count = round(count, 1.0);


    ord       = 26;
    param     = 'Ethnicity';
    stat_type = ethnic;


    /* Create n (%) display */

    trt_value =
        trim(left(put(count, 3.0)))
        || ' ('
        || trim(left(put(pct, 5.1)))
        || ')';


    keep
        trt01a
        ord
        param
        stat_type
        trt_value;

run;


/******************************************************************************
* STEP 17: OVERALL AGE GROUP COUNTS
*
* PROC FREQ without a treatment variable provides the category counts
* across the entire Full Analysis Set.
******************************************************************************/

proc freq data=adsl_fas noprint;

    tables agegr1 /
        out=agegrp_overall(drop=percent);

run;


data agegrp_overall_reporting;

    set agegrp_overall;

    length
        param     $40
        stat_type $25
        trt_value $20;

    trt01a = 'OVERALL';


    /* Calculate percentage using the overall denominator */

    pct = (count / &overall_n) * 100;

    pct   = round(pct, 0.1);
    count = round(count, 1.0);


    ord       = 14;
    param     = 'Age group (years)';
    stat_type = agegr1;


    trt_value =
        trim(left(put(count, 3.0)))
        || ' ('
        || trim(left(put(pct, 5.1)))
        || ')';


    keep
        trt01a
        ord
        param
        stat_type
        trt_value;

run;


/******************************************************************************
* STEP 18: OVERALL SEX COUNTS
******************************************************************************/

proc freq data=adsl_fas noprint;

    tables sex /
        out=sex_overall(drop=percent);

run;


data sex_overall_reporting;

    set sex_overall;

    length
        param     $40
        stat_type $25
        trt_value $20;

    trt01a = 'OVERALL';


    /* Calculate overall percentage */

    pct = (count / &overall_n) * 100;

    pct   = round(pct, 0.1);
    count = round(count, 1.0);


    ord       = 15;
    param     = 'Sex';
    stat_type = sex;


    trt_value =
        trim(left(put(count, 3.0)))
        || ' ('
        || trim(left(put(pct, 5.1)))
        || ')';


    keep
        trt01a
        ord
        param
        stat_type
        trt_value;

run;


/******************************************************************************
* STEP 19: OVERALL RACE COUNTS
******************************************************************************/

proc freq data=adsl_fas noprint;

    tables race /
        out=race_overall(drop=percent);

run;


data race_overall_reporting;

    set race_overall;

    length
        param     $40
        stat_type $50
        trt_value $30;

    trt01a = 'OVERALL';


    /* Calculate overall percentage */

    pct = (count / &overall_n) * 100;

    pct   = round(pct, 0.1);
    count = round(count, 1.0);


    ord       = 19;
    param     = 'Race';
    stat_type = race;


    trt_value =
        trim(left(put(count, 3.0)))
        || ' ('
        || trim(left(put(pct, 5.1)))
        || ')';


    keep
        trt01a
        ord
        param
        stat_type
        trt_value;

run;


/******************************************************************************
* STEP 20: OVERALL ETHNICITY COUNTS
******************************************************************************/

proc freq data=adsl_fas noprint;

    tables ethnic /
        out=ethnic_overall(drop=percent);

run;


data ethnic_overall_reporting;

    set ethnic_overall;

    length
        param     $40
        stat_type $50
        trt_value $30;

    trt01a = 'OVERALL';


    /* Calculate overall percentage */

    pct = (count / &overall_n) * 100;

    pct   = round(pct, 0.1);
    count = round(count, 1.0);


    ord       = 26;
    param     = 'Ethnicity';
    stat_type = ethnic;


    trt_value =
        trim(left(put(count, 3.0)))
        || ' ('
        || trim(left(put(pct, 5.1)))
        || ')';


    keep
        trt01a
        ord
        param
        stat_type
        trt_value;

run;


/******************************************************************************
* STEP 21: COMBINE ALL REPORTING DATASETS
*
* Each demographic component was created separately.
*
* The SET statement stacks these datasets vertically into one long-format
* reporting dataset.
******************************************************************************/

data all_reporting;

    set
        age_reporting
        age_overall_reporting
        agegrp_reporting
        agegrp_overall_reporting
        sex_reporting
        sex_overall_reporting
        race_reporting
        race_overall_reporting
        ethnic_reporting
        ethnic_overall_reporting;

run;


/******************************************************************************
* STEP 22: RESHAPE THE REPORTING DATA
*
* The reporting data are currently in LONG format.
*
* Example:
*
* Treatment A | Sex | Female | 20 (50.0)
* Treatment B | Sex | Female | 18 (45.0)
*
* PROC SQL is used to reshape the data into a WIDE reporting structure:
*
* Parameter | Category | Treatment A | Treatment B | Overall
*
* This structure is convenient for PROC REPORT.
******************************************************************************/

proc sql;

    create table reporting_wide as

    select
        a.ord,
        a.param,
        a.stat_type,

        b.trt_value as trt1,

        c.trt_value as trt2,

        d.trt_value as trt_overall


    from

        (
            select distinct
                ord,
                param,
                stat_type

            from all_reporting
        ) as a


    /*---------------------------------------------------------------
      Treatment A
    ----------------------------------------------------------------*/

    left join

        (
            select
                ord,
                param,
                stat_type,
                trt_value

            from all_reporting

            where trt01a = 'DRUG A'

        ) as b

        on  a.ord       = b.ord
        and a.param     = b.param
        and a.stat_type = b.stat_type


    /*---------------------------------------------------------------
      Treatment B
    ----------------------------------------------------------------*/

    left join

        (
            select
                ord,
                param,
                stat_type,
                trt_value

            from all_reporting

            where trt01a = 'DRUG B'

        ) as c

        on  a.ord       = c.ord
        and a.param     = c.param
        and a.stat_type = c.stat_type


    /*---------------------------------------------------------------
      Overall population
    ----------------------------------------------------------------*/

    left join

        (
            select
                ord,
                param,
                stat_type,
                trt_value

            from all_reporting

            where trt01a = 'OVERALL'

        ) as d

        on  a.ord       = d.ord
        and a.param     = d.param
        and a.stat_type = d.stat_type


    order by
        ord,
        param;

quit;


/******************************************************************************
* STEP 23: PREPARE FINAL DISPLAY DATASET
*
* This step prepares the dataset specifically for PROC REPORT.
*
* The main tasks are:
*
* - Create indentation for category rows
* - Convert coded values into readable labels
* - Create display parameters
* - Define reporting order
******************************************************************************/

data reporting_final;

    set reporting_wide;

    length
        indent       $2
        display_param $80;


    /*---------------------------------------------------------------
      Add indentation to category-level rows
    ----------------------------------------------------------------*/

    if param ne lag(param) or ord <= 13 then
        indent = '';

    else
        indent = '  ';


    /*---------------------------------------------------------------
      Convert coded sex values into display labels
    ----------------------------------------------------------------*/

    if stat_type = "F" then
        stat_type = "Female";

    if stat_type = "M" then
        stat_type = "Male";


    /*---------------------------------------------------------------
      Convert race categories into display labels
    ----------------------------------------------------------------*/

    if stat_type = 'ASIAN' then
        stat_type = 'Asian';

    if stat_type = 'WHITE' then
        stat_type = 'White';

    if stat_type = 'BLACK OR AFRICA' then
        stat_type = 'Black or African American';


    /*---------------------------------------------------------------
      Convert ethnicity categories into display labels
    ----------------------------------------------------------------*/

    if stat_type = 'HISPANIC OR LAT' then
        stat_type = 'Hispanic or Latino';

    if stat_type = 'NOT HISPANIC OR' then
        stat_type = 'Not Hispanic or Latino';


    /*---------------------------------------------------------------
      Construct the final display parameter
    ----------------------------------------------------------------*/

    display_param =
        trim(indent)
        || trim(stat_type);


    /*---------------------------------------------------------------
      Suppress repeated category headers
    ----------------------------------------------------------------*/

    if stat_type = param then
        display_param = '';


    /*---------------------------------------------------------------
      Define display ordering
    ----------------------------------------------------------------*/

    if param = "Age (years)" then
        ord1 = 1;

    else
        ord1 = ord;


    /*---------------------------------------------------------------
      Create final parameter labels
    ----------------------------------------------------------------*/

    if param = 'Age group (years)' then
        param = 'Age group (years) n (%)';

    if param = 'Sex' then
        param = 'Sex n (%)';

    if param = 'Race' then
        param = 'Race n (%)';

    if param = 'Ethnicity' then
        param = 'Ethnic group n (%)';

run;


/******************************************************************************
* STEP 24: GENERATE FINAL TABLE
*
* PROC REPORT is used to create the final presentation-ready table.
*
* The table contains:
*
* - Demographic characteristic
* - Category/statistic
* - Treatment A
* - Treatment B
* - Overall
*
* ODS PDF writes the resulting report to a PDF file.
******************************************************************************/

ods pdf file="&outpath/demographics_table.pdf";


/* Table title */

title1 "Demographic and Baseline Characteristics";

title2 "Full Analysis Set (FAS)";


/* Table footnote */

footnote1 "FAS = Full Analysis Set";


/******************************************************************************
* PROC REPORT
*
* NOWINDOWS:
*   Produces the report without interactive window behavior.
*
* SPLIT:
*   Allows headers to wrap across multiple lines.
*
* COLUMN:
*   Defines the variables and their order in the final table.
******************************************************************************/

proc report data=reporting_final
            nowindows
            split='/'

            style(report) =
                [rules=group frame=box]

            style(header) =
                [just=center]

            split="~";


    /*---------------------------------------------------------------
      Define columns
    ----------------------------------------------------------------*/

    column
        ord1
        param
        display_param
        trt1
        trt2
        trt_overall;


    /*---------------------------------------------------------------
      Ordering variable
    ----------------------------------------------------------------*/

    define ord1
        / order
          noprint;


    /*---------------------------------------------------------------
      Main demographic parameter
    ----------------------------------------------------------------*/

    define param
        / display
          "Demographic / characteristics";


    define param
        / order;


    /*---------------------------------------------------------------
      Category/statistic display column
    ----------------------------------------------------------------*/

    define display_param
        / display
          width=100
          left
          " ";


    /*---------------------------------------------------------------
      Treatment A column
    ----------------------------------------------------------------*/

    define trt1
        / display
          width=15
          center
          "DRUG A";


    /*---------------------------------------------------------------
      Treatment B column
    ----------------------------------------------------------------*/

    define trt2
        / display
          width=15
          center
          "DRUG B";


    /*---------------------------------------------------------------
      Overall column
    ----------------------------------------------------------------*/

    define trt_overall
        / display
          width=15
          center
          "Overall";


    /*---------------------------------------------------------------
      Add spacing between major demographic sections
    ----------------------------------------------------------------*/

    compute after param;

        line " ";

    endcomp;

run;


/******************************************************************************
* STEP 25: CLOSE OUTPUT DESTINATION
******************************************************************************/

ods pdf close;


/******************************************************************************
* PROGRAM COMPLETION MESSAGE
******************************************************************************/

%put NOTE: Demographic table generation complete.;

%put NOTE: Output file = &outpath/demographics_table.pdf;
