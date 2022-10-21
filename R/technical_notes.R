technical_notes <- function() {
  tabPanel(
    "technical_notes",
    h2("Technical notes"),
    br("Use this dashboard to view NEET and not known scorecards and contextual information for local authorities in England."),
    br("There is no scorecard for North Northamptonshire and West Northamptonshire as they are new local authorities, following the split from one local authority into two in April 2021. As these two new local authorities are not directly comparable with the pre LGR 2021 Northamptonshire, we were unable to produce complete figures for the majority of individual indictors included in the school places scorecard,
                           however the relevant data for these pre and post LGR 2021 local authorities are included in the England data and can be found in the summary data on explore education statistics."),
    br("Young people are measured according to their academic age; ie their age on 31 August."),
    br("The cohort does not include young adult offenders in custody."),
    br("For further information on participation in education, training and NEET age 16 to 17 by local authority figures please see the methodology document here INSERT LINK."),
    h3("Scorecard content from 2017"),
    br("Changes by DfE from 1 September 2016 reduced the amount of information that local
                    authorities are mandated to collect and record in their Client Caseload Information
                    Systems and submit to the DfE in monthly extracts. Local authorities are only required
                    to submit extracts of information about young people of academic age 16 and 17, and were
                    no longer required to collect and record information about young people beyond the end of
                    the academic year in which they reach their 18th birthday. As such from 2017 the scorecard
                    reflects information about 16 and 17 year olds only."),
    br("The headline measure in the scorecard combines NEET and not known figures for 16 and 17 year olds. This gives
                       an accurate picture of local authority performance in terms of tracking and supporting
                       young people and means that low NEET figures cannot be masked by high levels of <U+2018>not knowns<U+2019>."),
    h3("Quintiles"),
    br("Each area's performance is compared with that of other local authorities in England.  The 1st quintile is always the indicator
      of best performance. Therefore, the 30 local authorities that have the lowest levels of 16-17 year olds NEET and not knowns will fall
      into the first quintile; the next 30 will fall into the second quintile etc.  The other indicators are reported in a similar way but
      those with the highest levels will fall into the first quintile.    Comparisons between local authorities are made using data that has
      not been rounded."),
    br("The average England percentage may not necessarily fall in the middle of Quintile 3 because the average is calculated by averaging
      all the values; for example if some of the values are particularly large compared with the other values, the average will be larger than
      the middle ranked LA and may fall outside the middle quintile."),
    br("LAs with no value are excluded from the quintile calculation.  Therefore if five LAs have missing data there will be 29 LAs in each
      quintile rather than 30."),
    br(),
    h3("LA direction"),
    br("Each area's performance is compared with the same period of the previous year.  A green arrow denotes those areas whose performance has improved,
      a red arrow where performance is lower and a black arrow where performance has stayed the same compared to the previous year.  Year-on-year comparisons,
      where available,  have been made using data that has not been rounded."),
    br(),
    h3("CCIS"),
    br("Local authorities track young people's participation in education or training using information provided by schools and colleges.  They record this on a local database which is known as the
         Client Caseload Information System (CCIS).  An extract from CCIS is sent to DfE and used to produce statistics about young people's participation which can be found on explore education statistics."),
    br()
  )
}
