technical_notes <- function() {
  tabPanel(
    "Technical notes",
    h2("Technical notes"),
    br("Use this dashboard to view not in education, employment or training (NEET) and participation in education and training figures alongside contextual information for local authorities in England."),
    br("The NEET and participation figures are for young people aged 16 and 17. Young people are measured according to their academic age; ie their age on 31 August."),
    br("Data is collected using the National Client Caseload Information System (NCCIS), which draws together local databases used to support young people to engage in education and training and plan services that meet young people’s needs."),
    br("The cohort does not include young adult offenders in custody."),
    br("Annual comparisons are not available for Northamptonshire  in 2022 due to boundary changes. These annual changes appear as a 'z' in the dashboard"),
    br("For further information on the data used in this dashboard please see the methodology document here **INSERT LINK."),
    h3("Quintiles"),
    br("Each area's performance is compared with that of other local authorities in England. The best performing with the lowest NEET/highest participation fall into the green
      quintile, whereas local authorities with the highest NEET/lowest participation fall into the red quintile.  Comparisons between local authorities are made using data that has
      not been rounded."),
    br("The average England percentage may not necessarily fall in the middle of quintile 3 because the average is calculated by averaging
      all the values; for example if some of the values are particularly large compared with the other values, the average will be larger than
      the middle ranked LA and may fall outside the middle quintile."),
    br("Local authorities with no value or a suppressed vale are excluded from the quintile calculation."),
    br(),
    h3("Local authority direction"),
    br("Each area's performance is compared with the same period of the previous year.  A green arrow denotes those areas whose performance has improved (NEET gone down/participation gone up),
      a red arrow where performance is lower (NEET gone up/participation gone down) and a white double headed arrow where performance has stayed the same compared to the previous year.  Year-on-year comparisons,
      where available,  have been made using data that has not been rounded."),
    br()
  )
}
