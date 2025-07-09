technical_notes <- function() {
  tabPanel(
    "Technical notes",
    h2("Technical notes"),
    p("Use this dashboard to view not in education, employment or training (NEET) and participation in education and training figures alongside contextual information for local authorities in England."),
    p("The NEET and participation figures are for young people aged 16 and 17. Young people are measured according to their academic age; i.e. their age on 31 August."),
    p("Data is collected using the National Client Caseload Information System (NCCIS),which draws together local databases used to support young people to engage in education and training and plan services that meet young people's needs."),
    p("The cohort does not include young adult offenders in custody."),
    p(
      "For further information on the data used in this dashboard please see the methodology document on ",
      external_link(
        href = "https://explore-education-statistics.service.gov.uk/find-statistics/participation-in-education-training-and-neet-age-16-to-17-by-local-authority",
        "our Explore education statistics release page"
      ),
      "."
    ),
    h3("Quintiles"),
    p("When the values for an indicator are divided into five equal groups, each grouping is a known as a quintile. Each quintile represents 1/5 or 20% of the range of values for the indicator. The first quintile represents the lowest 1/5 of values from 0-20% of the range. Each quintile will contain approximately the same number of local authorities."),
    p("Each area's performance is compared with that of other local authorities in England. The best performing with the lowest NEET/highest participation fall into the green
      quintile, whereas local authorities with the highest NEET/lowest participation fall into the red quintile.  Comparisons between local authorities are made using data that has
      not been rounded."),
    p("The average England percentage may not necessarily fall in the middle of quintile 3 because the average is calculated by averaging
      all the values; for example if some of the values are particularly large compared with the other values, the average will be larger than
      the middle ranked LA and may fall outside the middle quintile."),
    p("The purpose of the quintiles is to compare the relative position of local authority NEET/Not known rates to other local authorities, rather than to make any judgement on national or regional rates."),
    p("Local authorities with no value or a suppressed value are excluded from the quintile calculation."),
    h3("Local authority direction"),
    p("Each area's performance is compared with the same period of the previous year. A green arrow denotes those areas whose performance has improved (NEET gone down/participation gone up),
      a red arrow where performance is lower (NEET gone up/participation gone down) and a white horizontal line where performance has stayed the same compared to the previous year.  Year-on-year comparisons,
      where available, have been made using data that has not been rounded."),
    h3("Data suppression"),
    p("Local authority data will be missing if it has been suppressed (c) to avoid disclosure, or the LA data is not available."),
    p("Participation data for Luton has been suppressed due to known quality issues with the March 2025 return."),
    br()
  )
}
