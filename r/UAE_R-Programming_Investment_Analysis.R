# RESULTS EDITION | 11 November 2026
# Saved results beneath each analysis, calculated from the two original workbooks.
# Snapshots computed independently with Python/pandas using the R calculation logic.
# R is unavailable in the preparation environment; this script has NOT been run in R.
# Live R output may use different formatting and tie order. Run locally to validate.
# Comments are static: changing code or data does not refresh the saved snapshots.
# Ratios such as weighted_yield are fractions (0.04252 = 4.252%); _pct uses percent.
# Display precision: six decimals; no rounding before calculation. NA means missing.
# Full row counts accompany previews. Source forecasts remain hypothetical scenarios.
# Run with working directory set to the project root (parent of data/ and R/).
# R >= 4.1 is needed for the native pipe operator |>.
# Source: A-UAE_Investment_Opportunity_AED.xlsx
# SHA256: f35b39e4f3b1427c430f53659561e900fcf69aecaf6da367de2fb958046749ae
# Source: A-UAE_Institutional_Capital_Forecast_2026_2030.xlsx
# SHA256: c175bb4f53be0eb5b3b351d6a5e0ced52e84c9dd89e72a33645c6aba312aa86f

# UAE PROPERTY INVESTMENT ANALYSIS — R
# Read the two supplied workbooks, then run each numbered question.
# No copied dataset rows, invented forecasts or notebook-export dependency.
# Source financial values are hypothetical. Residential GROSS and institutional NET
# measures are deliberately kept separate.
# Install once if needed: install.packages(c("readxl", "dplyr", "tidyr", "ggplot2"))
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Set this folder to the location of your two Excel files.
data_dir <- "data/raw"
r <- read_excel(file.path(data_dir, "A-UAE_Investment_Opportunity_AED.xlsx"), sheet="property_purchase_rental")
i <- read_excel(file.path(data_dir, "A-UAE_Institutional_Capital_Forecast_2026_2030.xlsx"), sheet="Institutional_laws")
budget <- 250000000
show_result <- function(result, question) {
 cat("\n", question, "\n")
 print(as_tibble(result), n=20, width=Inf)
 invisible(result)
}
# 1. Grain and validation: repeated project IDs reflect bedroom variants.
stopifnot(!anyDuplicated(i$candidate_id),!anyDuplicated(i[c('concept_id','delivery_route')]),
          all(r$purchase_price_aed>0),all(i$total_committed_cost_aed>0))
coverage <- data.frame(
 dataset=c('Residential','Institutional'),
 rows=c(nrow(r),nrow(i)),
 missing_rent_or_exit=c(sum(is.na(r$annual_rent_aed)),sum(is.na(i$terminal_net_sale_aed)))
)
show_result(coverage, 'coverage')
# 
# RESULT SNAPSHOT: Coverage | 2 rows | showing 2
#       dataset rows missing_rent_or_exit
#   Residential 2183                   16
# Institutional  600                   33

net_cols <- paste0('net_rent_',2026:2030,'_aed')
stopifnot(all(complete.cases(i[net_cols])))
stopifnot(all(abs(rowSums(i[net_cols])-i$cumulative_net_rent_aed)<1))
stopifnot(all(abs(i$total_committed_cost_aed-i$capex_spent_by_2030_aed-i$remaining_capex_aed)<1))
known <- !is.na(i$total_profit_aed)&!is.na(i$terminal_net_sale_aed)
stopifnot(all(abs(i$total_profit_aed[known]-(i$cumulative_net_rent_aed[known]+i$terminal_net_sale_aed[known]-i$total_committed_cost_aed[known]))<1))
# Validation result: all source integrity and institutional arithmetic checks passed
# in the independent calculation used to prepare these snapshots.
# 2. Residential gross yield; exclude unknown rents instead of replacing with zero.
re <- r |> filter(!is.na(annual_rent_aed),annual_rent_aed>=0) |>
  mutate(gross_yield=annual_rent_aed/purchase_price_aed)
stopifnot(all(abs(re$gross_yield-re$gross_rental_yield)<1e-6))
locations <- re |> group_by(location) |> summarise(rows=n(),cost=sum(purchase_price_aed),
  annual_gross_rent=sum(annual_rent_aed),median_yield=median(gross_yield),
  q25_yield=quantile(gross_yield,.25),q75_yield=quantile(gross_yield,.75),.groups='drop') |>
  mutate(weighted_yield=annual_gross_rent/cost) |> arrange(desc(weighted_yield))
show_result(locations,'residential_locations')
# 
# RESULT SNAPSHOT: Residential locations | 150 rows | showing 20
#                 location rows     cost annual_gross_rent median_yield q25_yield q75_yield weighted_yield
#          Al Habtoor City    8  5848000    384,984.000000     0.065624  0.064516  0.068006       0.065832
#                    Majan    8  6047000    384,984.000000     0.065953  0.052978  0.081177       0.063665
#                  Bukadra   16 12126000    769,968.000000     0.060075  0.058788  0.066911       0.063497
#            DAMAC Hills 2   24 18835000  1,154,952.000000     0.059953  0.054943  0.067248       0.061319
#             Uptown Dubai   12  9682000    577,476.000000     0.061718  0.048418  0.075601       0.059644
#                  Al Wasl   12  9814000    577,476.000000     0.056942  0.054284  0.066646       0.058842
#    Dubai Production City   20 16423000    963,576.000000     0.073830  0.051133  0.076498       0.058672
#                    Mudon   12  9980000    577,476.000000     0.066544  0.045780  0.068821       0.057863
#      Dubai World Central    8  6774000    384,984.000000     0.056824  0.052534  0.062896       0.056833
#               Umm Ramool   12 10210000    577,476.000000     0.061718  0.044420  0.070842       0.056560
# Rashid Yachts and Marina    8  6808000    384,984.000000     0.056666  0.051599  0.063600       0.056549
#        Dubai Sports City    8  6840000    384,984.000000     0.057687  0.047852  0.069624       0.056284
# Mohammed Bin Rashid City   28 23992000  1,347,444.000000     0.053331  0.046167  0.077199       0.056162
#                 Al Badaa   12 10309000    577,476.000000     0.068131  0.040471  0.071791       0.056017
#              Emaar South   24 21248000  1,171,272.000000     0.063814  0.047852  0.068559       0.055124
#             Al Rashidiya   12 10509000    578,136.000000     0.063025  0.038921  0.078749       0.055013
#       Downtown Jebel Ali   16 14177000    771,084.000000     0.058600  0.050032  0.066084       0.054390
#               The Greens   16 14572000    769,968.000000     0.055242  0.041182  0.072682       0.052839
#     Jumeirah Lake Towers   28 25576000  1,349,820.000000     0.053039  0.049418  0.059787       0.052777
#              The Gardens   12 11004000    577,476.000000     0.054310  0.046500  0.058337       0.052479
# Preview only: 130 more rows; run the code to inspect the full object.

show_result(re |> group_by(bedroom_type) |> summarise(rows=n(),cost=sum(purchase_price_aed),
  rent=sum(annual_rent_aed),.groups='drop') |> mutate(weighted_yield=rent/cost),'residential_bedrooms')
# 
# RESULT SNAPSHOT: Residential bedrooms | 8 rows | showing 8
# bedroom_type rows       cost              rent weighted_yield
#    1-bedroom  539  640256000 25,272,072.000000       0.039472
#    2-bedroom  542  856887000 34,375,236.000000       0.040116
#    3-bedroom  538 1089037000 43,461,960.000000       0.039909
#    4-bedroom   12  131732000  2,393,808.000000       0.018172
#    5-bedroom    5  111347000  1,879,812.000000       0.016882
#    6-bedroom    2   54228000  1,012,884.000000       0.018678
#    7-bedroom    1   18567000    501,816.000000       0.027027
#       Studio  528  375921000 15,982,980.000000       0.042517

p <- ggplot(head(locations,10),aes(reorder(location,weighted_yield),weighted_yield*100))+
 geom_col(fill='#187b91')+coord_flip()+theme_minimal()+labs(x=NULL,y='Annual gross yield (%)',
 title='Top residential locations — supplied scenarios')
print(p)
# 
# RESULT SNAPSHOT: Chart values: top ten locations | 10 rows | showing 10
#              location gross_yield_pct
#       Al Habtoor City        6.583174
#                 Majan        6.366529
#               Bukadra        6.349728
#         DAMAC Hills 2        6.131946
#          Uptown Dubai        5.964429
#               Al Wasl        5.884206
# Dubai Production City        5.867235
#                 Mudon        5.786333
#   Dubai World Central        5.683260
#            Umm Ramool        5.655984
# Running print(p) draws the bar chart in the R graphics viewer.

# 3. Institution route/type summaries. These are candidate-universe summaries,
# not investable portfolios: summing both routes would double-count concepts.
inst <- i |> group_by(institution_type,delivery_route) |> summarise(candidates=n(),
 cost=sum(total_committed_cost_aed),net_rent=sum(cumulative_net_rent_aed),
 missing_returns=sum(is.na(total_return_pct)),median_known_return=if(all(is.na(total_return_pct))) NA_real_ else median(total_return_pct,na.rm=TRUE),.groups='drop') |>
 mutate(net_income_on_cost=net_rent/cost) |> arrange(desc(net_income_on_cost))
show_result(inst,'institution_type_route')
# 
# RESULT SNAPSHOT: Institution type and route | 24 rows | showing 20
#              institution_type         delivery_route candidates                 cost             net_rent missing_returns median_known_return net_income_on_cost
#              Nursery / creche       Develop facility         25   118,457,266.200000    22,968,604.740409               0            0.092086           0.193898
#              Nursery / creche Buy completed facility         25   123,225,484.200000    22,968,604.740409               0            0.049828           0.186395
#       Music / performing arts Buy completed facility         25   633,622,500.000000   109,783,189.245483               0           -0.012497           0.173263
#    Fashion / design institute Buy completed facility         25   851,182,384.500000   141,939,933.871265               0           -0.033706           0.166756
#            Vocational academy Buy completed facility         25   916,847,274.000000   150,630,133.904199               0           -0.047989           0.164291
# Scientific research institute Buy completed facility         25 5,970,525,000.000000   966,092,065.360254               0           -0.077770           0.161810
#   Clinical training institute Buy completed facility         25 2,770,122,936.000000   447,593,323.146426               0           -0.089943           0.161579
#              Secondary school Buy completed facility         25 3,062,323,152.000000   459,372,094.808174               0           -0.155115           0.150008
#                Primary school Buy completed facility         25 1,549,112,500.000000   228,349,033.630605               0           -0.159864           0.147406
#          University / college Buy completed facility         25 7,174,317,167.999999 1,042,824,003.952148               0           -0.157718           0.145355
#                 Middle school Buy completed facility         25 2,241,132,093.000000   323,275,441.225166               0           -0.164142           0.144246
#                   K–12 school Buy completed facility         25 5,440,050,000.000000   761,163,445.435351               0           -0.202541           0.139918
#                Sports academy Buy completed facility         25 2,186,158,248.750000   287,107,559.255109               0           -0.260314           0.131330
#       Music / performing arts       Develop facility         25   623,190,000.000000    70,531,663.939453               0           -0.103343           0.113178
#    Fashion / design institute       Develop facility         25   833,779,842.000000    90,413,248.239863               0           -0.119035           0.108438
#            Vocational academy       Develop facility         25   889,670,184.000000    95,948,753.234141               0           -0.123831           0.107848
#              Secondary school       Develop facility         25 2,939,727,168.000000   297,221,526.418096               0           -0.214005           0.101105
#                Primary school       Develop facility         25 1,478,412,000.000000   146,705,860.994062               0           -0.213833           0.099232
#                 Middle school       Develop facility         25 2,145,363,408.000000   205,920,785.787117               0           -0.220211           0.095984
#                Sports academy       Develop facility         25 2,076,478,425.000000   185,763,454.011310               0           -0.304529           0.089461
# Preview only: 4 more rows; run the code to inspect the full object.

# 4. Paired alternatives. Positive buy-minus-develop = buy is higher.
pairs <- i |> select(concept_id,delivery_route,total_committed_cost_aed,cumulative_net_rent_aed,total_profit_aed) |>
 pivot_wider(names_from=delivery_route,values_from=c(total_committed_cost_aed,cumulative_net_rent_aed,total_profit_aed))
pairs <- pairs |> mutate(cost_difference=`total_committed_cost_aed_Buy completed facility`-`total_committed_cost_aed_Develop facility`,
 income_difference=`cumulative_net_rent_aed_Buy completed facility`-`cumulative_net_rent_aed_Develop facility`,
 profit_difference=`total_profit_aed_Buy completed facility`-`total_profit_aed_Develop facility`)
show_result(pairs |> select(concept_id,cost_difference,income_difference,profit_difference),'paired_routes')
# 
# RESULT SNAPSHOT: Buy minus develop | 300 rows | showing 20
# concept_id   cost_difference income_difference profit_difference
#      IC001    163,098.000000          0.000000   -163,098.000000
#      IC002  2,828,020.000000  3,326,203.728809  2,739,406.814917
#      IC003  4,379,085.000000  5,301,146.077752  8,297,409.048807
#      IC004  4,193,424.000000  5,516,630.574609  1,323,206.574609
#      IC005  9,222,000.000000 21,826,772.131836 35,017,002.992920
#      IC006  9,469,008.000000 32,637,741.466992                NA
#      IC007    929,016.000000 10,884,736.479902 13,668,105.386636
#      IC008  1,545,000.000000 27,703,210.782715 54,604,503.798706
#      IC009  1,242,690.000000  2,470,068.064902  4,663,920.130269
#      IC010  3,751,623.750000  3,447,894.109131   -303,729.640869
#      IC011    417,300.000000  1,599,136.408081  2,259,347.507172
#      IC012    795,742.500000  2,327,564.138081  4,770,101.661215
#      IC013    191,880.000000          0.000000   -191,880.000000
#      IC014  3,252,223.000000  3,705,102.097354  5,607,692.195403
#      IC015  3,236,715.000000  3,946,512.641836    709,797.641836
#      IC016  4,933,440.000000  6,652,407.457617  6,201,413.629834
#      IC017 10,605,300.000000 23,571,702.170605                NA
#      IC018  6,998,832.000000 25,779,639.031348 27,573,297.599927
#      IC019  1,092,960.000000 12,760,266.784766 24,769,841.749707
#      IC020  1,776,750.000000 29,917,929.678076                NA
# Preview only: 280 more rows; run the code to inspect the full object.

show_result(pairs |> summarise(concepts=n(),buy_cheaper=sum(cost_difference<0,na.rm=TRUE),
 buy_higher_income=sum(income_difference>0,na.rm=TRUE),equal_income=sum(abs(income_difference)<1,na.rm=TRUE),
 missing_profit_comparison=sum(is.na(profit_difference))),'paired_findings')
# 
# RESULT SNAPSHOT: Paired findings | 1 rows | showing 1
# concepts buy_cheaper buy_higher_income equal_income missing_profit_comparison
#      300           0               275           25                        33

# 5. Delays and outstanding full capital commitments, not just spent capital.
show_result(i |> group_by(opening_year,delivery_route) |> summarise(candidates=n(),
 commitment=sum(total_committed_cost_aed),remaining_capex=sum(remaining_capex_aed),
 unknown_exit=sum(is.na(terminal_net_sale_aed)),.groups='drop'),'opening_timeline')
# 
# RESULT SNAPSHOT: Opening timeline | 8 rows | showing 8
# opening_year         delivery_route candidates            commitment remaining_capex unknown_exit
#         2027 Buy completed facility        100  9,227,582,560.549999               0            0
#         2027       Develop facility          9     36,466,723.800000               0            0
#         2028 Buy completed facility        100 11,072,072,856.000000               0            0
#         2028       Develop facility         66  3,200,036,145.000000               0            0
#         2029 Buy completed facility        100 12,618,963,323.899998               0            0
#         2029       Develop facility         99  9,404,602,526.400000               0            0
#         2030       Develop facility         93 11,332,799,778.000000               0            0
#         2031       Develop facility         33  7,982,770,079.999999               0           33

show_result(i |> filter(total_committed_cost_aed>budget) |>
 select(candidate_id,location,institution_type,total_committed_cost_aed),'over_budget_candidates')
# 
# RESULT SNAPSHOT: Candidates above budget | 58 rows | showing 20
# candidate_id        location              institution_type total_committed_cost_aed
#     INST0011   Academic City          University / college       318,581,280.000000
#     INST0012   Academic City          University / college       328,050,288.000000
#     INST0034 Al Barsha South                   K–12 school       250,242,300.000000
#     INST0039 Al Barsha South Scientific research institute       272,867,400.000000
#     INST0040 Al Barsha South Scientific research institute       274,644,150.000000
#     INST0059       Al Furjan          University / college       277,027,200.000000
#     INST0060       Al Furjan          University / college       285,261,120.000000
#     INST0083      Al Garhoud          University / college       318,581,280.000000
#     INST0084      Al Garhoud          University / college       328,050,288.000000
#     INST0106       Al Jaddaf                   K–12 school       250,242,300.000000
#     INST0111       Al Jaddaf Scientific research institute       272,867,400.000000
#     INST0112       Al Jaddaf Scientific research institute       274,644,150.000000
#     INST0131    Al Khawaneej          University / college       277,027,200.000000
#     INST0132    Al Khawaneej          University / college       285,261,120.000000
#     INST0155         Al Quoz          University / college       318,581,280.000000
#     INST0156         Al Quoz          University / college       328,050,288.000000
#     INST0178       Al Qusais                   K–12 school       250,242,300.000000
#     INST0183       Al Qusais Scientific research institute       272,867,400.000000
#     INST0184       Al Qusais Scientific research institute       274,644,150.000000
#     INST0203           Arjan          University / college       277,027,200.000000
# Preview only: 38 more rows; run the code to inspect the full object.


# 6. Which institutional locations generate the strongest income on cost?
i |> group_by(location, delivery_route) |>
 summarise(cost=sum(total_committed_cost_aed), income=sum(cumulative_net_rent_aed), .groups="drop") |>
 mutate(net_income_on_cost_pct=100*income/cost) |>
 arrange(desc(net_income_on_cost_pct)) |> print(n=20)


# 
# RESULT SNAPSHOT: Institutional locations | 50 rows | showing 20
#                location         delivery_route                 cost             income net_income_on_cost_pct
#               Al Furjan Buy completed facility 1,283,628,551.050000 204,825,064.121040              15.956724
#            Al Khawaneej Buy completed facility 1,283,628,551.050000 204,825,064.121040              15.956724
#                   Arjan Buy completed facility 1,283,628,551.050000 204,825,064.121040              15.956724
#      Dubai Hills Estate Buy completed facility 1,283,628,551.050000 204,825,064.121040              15.956724
#     Dubai Silicon Oasis Buy completed facility 1,283,628,551.050000 204,825,064.121040              15.956724
#       Dubai Studio City Buy completed facility 1,283,628,551.050000 204,825,064.121040              15.956724
#               Dubailand Buy completed facility 1,283,628,551.050000 204,825,064.121040              15.956724
#            Nad Al Sheba Buy completed facility 1,283,628,551.050000 204,825,064.121040              15.956724
#           Academic City Buy completed facility 1,333,597,372.450000 194,907,038.665164              14.615134
#              Al Garhoud Buy completed facility 1,333,597,372.450000 194,907,038.665164              14.615134
#                 Al Quoz Buy completed facility 1,333,597,372.450000 194,907,038.665164              14.615134
#            Business Bay Buy completed facility 1,333,597,372.450000 194,907,038.665164              14.615134
#   Dubai Investment Park Buy completed facility 1,333,597,372.450000 194,907,038.665164              14.615134
#             Dubai South Buy completed facility 1,333,597,372.450000 194,907,038.665164              14.615134
#    Jumeirah Lake Towers Buy completed facility 1,333,597,372.450000 194,907,038.665164              14.615134
# Jumeirah Village Circle Buy completed facility 1,333,597,372.450000 194,907,038.665164              14.615134
#             Town Square Buy completed facility 1,333,597,372.450000 194,907,038.665164              14.615134
#         Al Barsha South Buy completed facility 1,330,901,747.500000 193,541,870.952475              14.542161
#               Al Jaddaf Buy completed facility 1,330,901,747.500000 193,541,870.952475              14.542161
#               Al Qusais Buy completed facility 1,330,901,747.500000 193,541,870.952475              14.542161
# Preview only: 30 more rows; run the code to inspect the full object.
# 7. What are the top three supplied returns in each institution category?
i |> filter(!is.na(total_return_pct)) |> group_by(institution_type) |>
 slice_max(total_return_pct,n=3,with_ties=TRUE) |> ungroup() |>
 select(candidate_id,location,institution_type,delivery_route,total_return_pct) |> print(n=40)


# 
# RESULT SNAPSHOT: Top three returns per type (ties included) | 100 rows | showing 40
# candidate_id                location            institution_type         delivery_route total_return_pct
#     INST0014           Academic City Clinical training institute Buy completed facility        -0.033214
#     INST0086              Al Garhoud Clinical training institute Buy completed facility        -0.033214
#     INST0158                 Al Quoz Clinical training institute Buy completed facility        -0.033214
#     INST0230            Business Bay Clinical training institute Buy completed facility        -0.033214
#     INST0302   Dubai Investment Park Clinical training institute Buy completed facility        -0.033214
#     INST0374             Dubai South Clinical training institute Buy completed facility        -0.033214
#     INST0446 Jumeirah Village Circle Clinical training institute Buy completed facility        -0.033214
#     INST0518             Town Square Clinical training institute Buy completed facility        -0.033214
#     INST0590    Jumeirah Lake Towers Clinical training institute Buy completed facility        -0.033214
#     INST0048         Al Barsha South  Fashion / design institute Buy completed facility         0.026528
#     INST0120               Al Jaddaf  Fashion / design institute Buy completed facility         0.026528
#     INST0192               Al Qusais  Fashion / design institute Buy completed facility         0.026528
#     INST0264   Dubai Healthcare City  Fashion / design institute Buy completed facility         0.026528
#     INST0336      Dubai Science Park  Fashion / design institute Buy completed facility         0.026528
#     INST0408       Dubai Sports City  Fashion / design institute Buy completed facility         0.026528
#     INST0480                  Mirdif  Fashion / design institute Buy completed facility         0.026528
#     INST0552   Dubai Design District  Fashion / design institute Buy completed facility         0.026528
#     INST0058               Al Furjan                 K–12 school Buy completed facility        -0.152832
#     INST0130            Al Khawaneej                 K–12 school Buy completed facility        -0.152832
#     INST0202                   Arjan                 K–12 school Buy completed facility        -0.152832
#     INST0274      Dubai Hills Estate                 K–12 school Buy completed facility        -0.152832
#     INST0346     Dubai Silicon Oasis                 K–12 school Buy completed facility        -0.152832
#     INST0418               Dubailand                 K–12 school Buy completed facility        -0.152832
#     INST0490            Nad Al Sheba                 K–12 school Buy completed facility        -0.152832
#     INST0562       Dubai Studio City                 K–12 school Buy completed facility        -0.152832
#     INST0030         Al Barsha South               Middle school Buy completed facility        -0.112039
#     INST0102               Al Jaddaf               Middle school Buy completed facility        -0.112039
#     INST0174               Al Qusais               Middle school Buy completed facility        -0.112039
#     INST0246   Dubai Healthcare City               Middle school Buy completed facility        -0.112039
#     INST0318      Dubai Science Park               Middle school Buy completed facility        -0.112039
#     INST0390       Dubai Sports City               Middle school Buy completed facility        -0.112039
#     INST0462                  Mirdif               Middle school Buy completed facility        -0.112039
#     INST0534   Dubai Design District               Middle school Buy completed facility        -0.112039
#     INST0070               Al Furjan     Music / performing arts Buy completed facility         0.049059
#     INST0142            Al Khawaneej     Music / performing arts Buy completed facility         0.049059
#     INST0214                   Arjan     Music / performing arts Buy completed facility         0.049059
#     INST0286      Dubai Hills Estate     Music / performing arts Buy completed facility         0.049059
#     INST0358     Dubai Silicon Oasis     Music / performing arts Buy completed facility         0.049059
#     INST0430               Dubailand     Music / performing arts Buy completed facility         0.049059
#     INST0502            Nad Al Sheba     Music / performing arts Buy completed facility         0.049059
# Preview only: 60 more rows; run the code to inspect the full object.
# 8. Which individual candidates fit the budget, and what cash remains?
i |> filter(total_committed_cost_aed<=budget) |>
 mutate(unspent_aed=budget-total_committed_cost_aed) |>
 arrange(desc(cumulative_net_rent_aed)) |>
 select(candidate_id,location,institution_type,delivery_route,total_committed_cost_aed,
 cumulative_net_rent_aed,unspent_aed) |> print(n=20)

# 
# RESULT SNAPSHOT: Individual budget-fit alternatives | 542 rows | showing 20
# candidate_id              location              institution_type         delivery_route total_committed_cost_aed cumulative_net_rent_aed       unspent_aed
#     INST0036       Al Barsha South          University / college Buy completed facility       242,471,952.000000       49,903,187.072168  7,528,048.000000
#     INST0108             Al Jaddaf          University / college Buy completed facility       242,471,952.000000       49,903,187.072168  7,528,048.000000
#     INST0180             Al Qusais          University / college Buy completed facility       242,471,952.000000       49,903,187.072168  7,528,048.000000
#     INST0252 Dubai Healthcare City          University / college Buy completed facility       242,471,952.000000       49,903,187.072168  7,528,048.000000
#     INST0324    Dubai Science Park          University / college Buy completed facility       242,471,952.000000       49,903,187.072168  7,528,048.000000
#     INST0396     Dubai Sports City          University / college Buy completed facility       242,471,952.000000       49,903,187.072168  7,528,048.000000
#     INST0468                Mirdif          University / college Buy completed facility       242,471,952.000000       49,903,187.072168  7,528,048.000000
#     INST0540 Dubai Design District          University / college Buy completed facility       242,471,952.000000       49,903,187.072168  7,528,048.000000
#     INST0064             Al Furjan Scientific research institute Buy completed facility       202,997,850.000000       45,744,588.149487 47,002,150.000000
#     INST0136          Al Khawaneej Scientific research institute Buy completed facility       202,997,850.000000       45,744,588.149487 47,002,150.000000
#     INST0208                 Arjan Scientific research institute Buy completed facility       202,997,850.000000       45,744,588.149487 47,002,150.000000
#     INST0280    Dubai Hills Estate Scientific research institute Buy completed facility       202,997,850.000000       45,744,588.149487 47,002,150.000000
#     INST0352   Dubai Silicon Oasis Scientific research institute Buy completed facility       202,997,850.000000       45,744,588.149487 47,002,150.000000
#     INST0424             Dubailand Scientific research institute Buy completed facility       202,997,850.000000       45,744,588.149487 47,002,150.000000
#     INST0496          Nad Al Sheba Scientific research institute Buy completed facility       202,997,850.000000       45,744,588.149487 47,002,150.000000
#     INST0568     Dubai Studio City Scientific research institute Buy completed facility       202,997,850.000000       45,744,588.149487 47,002,150.000000
#     INST0016         Academic City Scientific research institute Buy completed facility       238,821,000.000000       40,087,991.415527 11,179,000.000000
#     INST0088            Al Garhoud Scientific research institute Buy completed facility       238,821,000.000000       40,087,991.415527 11,179,000.000000
#     INST0160               Al Quoz Scientific research institute Buy completed facility       238,821,000.000000       40,087,991.415527 11,179,000.000000
#     INST0232          Business Bay Scientific research institute Buy completed facility       238,821,000.000000       40,087,991.415527 11,179,000.000000
# Preview only: 522 more rows; run the code to inspect the full object.
# Above rows are individual alternatives; adding them together may exceed the budget.

# 9. Annual net income by route (candidate universe, not a selected portfolio).
i |> select(delivery_route,all_of(net_cols)) |>
 pivot_longer(all_of(net_cols),names_to="year",values_to="net_rent") |>
 group_by(delivery_route,year) |> summarise(net_rent_aed=sum(net_rent),.groups="drop") |> print()


# 
# RESULT SNAPSHOT: Annual income by route | 10 rows | showing 10
#         delivery_route              year         net_rent_aed
# Buy completed facility net_rent_2026_aed             0.000000
# Buy completed facility net_rent_2027_aed   413,070,951.250000
# Buy completed facility net_rent_2028_aed   984,875,160.535156
# Buy completed facility net_rent_2029_aed 1,695,639,848.420146
# Buy completed facility net_rent_2030_aed 1,847,512,868.369286
#       Develop facility net_rent_2026_aed             0.000000
#       Develop facility net_rent_2027_aed     2,079,499.500000
#       Develop facility net_rent_2028_aed   153,597,222.404687
#       Develop facility net_rent_2029_aed   633,264,772.823359
#       Develop facility net_rent_2030_aed 1,280,480,565.948943
# 10. Are the supplied institutional selection flags feasible?
s <- i |> filter(selected==1)
print(tibble(selected_rows=nrow(s), duplicate_concepts=anyDuplicated(s$concept_id)>0,
 committed_aed=sum(s$total_committed_cost_aed),unspent_aed=budget-sum(s$total_committed_cost_aed),
 net_income_aed=if(nrow(s)>0) sum(s$cumulative_net_rent_aed) else NA_real_))
if(nrow(s)==0) message("No institutional selections in workbook; no portfolio outcome is claimed.")
if(nrow(s)>0) {
 s |> group_by(location) |> summarise(cost=sum(total_committed_cost_aed),.groups="drop") |>
 mutate(capital_share_pct=100*cost/sum(cost)) |> arrange(desc(capital_share_pct)) |> print()
}

# 
# RESULT SNAPSHOT: Workbook selected portfolio | 1 rows | showing 1
# selected_rows duplicate_concepts committed_aed unspent_aed net_income_aed
#             0              FALSE             0   250000000             NA
# No institutional selections in workbook; no portfolio outcome is claimed.
# Portfolio optimization remains in the notebook, not replaced with yield sorting.
# Mixed net-return comparisons and alternative forecasts require missing financial inputs.
