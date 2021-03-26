### Getting ACS income data for rank-order segregation calculation
### Tract-level income data for each MSA and calculate H Indices
### Nathan Quan - Feb. 24, 2019
### Obtaining ACS data adapted from Dr. Kramer


library(tidycensus)
library(tidyverse)
library(OasisR)

vv <- load_variables(2017, 'acs5', cache = T)

load("FinalMSA.RData") # msa_all_api_gt500: county-level observations
                       # Column names: CBSA.Code, birth_count, CBSA.Title, Metropolitan.Micropolitan.Statistical.Area, 
                       # County.County.Equivalent, FIPS64, StateFIPS, CountyFIPS




###Define 5-year ACS variables of interest




## INCOME VARIABLE----

# Define income vars----
vars_inc <- c(

  ## TABLE B19001: Household Income categories - TOTAL POP
  "B19001_002", # Income <10k
  "B19001_003", # Income 10-15k
  "B19001_004", # Income 15-20k
  "B19001_005", # Income 20-25k
  "B19001_006", # Income 25-30k
  "B19001_007", # Income 30-35k
  "B19001_008", # Income 35-40k
  "B19001_009", # Income 40-45k
  "B19001_010", # Income 45-50k
  "B19001_011", # Income 50-60k
  "B19001_012", # Income 60-75k
  "B19001_013", # Income 75-100k
  "B19001_014", # Income 100-125k
  "B19001_015", # Income 125-150k
  "B19001_016", # Income 150-200k
  "B19001_017" # Income 200+

)




# load("acs_inc.RData") 









## Create 134 MSA objects in a list----
## Each MSA object contains only the counties that are within the MSA
## Couldn't figure out a way to automate this

MSA_inc <- lst() #create empty list


MSA_inc$cbsa10740 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("35001", "35043", "35057", "35061"))


MSA_inc$cbsa11100 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("48011", "48065", "48359", "48375", "48381"))


MSA_inc$cbsa12100 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("34001"))

MSA_inc$cbsa12260 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("13033", "13073", "13181", "13189", "13245", "45003", "45037"))


MSA_inc$cbsa12940 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("22005", "22033", "22037", "22047", "22063", "22077", "22091", "22121", "22125"))


MSA_inc$cbsa13140 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("48199", "48245", "48351", "48361"))


MSA_inc$cbsa13820 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("1007", "01009", "01021", "01073", "01115", "01117", "01127"))


MSA_inc$cbsa14010 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("17039", "17113"))


MSA_inc$cbsa14260 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("16001", "16015", "16027", "16045", "16073"))


MSA_inc$cbsa14500 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("08013"))


MSA_inc$cbsa14740 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("53035"))


MSA_inc$cbsa16580 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("17019", "17053", "17147"))


MSA_inc$cbsa16700 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("45015", "45019", "45035"))


MSA_inc$cbsa17020 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06007"))


MSA_inc$cbsa17780 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("48041", "48051", "48395"))


MSA_inc$cbsa17900 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("45017", "45039", "45055", "45063", "45079", "45081"))


MSA_inc$cbsa19340 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("17073", "17131", "17161", "19163"))


MSA_inc$cbsa19380 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("39057", "39109", "39113"))


MSA_inc$cbsa21340 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("48141", "48229"))


MSA_inc$cbsa22180 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("37051", "37093"))


MSA_inc$cbsa23060 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("18003", "18179", "18183"))


MSA_inc$cbsa23540 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("12001", "12041"))


MSA_inc$cbsa24580 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("55009", "55061", "55083"))


MSA_inc$cbsa24860 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("45007", "45045", "45059", "45077"))


MSA_inc$cbsa25860 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("37003", "37023", "37027", "37035"))


MSA_inc$cbsa28420 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("53005", "53021"))


MSA_inc$cbsa28940 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("47001", "47009", "47013", "47057", "47093", "47105", "47129", "47145", "47173"))


MSA_inc$cbsa29200 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("18007", "18015", "18157"))


MSA_inc$cbsa29540 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("42071"))


MSA_inc$cbsa29620 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("26037", "26045", "26065"))


MSA_inc$cbsa30460 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("21017", "21049", "21067", "21113", "21209", "21239"))


MSA_inc$cbsa30700 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("31109", "31159"))


MSA_inc$cbsa30780 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("05045", "05053", "05085", "05105", "05119", "05125"))


MSA_inc$cbsa31700 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("33011"))


MSA_inc$cbsa32900 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06047"))


MSA_inc$cbsa36260 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("49003", "49011", "49029", "49057"))


MSA_inc$cbsa36500 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("53067"))


MSA_inc$cbsa37860 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("12033", "12113"))


MSA_inc$cbsa39340 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("49023", "49049"))


MSA_inc$cbsa40340 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("27039", "27045", "27109", "27157"))


MSA_inc$cbsa41420 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("41047", "41053"))


MSA_inc$cbsa41500 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06053"))


MSA_inc$cbsa42200 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06083"))


MSA_inc$cbsa42220 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06097"))


MSA_inc$cbsa42540 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("42069", "42079", "42131"))


MSA_inc$cbsa44060 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("53051", "53063", "53065"))


MSA_inc$cbsa44140 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("25013", "25015"))


MSA_inc$cbsa46540 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("36043", "36065"))


MSA_inc$cbsa47300 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06107"))


MSA_inc$cbsa48140 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("55073"))


MSA_inc$cbsa48620 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("20015", "20079", "20095", "20173", "20191"))


MSA_inc$cbsa49180 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("37057", "37059", "37067", "37169", "37197"))


MSA_inc$cbsa49700 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06101", "06115"))


MSA_inc$cbsa10420 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("39133", "39153"))


MSA_inc$cbsa10580 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("36001", "36083", "36091", "36093", "36095"))


MSA_inc$cbsa10900 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("34041", "42025", "42077","42095"))


MSA_inc$cbsa11260 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("02020", "02170"))


MSA_inc$cbsa11460 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("26161"))


MSA_inc$cbsa12540 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06029"))


MSA_inc$cbsa14860 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("09001"))


MSA_inc$cbsa15380 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("36029", "36063"))


MSA_inc$cbsa17140 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("18029", "18115", "18161", "21015", "21023", "21037", "21077", "21081", "21117", "21191", "39015", "39017", "39025", "39061", "39165"))


MSA_inc$cbsa17460 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("39035", "39055", "39085", "39093", "39103"))


MSA_inc$cbsa17820 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("08041", "08119"))


MSA_inc$cbsa18140 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("39041", "39045", "39049", "39073", "39089", "39097", "39117", "39127", "39129", "39159"))


MSA_inc$cbsa19780 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("19049", "19077", "19121", "19153", "19181"))


MSA_inc$cbsa20500 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("37037", "37063", "37135", "37145"))


MSA_inc$cbsa22220 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("05007", "05087", "05143", "29119"))


MSA_inc$cbsa24340 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("26015", "26081", "26117", "26139"))


MSA_inc$cbsa24660 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("37081", "37151", "37157"))


MSA_inc$cbsa25420 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("42041", "42043", "42099"))


MSA_inc$cbsa25540 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("09003", "09007", "09013"))


MSA_inc$cbsa26900 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("18011", "18013", "18057", "18059", "18063", "18081", "18095", "18097", "18109", "18133", "18145"))


MSA_inc$cbsa27260 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("12003", "12019", "12031", "12089", "12109"))


MSA_inc$cbsa27980 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("15005", "15009"))


MSA_inc$cbsa28140 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("20091", "20103", "20107", "20121", "20209", "29013", "29025", "29037", "29047", "29049", "29095", "29107", "29165", "29177"))


MSA_inc$cbsa28660 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("48027", "48099", "48281"))


MSA_inc$cbsa31140 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("18019", "18043", "18061", "18143", "18175", "21029", "21103", "21111", "21185", "21211", "21215", "21223"))


MSA_inc$cbsa31540 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("55021", "55025", "55045", "55049"))


MSA_inc$cbsa32820 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("05035", "28009", "28033", "28093", "28137", "28143", "47047", "47157", "47167"))


MSA_inc$cbsa33340 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("55079", "55089", "55131", "55133"))


MSA_inc$cbsa33700 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06099"))


MSA_inc$cbsa34980 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("47015", "47021", "47037", "47043", "47081", "47111", "47119", "47147", "47149", "47159", "47165", "47169", "47187", "47189"))


MSA_inc$cbsa35300 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("09009"))


MSA_inc$cbsa35380 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("22051", "22071", "22075", "22087", "22089", "22093", "22095", "22103"))


MSA_inc$cbsa36420 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("40017", "40027", "40051", "40081", "40083", "40087", "40109"))


MSA_inc$cbsa36540 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("19085", "19129", "19155", "31025", "31055", "31153", "31155", "31177"))


MSA_inc$cbsa36740 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("12069", "12095", "12097", "12117"))


MSA_inc$cbsa37100 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06111"))


MSA_inc$cbsa38300 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("42003", "42005", "42007", "42019", "42051", "42125", "42129"))


MSA_inc$cbsa39300 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("25005", "44001", "44003", "44005", "44007", "44009"))


MSA_inc$cbsa39580 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("37069", "37101", "37183"))


MSA_inc$cbsa39900 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("32029", "32031"))


MSA_inc$cbsa40060 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("51007", "51033", "51036", "51041", "51053", "51075", "51085", "51087", "51101", "51127", "51145", "51149", "51183", "51570", "51670", "51730", "51760"))


MSA_inc$cbsa40380 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("36051", "36055", "36069", "36073", "36117", "36123"))


MSA_inc$cbsa41180 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("17005", "17013", "17027", "17083", "17117", "17119", "17133", "17163", "29071", "29099", "29113", "29183", "29189", "29219", "29510"))


MSA_inc$cbsa41620 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("49035", "49045"))


MSA_inc$cbsa41700 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("48013", "48019", "48029", "48091", "48187", "48259", "48325", "48493"))


MSA_inc$cbsa44700 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06077"))


MSA_inc$cbsa45060 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("36053", "36067", "36075"))


MSA_inc$cbsa45300 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("12053", "12057", "12101", "12103"))


MSA_inc$cbsa45940 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("34021"))


MSA_inc$cbsa46060 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("04019"))


MSA_inc$cbsa46140 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("40037", "40111", "40113", "40117", "40131", "40143", "40145"))


MSA_inc$cbsa46700 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06095"))


MSA_inc$cbsa47260 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("37053", "37073", "51073", "51093", "51095", "51115", "51199", "51550", "51650", "51700", "51710", "51735", "51740", "51800", "51810", "51830"))


MSA_inc$cbsa49340 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("09015", "25027"))


MSA_inc$cbsa12060 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("13013", "13015", "13035", "13045", "13057",  "13063", "13067", "13077", "13085", "13089", "13097", "13113", "13117", "13121", "13135", "13143", "13149", "13151", "13159", "13171", "13199", "13211", "13217", "13223", "13227", "13231", "13247", "13255", "13297"))


MSA_inc$cbsa12420 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("48021", "48055", "48209", "48453", "48491"))


MSA_inc$cbsa12580 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("24003", "24005", "24013", "24025", "24027", "24035", "24510"))


MSA_inc$cbsa14460 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("25021", "25023", "25025", "25009", "25017", "33015", "33017"))


MSA_inc$cbsa16740 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("37025", "37071", "37097", "37109", "37119", "37159", "37179", "45023", "45057", "45091"))


MSA_inc$cbsa16980 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("17031", "17043", "17063", "17093", "17111", "17197", "17037", "17089", "18073", "18089", "18111", "18127", "17097", "55059"))


MSA_inc$cbsa19100 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("48085", "48113", "48121", "48139", "48231", "48257", "48397", "48221", "48251", "48367", "48425", "48439", "48497"))


MSA_inc$cbsa19740 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("08001", "08005", "08014", "08019", "08031", "08035", "08039", "08047", "08059", "08093"))


MSA_inc$cbsa19820 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("26163", "26087", "26093", "26099", "26125", "26147"))


MSA_inc$cbsa23420 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06019"))


MSA_inc$cbsa26420 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("48015", "48039", "48071", "48157", "48167", "48201", "48291", "48339", "48473"))


MSA_inc$cbsa29820 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("32003"))


MSA_inc$cbsa31080 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06059", "06037"))


MSA_inc$cbsa33100 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("12011", "12086", "12099"))


MSA_inc$cbsa33460 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("27003", "27019", "27025", "27037", "27053", "27059", "27079", "27095", "27123", "27139", "27141", "27143", "27163", "27171",  "55093", "55109"))


MSA_inc$cbsa35620 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("36027", "36079", "36059", "36103", "34013", "34019", "34027", "34035", "34037", "34039", "42103", "34003", "34017", "34023", "34025", "34029", "34031", "36005", "36047", "36061", "36071", "36081", "36085", "36087", "36119"))


MSA_inc$cbsa37980 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("34005", "34007", "34015", "42017", "42029", "42091", "42045", "42101", "10003", "24015", "34033"))


MSA_inc$cbsa38060 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("04013", "04021"))


MSA_inc$cbsa38900 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("41005", "41009", "41051", "41067", "41071", "53011", "53059"))


MSA_inc$cbsa40140 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06065", "06071"))


MSA_inc$cbsa40900 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06017", "06061", "06067", "06113"))


MSA_inc$cbsa41740 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06073"))


MSA_inc$cbsa41860 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06001", "06013", "06075", "06081", "06041"))


MSA_inc$cbsa41940 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("06069", "06085"))


MSA_inc$cbsa42660 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("53033", "53061", "53053"))


MSA_inc$cbsa46520 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("15003"))


MSA_inc$cbsa47900 <- msa_all_api_gt500 %>%
  filter(FIPS64 %in% c("24021", "24031", "11001", "24009", "24017", "24033", "51013", "51043", "51047", "51059", "51061", "51107", "51153", "51157", "51177", "51179", "51187", "51510", "51600", "51610", "51630", "51683", "51685", "54037"))








## ACQUIRE Income data ----

cbsaNames <- names(MSA_inc) # create vector of CBSA names from MSA_inc list
save(cbsaNames, file = "cbsaNames.RData")

 # loop through cbsaNames to retain CBSA code names in new list of income data
acs_inc <- lst() # create empty list
for(name in cbsaNames){
 ## Obtain income data
 acs_inc[[name]] <- get_acs(geography = 'tract',
                           state = MSA_inc[[name]]$StateFIPS,
                           county = MSA_inc[[name]]$CountyFIPS,
                           variables = vars_inc,
                           year = 2017,
                           survey = 'acs5',
                           geometry = F,
                           key = 'e3167c8ca832ee29daccf3735875272e50606b85')

   Sys.sleep(5) # Trying to get API call not to fail by building in 5-second pause

}


save(acs_inc, file = "acs_inc.RData")







### PREP DATA FOR RANKORDERSEG CALCULATION----

## rankorderseg() from OasisR package requires an object of class matrix
## where each column represents the distribution fo a group within spatial units
## i.e. columns = income categories; rows = tracts

## within the acs_inc list, each cbsa object has columns, GEOID, name, variable (income category), estimate, moe
## want to have columns be the income categories and the rows be the tracts


# loop through cbsaNames to retain CBSA code names in new list of income data in wide format
## Pivot so that columns = income categories; rows = tracts
acs_inc_wide <- lst() # create empty list
for(name in cbsaNames){
  acs_inc_wide[[name]] <- acs_inc[[name]] %>%
    select(-moe, -NAME) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    select(-GEOID)

  ## Removing duplicate income values due to collapsing tracts from pivot_wider
  for (incomecat in vars_inc) {
    
    for (i in 1:nrow(acs_inc_wide[[name]])) {
      acs_inc_wide[[name]][[incomecat]][[i]] <- acs_inc_wide[[name]][[incomecat]][[i]][!duplicated(acs_inc_wide[[name]][[incomecat]][[i]])]
    }
    
  }


}


save(acs_inc_wide, file = "acs_inc_wide.RData")





## Calculate rankorder H indices----


# loop through cbsaNames to retain CBSA code names in new list of calculated rankorder segregation indices
msa_h <- lst() # create empty list
for(name in cbsaNames){
  msa_h[[name]] <- matrix(unlist(acs_inc_wide[[name]]),
                       ncol=length(acs_inc_wide[[name]]),
                       byrow=F) %>%
    rankorderseg(polorder = 4, pred = c(0.10, 0.90))
}


save(msa_h, file = "msa_h.RData")




## Extract H index values----

# Instantiate vectors for Index, H10, H90 to place extracted values in
Index <- vector()
H10 <- vector()
H90 <- vector()

# Overall H Index
for(name in cbsaNames){
  Index[[name]] <- unlist(msa_h[[name]]$Hr$Index)
  H10[[name]] <- unlist(msa_h[[name]]$Hr$Predict$fit[[1]])
  H90[[name]] <- unlist(msa_h[[name]]$Hr$Predict$fit[[2]])
}

msa_h_final <- bind_cols(list(cbsaNames, Index, H10, H90))

  
names(msa_h_final) <- c("CBSA.Code", "H.Index", "H10", "H90")

msa_h_final$CBSA.Code <- str_replace(msa_h_final$CBSA.Code,"cbsa", "")

save(msa_h_final, file = "msa_h_final.RData")




