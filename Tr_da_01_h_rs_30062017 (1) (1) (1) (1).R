Sys.setenv('http_proxy' = 'http://10.74.1.25:8080/')
Sys.setenv('https_proxy' = 'http://10.74.1.25:8080/') 

install.packages("stargazer")
library("stargazer")
install.packages("lubridate")
library("lubridate")

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}


load("~/Tr_da_01_h_rs_env_20170512.RData")

rm(m_ex_control, m_ex_controlFE, m_ex_prev_cur_2000data, m_ex_prev_cur_2000data2, m_ex_prev_cur_trend, m_ex_prev_cur_trend_age)
rm(data_btc2_all, data_sd_btc2_all, data_ssd_btc2_all, data_state_btc2_all)
gc()

#load the ssd index file...
ssd_index_201609 = read.csv("SSD Hedonic Index 201609.csv", header = TRUE, stringsAsFactors = FALSE)
 # head(ssd_index_201609)
 # str(ssd_index_201609)

ssd_index_201609$SSDcode = as.character(ssd_index_201609$SSDcode)
ssd_index_201609$Index_val = as.numeric(ssd_index_201609$Index_val)

cpi_all_australia <- read.csv("CPI_all_Australia.csv", header = TRUE, stringsAsFactors = FALSE)

install.packages("zoo")
library(zoo)
tr_da_01_h_rs$prevdateq <- as.yearqtr(tr_da_01_h_rs$prev_sale_date, format = "%Y-%m-%d")
format(tr_da_01_h_rs$prevdateq, format = "%Y-%q")
tr_da_01_h_rs = merge(tr_da_01_h_rs, cpi_all_australia, by.x = "prevdateq", by.y = "CPI_Quarter", all.x = TRUE)
tr_da_01_h_rs$Quarter2 = NULL
colnames(tr_da_01_h_rs)[colnames(tr_da_01_h_rs) == 'CPI_aus'] <- 'cpi_aus_prevdate'
tr_da_01_h_rs$const_dollar_prev_price_201609 = tr_da_01_h_rs$prev_sale_price * 109.4 / tr_da_01_h_rs$cpi_aus_prevdate
tr_da_01_h_rs$const_dollar_cur_price_201609  = tr_da_01_h_rs$contract_price  * 109.4 / tr_da_01_h_rs$cpi_aus_cdate



tr_da_01_h_rs$building_type_compact2 = as.character(tr_da_01_h_rs$building_type_compact2)
tr_da_01_h_rs$building_type_compact2 = ifelse(tr_da_01_h_rs$building_type_compact2 == "Multiple DA","Multiple_DAs",tr_da_01_h_rs$building_type_compact2)
tr_da_01_h_rs$building_type_compact2 = as.factor(tr_da_01_h_rs$building_type_compact2)
tr_da_01_h_rs$building_type_compact2 = relevel(tr_da_01_h_rs$building_type_compact2,"Non-DA")

tr_da_01_h_rs2 = merge(tr_da_01_h_rs,ssd_index_201609, by.x = c("SSDcode","pdate"), by.y = c("SSDcode", "i_date"), all.x = TRUE)

tr_da_01_h_rs2$Property_type = NULL
tr_da_01_h_rs2$Index_date = NULL


colnames(tr_da_01_h_rs2)[colnames(tr_da_01_h_rs2) == "Index_val"] = "ssd_index_pdate"


tr_da_01_h_rs2$prev_sale_indexed_pdate = (tr_da_01_h_rs2$ssd_index_pdate/tr_da_01_h_rs2$ssd_index_prev_sale2) * tr_da_01_h_rs2$prev_sale_price
tr_da_01_h_rs2$prev_sale_indexed_pdate_const_doll_2016 = (109.4/tr_da_01_h_rs2$cpi_aus_pdate) * tr_da_01_h_rs2$prev_sale_indexed_pdate

tr_da_01_h_rs2$prev_sale_indexed_pdate_plus_cost = tr_da_01_h_rs2$prev_sale_indexed_pdate + tr_da_01_h_rs2$value

#tr_da_01_h_rs2$prev_sale_indexed_pdate_plus_cost_indexed_cur_sale = da_rs_final_duplex_139k3$prev_sale_indexed_pdate_plus_cost * da_rs_final_duplex_139k3$ssd_index_cur_sale2/da_rs_final_duplex_139k3$index_val_pdate

tr_da_01_h_rs2$purchase_price_notional = ifelse(tr_da_01_h_rs2$DA == 1, tr_da_01_h_rs2$prev_sale_indexed_pdate_plus_cost, tr_da_01_h_rs2$prev_sale_price)
tr_da_01_h_rs2$purchase_price_notional_const_doll_2016 = tr_da_01_h_rs2$purchase_price_notional * 109.4/tr_da_01_h_rs2$cpi_aus_pdate
tr_da_01_h_rs2$ssd_index_purchase_notional = ifelse(tr_da_01_h_rs2$DA == 1, tr_da_01_h_rs2$ssd_index_pdate , tr_da_01_h_rs2$ssd_index_prev_sale2)
tr_da_01_h_rs2$purchase_date_notional = as.Date(ifelse(tr_da_01_h_rs2$DA == 1, as.character(tr_da_01_h_rs2$permit_date), as.character(tr_da_01_h_rs2$prev_sale_date2)))
tr_da_01_h_rs2$contract_price_duplex_double = ifelse(tr_da_01_h_rs2$DA==0, tr_da_01_h_rs2$contract_price,tr_da_01_h_rs2$contract_price_duplex_double)
tr_da_01_h_rs2$age_purchase_notional = as.integer(tr_da_01_h_rs2$contract_date2 - tr_da_01_h_rs2$purchase_date_notional)

 # m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ DA + log(ssd_index_cur_sale2 / ssd_index_purchase_notional) + I(contract_date2 - purchase_date_notional) +  I(beds_cur_sale-beds_prev_sale) + I(baths_curr_sale-baths_prev_sale) + I(cars_curr_sale-cars_prev_sale) + state, purchase_date_notional >= as.Date("2004-03-01") & vacantb==0 & vacant_s==0 & vacant_bs ==0, data = tr_da_01_h_rs2)

 # m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ building_type_compact2 + log(ssd_index_cur_sale2 / ssd_index_purchase_notional) + I(contract_date2 - purchase_date_notional) +  I(beds_cur_sale-beds_prev_sale) + I(baths_curr_sale-baths_prev_sale) + I(cars_curr_sale-cars_prev_sale) + state, purchase_date_notional >= as.Date("2004-03-01") & vacantb==0 & vacant_s==0 & vacant_bs ==0, data = tr_da_01_h_rs2)

 # m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ building_type_compact2 + log(ssd_index_cur_sale2 / ssd_index_purchase_notional) + age_purchase_notional + I(age_purchase_notional^2) + I(beds_cur_sale-beds_prev_sale) + I(baths_curr_sale-baths_prev_sale) + I(cars_curr_sale-cars_prev_sale) + state, purchase_date_notional >= as.Date("2004-03-01") & vacantb==0 & vacant_s==0 & vacant_bs ==0, data = tr_da_01_h_rs2)

gc()

tr_da_01_h_rs2$beds_cur_sale_du = ifelse(tr_da_01_h_rs2$building_type_compact2=="Duplex",2*tr_da_01_h_rs2$beds_cur_sale,tr_da_01_h_rs2$beds_cur_sale)
tr_da_01_h_rs2$baths_curr_sale_du = ifelse(tr_da_01_h_rs2$building_type_compact2=="Duplex",2*tr_da_01_h_rs2$baths_curr_sale,tr_da_01_h_rs2$baths_curr_sale)
tr_da_01_h_rs2$cars_curr_sale_du = ifelse(tr_da_01_h_rs2$building_type_compact2=="Duplex",2*tr_da_01_h_rs2$cars_curr_sale,tr_da_01_h_rs2$cars_curr_sale)


select_suburb = read.csv("self_select_suburbs.csv", header = TRUE, stringsAsFactors = FALSE)
tr_da_01_h_rs3 = merge(tr_da_01_h_rs2, select_suburb, by = c("state","suburb","postcode"), all.x = TRUE)
tr_da_01_h_rs3$selfselect = ifelse(is.na(tr_da_01_h_rs3$selfselect),0,1)
tr_da_01_h_rs3$selfselect2 = ifelse(tr_da_01_h_rs3$DA==1,1,tr_da_01_h_rs3$selfselect)
tr_da_01_h_rs3$age_notional_months = elapsed_months(tr_da_01_h_rs3$contract_date2,tr_da_01_h_rs3$purchase_date_notional)
tr_da_01_h_rs3$months_bet_sales = elapsed_months(tr_da_01_h_rs3$contract_date2,tr_da_01_h_rs3$prev_sale_date2)



building_type_compact2 = c("Carports/Garages/Sheds","Duplex","Extension/Alteration","House/Single Dwelling"
                           ,"Multiple_DAs","Swimming Pool","Verandahs/Pergolas")
cm_time = c(0,3,0,2,1,0,0)
ct_time = c(0,11,2,7,6,3,0)
cm_ct_times = data.frame(building_type_compact2,cm_time,ct_time,stringsAsFactors = FALSE)

tr_da_01_h_rs3_times = merge(tr_da_01_h_rs3,cm_ct_times, by = "building_type_compact2", all.x = TRUE)
tr_da_01_h_rs3_times$cm_time = ifelse(tr_da_01_h_rs3_times$DA==0,0,tr_da_01_h_rs3_times$cm_time)
tr_da_01_h_rs3_times$ct_time = ifelse(tr_da_01_h_rs3_times$DA==0,0,tr_da_01_h_rs3_times$ct_time)
tr_da_01_h_rs3_times$age_notional_months_times = tr_da_01_h_rs3_times$age_notional_months + tr_da_01_h_rs3_times$cm_time +tr_da_01_h_rs3_times$ct_time

tr_da_01_h_rs3_times$investor = ifelse(tr_da_01_h_rs3_times$months_bet_sales<=24,1,0)
tr_da_01_h_rs3_times$investor_da = ifelse( (tr_da_01_h_rs3_times$DA==0 | tr_da_01_h_rs3_times$months_bet_sales>24), ifelse(tr_da_01_h_rs3_times$age_notional_months <=24,1,0), NA)


tr_da_01_h_rs3_cond$investor = ifelse(tr_da_01_h_rs3_cond$months_bet_sales<=154,1,0)


conditions =  (tr_da_01_h_rs3_times$selfselect2 == 1 | tr_da_01_h_rs3_times$selfselect2 ==0) &
  tr_da_01_h_rs3_times$purchase_date_notional >= as.Date("2004-03-01") & 
  tr_da_01_h_rs3_times$vacantb==0 & tr_da_01_h_rs3_times$vacant_s==0 & tr_da_01_h_rs3_times$vacant_bs==0 &
  tr_da_01_h_rs3_times$prev_sale_price >= 40000 & tr_da_01_h_rs3_times$prev_sale_price<=5500000 & 
  tr_da_01_h_rs3_times$contract_price >= 40000 & tr_da_01_h_rs3_times$contract_price <=5500000
tr_da_01_h_rs3_cond = subset(tr_da_01_h_rs3_times,conditions)
## below is for count
cls <- split(tr_da_01_h_rs3_cond$property_id, list(tr_da_01_h_rs3_cond$DA, tr_da_01_h_rs3_cond$investor))
sapply(cls, function(dat) sum(!is.na(dat)))




m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
               DA
               + log(ssd_index_cur_sale2/ssd_index_purchase_notional)
               + age_purchase_notional + I(age_purchase_notional^2)
               + I(beds_cur_sale-beds_prev_sale) 
               + I(baths_curr_sale-baths_prev_sale) 
               + I(cars_curr_sale-cars_prev_sale)
               + state + as.factor(year(purchase_date_notional))
               , (selfselect2 == 1 | selfselect2 ==0) & 
        prev_sale_price >= 40000 & prev_sale_price<=5500000 
        & contract_price >= 40000 & contract_price <=5500000 & 
        purchase_date_notional >= as.Date("2004-03-01") & 
        vacantb==0 & vacant_s==0 & vacant_bs ==0 
              , data = tr_da_01_h_rs3)


m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
          + DA 
          + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
          + age_notional_months_times + I(age_notional_months_times**2) 
          + I(beds_cur_sale-beds_prev_sale) 
          + I(baths_curr_sale-baths_prev_sale) 
          + I(cars_curr_sale-cars_prev_sale) 
          + state + as.factor(year(purchase_date_notional))
     ,(selfselect2 == 1 | selfselect2 ==0)   &
        prev_sale_price >= 40000 & prev_sale_price<=5500000 &
        contract_price >= 40000 & contract_price <=5500000 & 
        purchase_date_notional >= as.Date("2004-03-01") & 
        vacantb==0 & vacant_s==0 & vacant_bs ==0 
                  , data = tr_da_01_h_rs3_times )


m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ -1+
          building_type_compact2 * log(ssd_index_cur_sale2/ssd_index_purchase_notional)
        + building_type_compact2*age_purchase_notional 
        + building_type_compact2*contract_date2
        + I(beds_cur_sale-beds_prev_sale) + I(baths_curr_sale-baths_prev_sale) + I(cars_curr_sale-cars_prev_sale)
        + state
        , (selfselect2 == 1 | selfselect2 ==1) & 
          prev_sale_price >= 40000 & prev_sale_price<=5500000 & contract_price >= 40000 & contract_price <=5500000 & 
          purchase_date_notional >= as.Date("2004-03-01") & 
          vacantb==0 & vacant_s==0 & vacant_bs ==0
        , data = tr_da_01_h_rs3)

m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
          building_type_compact2 * log(ssd_index_cur_sale2/ssd_index_purchase_notional)
        + building_type_compact2*age_purchase_notional  + building_type_compact2*contract_date2
        + I(beds_cur_sale-beds_prev_sale) + I(baths_curr_sale-baths_prev_sale) + I(cars_curr_sale-cars_prev_sale)
        + as.factor(SSDcode), (selfselect2 == 1 | selfselect2 ==1) & 
          prev_sale_price >= 40000 & prev_sale_price<=5500000 & contract_price >= 40000 & contract_price <=5500000 & 
          purchase_date_notional >= as.Date("2004-03-01") & 
          vacantb==0 & vacant_s==0 & vacant_bs ==0
        , data = tr_da_01_h_rs3)

# above self selection methods is using postcode and suburb this is deeper and serves two purposes .. one it is more closer match to the area so captures the neighbourhood caracteristics..and also if that suburb and postcode does not ahve a DA then we exclude such suburb -postcode from the non da.. as that comibation may not have data provided by the councils..




self = selfselect_suburb
self$postcode = NULL
self = unique(self)
tr_da_01_h_rs4 = merge(tr_da_01_h_rs2, self, by = c("state","suburb"), all.x = TRUE)


self = selfselect_suburb
self$suburb = NULL
self = unique(self)
tr_da_01_h_rs4 = merge(tr_da_01_h_rs2, self, by = c("state","postcode"), all.x = TRUE)

## we have used the date condition of purchase notional which is the preva sale date for non da and da date for da home.. 
##in the modelling we take all properties greter with previous sale than 2004 and da date greater than 2004 for non da and da homeser than 2004 mar and not the notional date..
## just for the distribution plot I have used all sales greater than 2004 for all properties but all other statistics are based on notionioal pruchase > 2004


conditions = tr_da_01_h_rs3$prev_sale_date2 >= as.Date("2004-03-01") & 
  tr_da_01_h_rs3$vacantb==0 & tr_da_01_h_rs3$vacant_s==0 & tr_da_01_h_rs3$vacant_bs==0 &
  tr_da_01_h_rs3$prev_sale_price >= 40000 & tr_da_01_h_rs3$prev_sale_price<=5500000 & tr_da_01_h_rs3$contract_price >= 40000 & tr_da_01_h_rs3$contract_price <=5500000  &
  (tr_da_01_h_rs3$selfselect2 == 1 | tr_da_01_h_rs3$selfselect2 ==1)





s=1

### 1. below is creating the table now.. 2models = self select 0 and 1 for council bias correction


#1
m_da <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
          + DA 
        + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
        + age_notional_months_times + I(age_notional_months_times**2)
        + I(beds_cur_sale - beds_prev_sale)
        + I(baths_curr_sale - baths_prev_sale)
        + I(cars_curr_sale - cars_prev_sale)
        + as.factor(year(purchase_date_notional)) + as.factor(state) 
        ,(selfselect2 == 1 | selfselect2 == s) &
          prev_sale_price >= 40000 & prev_sale_price<=5500000 &
          contract_price >= 40000 & contract_price <=5500000 & 
          purchase_date_notional >= as.Date("2004-03-01") & 
          vacantb==0 & vacant_s==0 & vacant_bs ==0 
        , data = tr_da_01_h_rs3_times )


m_da2 <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
             + DA 
           + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
           + age_notional_months_times + I(age_notional_months_times**2)
           + beds_cur_sale 
           + baths_curr_sale 
           + cars_curr_sale
           + beds_prev_sale
           + baths_prev_sale
           + cars_prev_sale
           + as.factor(year(purchase_date_notional)) + as.factor(state) 
          ,(selfselect2 == 1 | selfselect2 == s) &
             prev_sale_price >= 40000 & prev_sale_price<=5500000 &
             contract_price >= 40000 & contract_price <=5500000 & 
             purchase_date_notional >= as.Date("2004-03-01") & 
             vacantb==0 & vacant_s==0 & vacant_bs ==0 
           , data = tr_da_01_h_rs3_times )





### 1. below is creating the table now... run with selfselect = 0 and 1 (2 models)

file_hp = capture.output(cat('log_return_',s,'.html',sep = ''))

#1

m_change <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
          + building_type_compact2 
        + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
        + age_notional_months_times + I(age_notional_months_times**2)
        + I(beds_cur_sale - beds_prev_sale)
        + I(baths_curr_sale - baths_prev_sale)
        + I(cars_curr_sale - cars_prev_sale)
        + as.factor(year(purchase_date_notional)) + as.factor(state) 
        ,(selfselect2 == 1 | selfselect2 == s) &
          prev_sale_price >= 40000 & prev_sale_price<=5500000 &
          contract_price >= 40000 & contract_price <=5500000 & 
          purchase_date_notional >= as.Date("2004-03-01") & 
          vacantb==0 & vacant_s==0 & vacant_bs ==0 
        , data = tr_da_01_h_rs3_times )


m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
          + building_type_compact2 
        + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
        + age_notional_months_times + I(age_notional_months_times**2)
        + beds_cur_sale
        + baths_curr_sale
        + cars_curr_sale
        + beds_prev_sale
        + baths_prev_sale
        + cars_prev_sale
        + as.factor(year(purchase_date_notional)) + as.factor(state) 
        ,(selfselect2 == 1 | selfselect2 == s) &
          prev_sale_price >= 40000 & prev_sale_price<=5500000 &
          contract_price >= 40000 & contract_price <=5500000 & 
          purchase_date_notional >= as.Date("2004-03-01") & 
          vacantb==0 & vacant_s==0 & vacant_bs ==0 
        , data = tr_da_01_h_rs3_times )



m_ex_age2 <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
          + building_type_compact2 
        + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
        + age_notional_months_times 
        + beds_cur_sale
        + baths_curr_sale
        + cars_curr_sale
        + beds_prev_sale
        + baths_prev_sale
        + cars_prev_sale
        + as.factor(year(purchase_date_notional)) + as.factor(state) 
        ,(selfselect2 == 1 | selfselect2 == s) &
          prev_sale_price >= 40000 & prev_sale_price<=5500000 &
          contract_price >= 40000 & contract_price <=5500000 & 
          purchase_date_notional >= as.Date("2004-03-01") & 
          vacantb==0 & vacant_s==0 & vacant_bs ==0 
        , data = tr_da_01_h_rs3_times )



#7
m_ex_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                      + building_type_compact2 
                    + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                    + beds_cur_sale
                    + baths_curr_sale  
                    + cars_curr_sale
                    + beds_prev_sale
                    + baths_prev_sale 
                    + cars_prev_sale 
                    + as.factor(year(purchase_date_notional)) + as.factor(state) 
                    ,(selfselect2 == 1 | selfselect2 == s) &
                      prev_sale_price >= 40000 & prev_sale_price<=5500000 &
                      contract_price >= 40000 & contract_price <=5500000 & 
                      purchase_date_notional >= as.Date("2004-03-01") & 
                      vacantb==0 & vacant_s==0 & vacant_bs ==0 
                    , data = tr_da_01_h_rs3_times )



#9
m_ex_prev_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                           + building_type_compact2 
                         + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                         + beds_cur_sale
                         + baths_curr_sale
                         + cars_curr_sale
                         + as.factor(year(purchase_date_notional)) + as.factor(state) 
                         ,(selfselect2 == 1 | selfselect2 == s) &
                           prev_sale_price >= 40000 & prev_sale_price<=5500000 &
                           contract_price >= 40000 & contract_price <=5500000 & 
                           purchase_date_notional >= as.Date("2004-03-01") & 
                           vacantb==0 & vacant_s==0 & vacant_bs ==0 
                         , data = tr_da_01_h_rs3_times )

#10
m_ex_cur_prev_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                               + building_type_compact2 
                             + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                             + as.factor(year(purchase_date_notional)) + as.factor(state) 
                             ,(selfselect2 == 1 | selfselect2 == s) &
                               prev_sale_price >= 40000 & prev_sale_price<=5500000 &
                               contract_price >= 40000 & contract_price <=5500000 & 
                               purchase_date_notional >= as.Date("2004-03-01") & 
                               vacantb==0 & vacant_s==0 & vacant_bs ==0 
                             , data = tr_da_01_h_rs3_times )



stargazer(m_ex_cur_prev_age2_age, m_ex_prev_age2_age, m_ex_age2_age, m_ex_age2, m, m_change, m_da2, m_da,
          # title = "" , 
          style = "aer", out = file_hp, float = TRUE, float.env = "sidewaystable", align = TRUE, digits = 3,
          omit = c("year","state"), 
          #  omit.labels = c("State"),
          keep.stat = c("n","adj.rsq","f"),
          # omit.stat = c("rsq","f"),
          
          add.lines = list(c("Year Fixed Effects","Yes","Yes","Yes","Yes","Yes","Yes","Yes"), 
                           c("Location Fixed Effects","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
          se=list( summary(m_ex_cur_prev_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_prev_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_age2,robust=T)$coef[,"Std. Error"],
                   summary(m,robust=T)$coef[,"Std. Error"],
                   summary(m_change,robust=T)$coef[,"Std. Error"],
                   summary(m_da2,robust=T)$coef[,"Std. Error"],
                   summary(m_da,robust=T)$coef[,"Std. Error"]
                   ),
          # dep.var.caption = "Model excluding Duplex", 
          dep.var.labels.include = T, dep.var.labels = "Log(Resale Price/Purchase Price (Notional))",
          covariate.labels = c("DA","Carports/Garages/Sheds", "Duplex", "Extension/Alteration", "House/Single Dwelling", 
                               "Multiple DA", "Swimming Pool", "Verandahs/Pergolas", "MktReturn", 
                               "Months between sales", "Months between sales squared" , 
                               "No. of beds (resale)","No. of baths (resale)","No. of cars (resale)", 
                              "No. of beds (purchase)","No. of baths (purchase)","No. of cars (purchase)", "Change in \\# of beds", "Change in \\# of baths", "Change in \\# of cars"),
          order = c(8),
         notes = "Robust standard errors in parentheses. *** 0.1% significance ** 1% significance * 5% significance . 10% significance", notes.append = F)


#rm(m_ex_cur_prev_age2_age,m_ex_prev_age2_age,m_ex_age2_age,m_ex_age2,m,m_change,m_da)


### below is for prev sale >2004

s=1 

# s is only =1
### 1. below is creating the table now.. 2models = self select 0 and 1 for council bias correction


#1
m_da <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
             + DA 
           + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
           + age_notional_months_times + I(age_notional_months_times**2)
           + I(beds_cur_sale - beds_prev_sale)
           + I(baths_curr_sale - baths_prev_sale)
           + I(cars_curr_sale - cars_prev_sale)
           + as.factor(year(purchase_date_notional)) + as.factor(state) 
           ,(selfselect2 == 1 | selfselect2 == s) &
             prev_sale_price >= 40000 & prev_sale_price<=5500000 &
             contract_price >= 40000 & contract_price <=5500000 & 
             prev_sale_date2 >= as.Date("2004-03-01") & 
             vacantb==0 & vacant_s==0 & vacant_bs ==0 
           , data = tr_da_01_h_rs3_times )


m_da2 <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
             + DA 
           + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
           + age_notional_months_times + I(age_notional_months_times**2)
           + beds_cur_sale 
           + baths_curr_sale
           + cars_curr_sale 
           + beds_prev_sale
           + baths_prev_sale
           + cars_prev_sale
           + as.factor(year(purchase_date_notional)) + as.factor(state) 
          ,(selfselect2 == 1 | selfselect2 == s) &
             prev_sale_price >= 40000 & prev_sale_price<=5500000 &
             contract_price >= 40000 & contract_price <=5500000 & 
             prev_sale_date2 >= as.Date("2004-03-01") & 
             vacantb==0 & vacant_s==0 & vacant_bs ==0 
          , data = tr_da_01_h_rs3_times )


### 1. below is creating the table now... run with selfselect = 0 and 1 (2 models)

file_hp = capture.output(cat('log_return_prev2004_',s,'.html',sep = ''))

#1

m_change <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                 + building_type_compact2 
               + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
               + age_notional_months_times + I(age_notional_months_times**2)
               + I(beds_cur_sale - beds_prev_sale)
               + I(baths_curr_sale - baths_prev_sale)
               + I(cars_curr_sale - cars_prev_sale)
               + as.factor(year(purchase_date_notional)) + as.factor(state) 
               ,(selfselect2 == 1 | selfselect2 == s) &
                 prev_sale_price >= 40000 & prev_sale_price<=5500000 &
                 contract_price >= 40000 & contract_price <=5500000 & 
                 prev_sale_date2 >= as.Date("2004-03-01") & 
                 vacantb==0 & vacant_s==0 & vacant_bs ==0 
               , data = tr_da_01_h_rs3_times )


m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
          + building_type_compact2 
        + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
        + age_notional_months_times + I(age_notional_months_times**2)
        + beds_cur_sale
        + baths_curr_sale
        + cars_curr_sale
        + beds_prev_sale
        + baths_prev_sale
        + cars_prev_sale
        + as.factor(year(purchase_date_notional)) + as.factor(state) 
        ,(selfselect2 == 1 | selfselect2 == s) &
          prev_sale_price >= 40000 & prev_sale_price<=5500000 &
          contract_price >= 40000 & contract_price <=5500000 & 
          prev_sale_date2 >= as.Date("2004-03-01") & 
          vacantb==0 & vacant_s==0 & vacant_bs ==0 
        , data = tr_da_01_h_rs3_times )



m_ex_age2 <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                  + building_type_compact2 
                + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                + age_notional_months_times 
                + beds_cur_sale
                + baths_curr_sale
                + cars_curr_sale
                + beds_prev_sale
                + baths_prev_sale
                + cars_prev_sale
                + as.factor(year(purchase_date_notional)) + as.factor(state) 
                ,(selfselect2 == 1 | selfselect2 == s) &
                  prev_sale_price >= 40000 & prev_sale_price<=5500000 &
                  contract_price >= 40000 & contract_price <=5500000 & 
                  prev_sale_date2 >= as.Date("2004-03-01") & 
                  vacantb==0 & vacant_s==0 & vacant_bs ==0 
                , data = tr_da_01_h_rs3_times )



#7
m_ex_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                      + building_type_compact2 
                    + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                    + beds_cur_sale
                    + baths_curr_sale  
                    + cars_curr_sale
                    + beds_prev_sale
                    + baths_prev_sale 
                    + cars_prev_sale 
                    + as.factor(year(purchase_date_notional)) + as.factor(state) 
                    ,(selfselect2 == 1 | selfselect2 == s) &
                      prev_sale_price >= 40000 & prev_sale_price<=5500000 &
                      contract_price >= 40000 & contract_price <=5500000 & 
                      prev_sale_date2 >= as.Date("2004-03-01") & 
                      vacantb==0 & vacant_s==0 & vacant_bs ==0 
                    , data = tr_da_01_h_rs3_times )



#9
m_ex_prev_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                           + building_type_compact2 
                         + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                         + beds_cur_sale
                         + baths_curr_sale
                         + cars_curr_sale
                         + as.factor(year(purchase_date_notional)) + as.factor(state) 
                         ,(selfselect2 == 1 | selfselect2 == s) &
                           prev_sale_price >= 40000 & prev_sale_price<=5500000 &
                           contract_price >= 40000 & contract_price <=5500000 & 
                           prev_sale_date2 >= as.Date("2004-03-01") & 
                           vacantb==0 & vacant_s==0 & vacant_bs ==0 
                         , data = tr_da_01_h_rs3_times )

#10
m_ex_cur_prev_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                               + building_type_compact2 
                             + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                             + as.factor(year(purchase_date_notional)) + as.factor(state) 
                             ,(selfselect2 == 1 | selfselect2 == s) &
                               prev_sale_price >= 40000 & prev_sale_price<=5500000 &
                               contract_price >= 40000 & contract_price <=5500000 & 
                               prev_sale_date2 >= as.Date("2004-03-01") & 
                               vacantb==0 & vacant_s==0 & vacant_bs ==0 
                             , data = tr_da_01_h_rs3_times )



stargazer(m_ex_cur_prev_age2_age, m_ex_prev_age2_age, m_ex_age2_age, m_ex_age2, m, m_change, m_da2, m_da,
          # title = "" , 
          style = "aer", out = file_hp, float = TRUE, float.env = "sidewaystable", align = TRUE, digits = 3,
          omit = c("year","state"), 
          #  omit.labels = c("State"),
          keep.stat = c("n","adj.rsq","f"),
          # omit.stat = c("rsq","f"),
          
          add.lines = list(c("Year Fixed Effects","Yes","Yes","Yes","Yes","Yes","Yes","Yes"), 
                           c("Location Fixed Effects","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
          se=list( summary(m_ex_cur_prev_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_prev_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_age2,robust=T)$coef[,"Std. Error"],
                   summary(m,robust=T)$coef[,"Std. Error"],
                   summary(m_change,robust=T)$coef[,"Std. Error"],
                   summary(m_da2,robust=T)$coef[,"Std. Error"],
                   summary(m_da,robust=T)$coef[,"Std. Error"]
          ),
          # dep.var.caption = "Model excluding Duplex", 
          dep.var.labels.include = T, dep.var.labels = "Log(Resale Price/Purchase Price (Notional))",
          covariate.labels = c("DA","Carports/Garages/Sheds", "Duplex", "Extension/Alteration", "House/Single Dwelling", 
                               "Multiple DA", "Swimming Pool", "Verandahs/Pergolas", "MktReturn", 
                               "Months between sales", "Months between sales squared" , 
                               "No. of beds (resale)","No. of baths (resale)","No. of cars (resale)", 
                               "No. of beds (purchase)","No. of baths (purchase)","No. of cars (purchase)", "Change in \\# of beds", "Change in \\# of baths", "Change in \\# of cars"),
          order = c(8),
          notes = "Robust standard errors in parentheses. *** 0.1% significance ** 1% significance * 5% significance . 10% significance", notes.append = F)


### above is for prev sale >2004











#### below is for only DA obs...


### above is for only DA .....



### below is for SSD level

s=0
file_hp = capture.output(cat('log_return_SSD_',s,'.html',sep = ''))

#1
m_da <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
             + DA 
           + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
           + age_notional_months_times + I(age_notional_months_times**2)
           + I(beds_cur_sale - beds_prev_sale)
           + I(baths_curr_sale - baths_prev_sale)
           + I(cars_curr_sale - cars_prev_sale)
           + as.factor(year(purchase_date_notional)) + as.factor(SSDcode) 
           ,(selfselect2 == 1 | selfselect2 == s) &
             prev_sale_price >= 40000 & prev_sale_price<=5500000 &
             contract_price >= 40000 & contract_price <=5500000 & 
             purchase_date_notional >= as.Date("2004-03-01") & 
             vacantb==0 & vacant_s==0 & vacant_bs ==0 
           , data = tr_da_01_h_rs3_times )


m_da2 <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
             + DA 
           + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
           + age_notional_months_times + I(age_notional_months_times**2)
           + beds_cur_sale 
           + baths_curr_sale 
           + cars_curr_sale 
           + beds_prev_sale
           + baths_prev_sale
           + cars_prev_sale
           + as.factor(year(purchase_date_notional)) + as.factor(SSDcode) 
           ,(selfselect2 == 1 | selfselect2 == s) &
             prev_sale_price >= 40000 & prev_sale_price<=5500000 &
             contract_price >= 40000 & contract_price <=5500000 & 
             purchase_date_notional >= as.Date("2004-03-01") & 
             vacantb==0 & vacant_s==0 & vacant_bs ==0 
           , data = tr_da_01_h_rs3_times )







### 1. below is creating the table now... run with selfselect = 0 and 1 (2 models)



#1

m_change <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                 + building_type_compact2 
               + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
               + age_notional_months_times + I(age_notional_months_times**2)
               + I(beds_cur_sale - beds_prev_sale)
               + I(baths_curr_sale - baths_prev_sale)
               + I(cars_curr_sale - cars_prev_sale)
               + as.factor(year(purchase_date_notional)) + as.factor(SSDcode) 
               ,(selfselect2 == 1 | selfselect2 == s) &
                 prev_sale_price >= 40000 & prev_sale_price<=5500000 &
                 contract_price >= 40000 & contract_price <=5500000 & 
                 purchase_date_notional >= as.Date("2004-03-01") & 
                 vacantb==0 & vacant_s==0 & vacant_bs ==0 
               , data = tr_da_01_h_rs3_times )


m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
          + building_type_compact2 
        + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
        + age_notional_months_times + I(age_notional_months_times**2)
        + beds_cur_sale
        + baths_curr_sale
        + cars_curr_sale
        + beds_prev_sale
        + baths_prev_sale
        + cars_prev_sale
        + as.factor(year(purchase_date_notional)) + as.factor(SSDcode) 
        ,(selfselect2 == 1 | selfselect2 == s) &
          prev_sale_price >= 40000 & prev_sale_price<=5500000 &
          contract_price >= 40000 & contract_price <=5500000 & 
          purchase_date_notional >= as.Date("2004-03-01") & 
          vacantb==0 & vacant_s==0 & vacant_bs ==0 
        , data = tr_da_01_h_rs3_times )



m_ex_age2 <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
          + building_type_compact2 
        + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
        + age_notional_months_times
        + beds_cur_sale
        + baths_curr_sale
        + cars_curr_sale
        + beds_prev_sale
        + baths_prev_sale
        + cars_prev_sale
        + as.factor(year(purchase_date_notional)) + as.factor(SSDcode) 
        ,(selfselect2 == 1 | selfselect2 == s) &
          prev_sale_price >= 40000 & prev_sale_price<=5500000 &
          contract_price >= 40000 & contract_price <=5500000 & 
          purchase_date_notional >= as.Date("2004-03-01") & 
          vacantb==0 & vacant_s==0 & vacant_bs ==0 
        , data = tr_da_01_h_rs3_times )



#7
m_ex_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                      + building_type_compact2 
                    + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                    + beds_cur_sale
                    + baths_curr_sale  
                    + cars_curr_sale
                    + beds_prev_sale
                    + baths_prev_sale 
                    + cars_prev_sale 
                    + as.factor(year(purchase_date_notional)) + as.factor(SSDcode) 
                    ,(selfselect2 == 1 | selfselect2 == s) &
                      prev_sale_price >= 40000 & prev_sale_price<=5500000 &
                      contract_price >= 40000 & contract_price <=5500000 & 
                      purchase_date_notional >= as.Date("2004-03-01") & 
                      vacantb==0 & vacant_s==0 & vacant_bs ==0 
                    , data = tr_da_01_h_rs3_times )



#9
m_ex_prev_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                           + building_type_compact2 
                         + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                         + beds_cur_sale
                         + baths_curr_sale
                         + cars_curr_sale
                         + as.factor(year(purchase_date_notional)) + as.factor(SSDcode) 
                         ,(selfselect2 == 1 | selfselect2 == s) &
                           prev_sale_price >= 40000 & prev_sale_price<=5500000 &
                           contract_price >= 40000 & contract_price <=5500000 & 
                           purchase_date_notional >= as.Date("2004-03-01") & 
                           vacantb==0 & vacant_s==0 & vacant_bs ==0 
                         , data = tr_da_01_h_rs3_times )

#10
m_ex_cur_prev_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                               + building_type_compact2 
                             + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                             + as.factor(year(purchase_date_notional)) + as.factor(SSDcode) 
                             ,(selfselect2 == 1 | selfselect2 == s) &
                               prev_sale_price >= 40000 & prev_sale_price<=5500000 &
                               contract_price >= 40000 & contract_price <=5500000 & 
                               purchase_date_notional >= as.Date("2004-03-01") & 
                               vacantb==0 & vacant_s==0 & vacant_bs ==0 
                             , data = tr_da_01_h_rs3_times )


stargazer(m_ex_cur_prev_age2_age, m_ex_prev_age2_age, m_ex_age2_age, m_ex_age2, m, m_change, m_da2, m_da,
          # title = "" , 
          style = "aer", out = file_hp, float = TRUE, float.env = "sidewaystable", align = TRUE, digits = 3,
          omit = c("year","SSDcode"), 
          #  omit.labels = c("State"),
          keep.stat = c("n","adj.rsq","f"),
          # omit.stat = c("rsq","f"),
          
          add.lines = list(c("Year Fixed Effects","Yes","Yes","Yes","Yes","Yes","Yes","Yes"), 
                           c("Location Fixed Effects","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
          se=list( summary(m_ex_cur_prev_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_prev_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_age2,robust=T)$coef[,"Std. Error"],
                   summary(m,robust=T)$coef[,"Std. Error"],
                   summary(m_change,robust=T)$coef[,"Std. Error"],
                   summary(m_da2,robust=T)$coef[,"Std. Error"],
                   summary(m_da,robust=T)$coef[,"Std. Error"]
          ),
          # dep.var.caption = "Model excluding Duplex", 
          dep.var.labels.include = T, dep.var.labels = "Log(Resale Price/Purchase Price (Notional))",
          covariate.labels = c("DA","Carports/Garages/Sheds", "Duplex", "Extension/Alteration", "House/Single Dwelling", 
                               "Multiple DA", "Swimming Pool", "Verandahs/Pergolas", "MktReturn", 
                               "Months between sales", "Months between sales squared" , 
                               "No. of beds (resale)","No. of baths (resale)","No. of cars (resale)", 
                               "No. of beds (purchase)","No. of baths (purchase)","No. of cars (purchase)", "Change in \\# of beds", "Change in \\# of baths", "Change in \\# of cars"),
          order = c(8),
          notes = "Robust standard errors in parentheses. *** 0.1% significance ** 1% significance * 5% significance . 10% significance", notes.append = F)



### below is for investor run this model for investor and non investor


s=0

### 1.


### 1. below is creating the table now... run with selfselect = 0 and 1 (2 models)

file_hp = capture.output(cat('log_return_investor_DA_type_',s,'.html',sep = ''))

#1

m_change <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                 + building_type_compact2 
               + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
               + age_notional_months_times + I(age_notional_months_times**2)
               + I(beds_cur_sale - beds_prev_sale)
               + I(baths_curr_sale - baths_prev_sale)
               + I(cars_curr_sale - cars_prev_sale)
               + as.factor(year(purchase_date_notional)) + as.factor(state) 
               ,(selfselect2 == 1 | selfselect2 == s) &
                 prev_sale_price >= 40000 & prev_sale_price<=5500000 & (months_bet_sales <= 24) &
                 contract_price >= 40000 & contract_price <=5500000 & 
                 purchase_date_notional >= as.Date("2004-03-01") & 
                 vacantb==0 & vacant_s==0 & vacant_bs ==0 
               , data = tr_da_01_h_rs3_times )


m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
          + building_type_compact2 
        + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
        + age_notional_months_times + I(age_notional_months_times**2)
        + beds_cur_sale
        + baths_curr_sale
        + cars_curr_sale
        + beds_prev_sale
        + baths_prev_sale
        + cars_prev_sale
        + as.factor(year(purchase_date_notional)) + as.factor(state) 
        ,(selfselect2 == 1 | selfselect2 == s) &
          prev_sale_price >= 40000 & prev_sale_price<=5500000 & (months_bet_sales <= 24) &
          contract_price >= 40000 & contract_price <=5500000 & 
          purchase_date_notional >= as.Date("2004-03-01") & 
          vacantb==0 & vacant_s==0 & vacant_bs ==0 
        , data = tr_da_01_h_rs3_times )



m_ex_age2 <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                  + building_type_compact2 
                + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                + age_notional_months_times 
                + beds_cur_sale
                + baths_curr_sale
                + cars_curr_sale
                + beds_prev_sale
                + baths_prev_sale
                + cars_prev_sale
                + as.factor(year(purchase_date_notional)) + as.factor(state) 
                ,(selfselect2 == 1 | selfselect2 == s) &
                  prev_sale_price >= 40000 & prev_sale_price<=5500000 & (months_bet_sales <= 24) &
                  contract_price >= 40000 & contract_price <=5500000 & 
                  purchase_date_notional >= as.Date("2004-03-01") & 
                  vacantb==0 & vacant_s==0 & vacant_bs ==0 
                , data = tr_da_01_h_rs3_times )



#7
m_ex_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                      + building_type_compact2 
                    + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                    + beds_cur_sale
                    + baths_curr_sale  
                    + cars_curr_sale
                    + beds_prev_sale
                    + baths_prev_sale 
                    + cars_prev_sale 
                    + as.factor(year(purchase_date_notional)) + as.factor(state) 
                    ,(selfselect2 == 1 | selfselect2 == s) &
                      prev_sale_price >= 40000 & prev_sale_price<=5500000 & (months_bet_sales <= 24) &
                      contract_price >= 40000 & contract_price <=5500000 & 
                      purchase_date_notional >= as.Date("2004-03-01") & 
                      vacantb==0 & vacant_s==0 & vacant_bs ==0 
                    , data = tr_da_01_h_rs3_times )



#9
m_ex_prev_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                           + building_type_compact2 
                         + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                         + beds_cur_sale
                         + baths_curr_sale
                         + cars_curr_sale
                         + as.factor(year(purchase_date_notional)) + as.factor(state) 
                         ,(selfselect2 == 1 | selfselect2 == s) &
                           prev_sale_price >= 40000 & prev_sale_price<=5500000 & (months_bet_sales <= 24) &
                           contract_price >= 40000 & contract_price <=5500000 & 
                           purchase_date_notional >= as.Date("2004-03-01") & 
                           vacantb==0 & vacant_s==0 & vacant_bs ==0 
                         , data = tr_da_01_h_rs3_times )

#10
m_ex_cur_prev_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                               + building_type_compact2 
                             + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                             + as.factor(year(purchase_date_notional)) + as.factor(state) 
                             ,(selfselect2 == 1 | selfselect2 == s) &
                               prev_sale_price >= 40000 & prev_sale_price<=5500000 & (months_bet_sales <= 24) &
                               contract_price >= 40000 & contract_price <=5500000 & 
                               purchase_date_notional >= as.Date("2004-03-01") & 
                               vacantb==0 & vacant_s==0 & vacant_bs ==0 
                             , data = tr_da_01_h_rs3_times )



stargazer(m_ex_cur_prev_age2_age, m_ex_prev_age2_age, m_ex_age2_age, m_ex_age2, m, m_change,
          # title = "" , 
          style = "aer", out = file_hp, float = TRUE, float.env = "sidewaystable", align = TRUE, digits = 3,
          omit = c("year","state"), 
          #  omit.labels = c("State"),
          keep.stat = c("n","adj.rsq","f"),
          # omit.stat = c("rsq","f"),
          
          add.lines = list(c("Year Fixed Effects","Yes","Yes","Yes","Yes","Yes","Yes"), 
                           c("Location Fixed Effects","Yes","Yes","Yes","Yes","Yes","Yes")),
          se=list( summary(m_ex_cur_prev_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_prev_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_age2,robust=T)$coef[,"Std. Error"],
                   summary(m,robust=T)$coef[,"Std. Error"],
                   summary(m_change,robust=T)$coef[,"Std. Error"]
                   
          ),
          # dep.var.caption = "Model excluding Duplex", 
          dep.var.labels.include = T, dep.var.labels = "Log(Resale Price/Purchase Price (Notional))",
         # covariate.labels = c("DA","Carports/Garages/Sheds", "Duplex", "Extension/Alteration", "House/Single Dwelling", 
          #                     "Multiple DA", "Swimming Pool", "Verandahs/Pergolas", "MktReturn", 
           #                    "Months between sales", "Months between sales squared" , 
            #                   "No. of beds (resale)","No. of baths (resale)","No. of cars (resale)", 
             #                  "No. of beds (purchase)","No. of baths (purchase)","No. of cars (purchase)", "Change in \\# of beds", "Change in \\# of baths", "Change in \\# of cars"),
        #  order = c(8),
          notes = "Robust standard errors in parentheses. *** 0.1% significance ** 1% significance * 5% significance . 10% significance", notes.append = F)




s=0

### 1.


### 1. below is creating the table now... run with selfselect = 0 and 1 (2 models)

file_hp = capture.output(cat('log_return_investorDA_DA_type_',s,'.html',sep = ''))

#1

m_change <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                 + building_type_compact2 
               + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
               + age_notional_months_times + I(age_notional_months_times**2)
               + I(beds_cur_sale - beds_prev_sale)
               + I(baths_curr_sale - baths_prev_sale)
               + I(cars_curr_sale - cars_prev_sale)
               + as.factor(year(purchase_date_notional)) + as.factor(state) 
               ,(selfselect2 == 1 | selfselect2 == s) &
                 prev_sale_price >= 40000 & prev_sale_price<=5500000 & (investor_da ==1) &
                 contract_price >= 40000 & contract_price <=5500000 & 
                 purchase_date_notional >= as.Date("2004-03-01") & 
                 vacantb==0 & vacant_s==0 & vacant_bs ==0 
               , data = tr_da_01_h_rs3_times )


m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
          + building_type_compact2 
        + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
        + age_notional_months_times + I(age_notional_months_times**2)
        + beds_cur_sale
        + baths_curr_sale
        + cars_curr_sale
        + beds_prev_sale
        + baths_prev_sale
        + cars_prev_sale
        + as.factor(year(purchase_date_notional)) + as.factor(state) 
        ,(selfselect2 == 1 | selfselect2 == s) &
          prev_sale_price >= 40000 & prev_sale_price<=5500000 & (investor_da ==1) &
          contract_price >= 40000 & contract_price <=5500000 & 
          purchase_date_notional >= as.Date("2004-03-01") & 
          vacantb==0 & vacant_s==0 & vacant_bs ==0 
        , data = tr_da_01_h_rs3_times )



m_ex_age2 <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                  + building_type_compact2 
                + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                + age_notional_months_times 
                + beds_cur_sale
                + baths_curr_sale
                + cars_curr_sale
                + beds_prev_sale
                + baths_prev_sale
                + cars_prev_sale
                + as.factor(year(purchase_date_notional)) + as.factor(state) 
                ,(selfselect2 == 1 | selfselect2 == s) &
                  prev_sale_price >= 40000 & prev_sale_price<=5500000 & (investor_da  ==1) &
                  contract_price >= 40000 & contract_price <=5500000 & 
                  purchase_date_notional >= as.Date("2004-03-01") & 
                  vacantb==0 & vacant_s==0 & vacant_bs ==0 
                , data = tr_da_01_h_rs3_times )



#7
m_ex_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                      + building_type_compact2 
                    + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                    + beds_cur_sale
                    + baths_curr_sale  
                    + cars_curr_sale
                    + beds_prev_sale
                    + baths_prev_sale 
                    + cars_prev_sale 
                    + as.factor(year(purchase_date_notional)) + as.factor(state) 
                    ,(selfselect2 == 1 | selfselect2 == s) &
                      prev_sale_price >= 40000 & prev_sale_price<=5500000 & (investor_da  ==1) &
                      contract_price >= 40000 & contract_price <=5500000 & 
                      purchase_date_notional >= as.Date("2004-03-01") & 
                      vacantb==0 & vacant_s==0 & vacant_bs ==0 
                    , data = tr_da_01_h_rs3_times )



#9
m_ex_prev_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                           + building_type_compact2 
                         + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                         + beds_cur_sale
                         + baths_curr_sale
                         + cars_curr_sale
                         + as.factor(year(purchase_date_notional)) + as.factor(state) 
                         ,(selfselect2 == 1 | selfselect2 == s) &
                           prev_sale_price >= 40000 & prev_sale_price<=5500000 & (investor_da  ==1) &
                           contract_price >= 40000 & contract_price <=5500000 & 
                           purchase_date_notional >= as.Date("2004-03-01") & 
                           vacantb==0 & vacant_s==0 & vacant_bs ==0 
                         , data = tr_da_01_h_rs3_times )

#10
m_ex_cur_prev_age2_age <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                               + building_type_compact2 
                             + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                             + as.factor(year(purchase_date_notional)) + as.factor(state) 
                             ,(selfselect2 == 1 | selfselect2 == s) &
                               prev_sale_price >= 40000 & prev_sale_price<=5500000 & (investor_da ==1) &
                               contract_price >= 40000 & contract_price <=5500000 & 
                               purchase_date_notional >= as.Date("2004-03-01") & 
                               vacantb==0 & vacant_s==0 & vacant_bs ==0 
                             , data = tr_da_01_h_rs3_times )



stargazer(m_ex_cur_prev_age2_age, m_ex_prev_age2_age, m_ex_age2_age, m_ex_age2, m, m_change,
          # title = "" , 
          style = "aer", out = file_hp, float = TRUE, float.env = "sidewaystable", align = TRUE, digits = 3,
          omit = c("year","state"), 
          #  omit.labels = c("State"),
          keep.stat = c("n","adj.rsq","f"),
          # omit.stat = c("rsq","f"),
          
          add.lines = list(c("Year Fixed Effects","Yes","Yes","Yes","Yes","Yes","Yes"), 
                           c("Location Fixed Effects","Yes","Yes","Yes","Yes","Yes","Yes")),
          se=list( summary(m_ex_cur_prev_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_prev_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_age2,robust=T)$coef[,"Std. Error"],
                   summary(m,robust=T)$coef[,"Std. Error"],
                   summary(m_change,robust=T)$coef[,"Std. Error"]
                   
          ),
          # dep.var.caption = "Model excluding Duplex", 
          dep.var.labels.include = T, dep.var.labels = "Log(Resale Price/Purchase Price (Notional))",
          # covariate.labels = c("DA","Carports/Garages/Sheds", "Duplex", "Extension/Alteration", "House/Single Dwelling", 
          #                     "Multiple DA", "Swimming Pool", "Verandahs/Pergolas", "MktReturn", 
          #                    "Months between sales", "Months between sales squared" , 
          #                   "No. of beds (resale)","No. of baths (resale)","No. of cars (resale)", 
          #                  "No. of beds (purchase)","No. of baths (purchase)","No. of cars (purchase)", "Change in \\# of beds", "Change in \\# of baths", "Change in \\# of cars"),
          #  order = c(8),
          notes = "Robust standard errors in parentheses. *** 0.1% significance ** 1% significance * 5% significance . 10% significance", notes.append = F)









##this is for investor interact

s=0

### 1.

file_hp = capture.output(cat('log_return_investor_interact',s,'.html',sep = ''))


#1
m_da_change_interact <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
             + DA * investor
           + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
           + age_notional_months_times + I(age_notional_months_times**2)
           + I(beds_cur_sale - beds_prev_sale)
           + I(baths_curr_sale - baths_prev_sale)
           + I(cars_curr_sale - cars_prev_sale)
           + as.factor(year(purchase_date_notional)) + as.factor(state) 
           ,(selfselect2 == 1 | selfselect2 == s) &
             prev_sale_price >= 40000 & prev_sale_price<=5500000 & #(DA ==0 | months_bet_sales <=24)&
             contract_price >= 40000 & contract_price <=5500000 & 
             purchase_date_notional >= as.Date("2004-03-01") & 
             vacantb==0 & vacant_s==0 & vacant_bs ==0 
           , data = tr_da_01_h_rs3_times )


#2
m_da_interact <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
             + DA * investor
           + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
           + age_notional_months_times + I(age_notional_months_times**2)
           + beds_cur_sale
           + baths_curr_sale  
           + cars_curr_sale
           + beds_prev_sale
           + baths_prev_sale 
           + cars_prev_sale 
           + as.factor(year(purchase_date_notional)) + as.factor(state) 
           ,(selfselect2 == 1 | selfselect2 == s) &
             prev_sale_price >= 40000 & prev_sale_price<=5500000 & #(DA ==0 | months_bet_sales <=24)&
             contract_price >= 40000 & contract_price <=5500000 & 
             purchase_date_notional >= as.Date("2004-03-01") & 
             vacantb==0 & vacant_s==0 & vacant_bs ==0 
           , data = tr_da_01_h_rs3_times)


stargazer(m_da, m_da_change,
          # title = "" , 
          style = "aer", out = file_hp, float = TRUE, float.env = "sidewaystable", align = TRUE, digits = 3,
          omit = c("year","state"), 
          #  omit.labels = c("State"),
          keep.stat = c("n","adj.rsq","f"),
          # omit.stat = c("rsq","f"),
          
          add.lines = list(c("Year Fixed Effects","Yes","Yes"), 
                           c("Location Fixed Effects","Yes","Yes")),
           se=list( summary(m_da,robust=T)$coef[,"Std. Error"],
                   summary(m_da_change,robust=T)$coef[,"Std. Error"]
                 ),
          # dep.var.caption = "Model excluding Duplex", 
          dep.var.labels.include = T, dep.var.labels = "Log(Resale Price/Purchase Price (Notional))",
            covariate.labels = c("DA","Investor","MktReturn", 
                               "Months between sales", "Months between sales squared" , 
                              "No. of beds (resale)","No. of baths (resale)","No. of cars (resale)", 
                             "No. of beds (purchase)","No. of baths (purchase)","No. of cars (purchase)", "Change in \\# of beds", "Change in \\# of baths", "Change in \\# of cars", "DA:Investor"),
          #  order = c("DA",":","investor"),
         notes = "Robust standard errors in parentheses. *** 0.1% significance ** 1% significance * 5% significance . 10% significance", notes.append = F)






##this is for investor interact with investor DA i.e. those that are DA but months bet sale is greater than 24 month and age of DA is less than 24 months

s=0

### 1.

file_hp = capture.output(cat('log_return_investorDA_interact',s,'.html',sep = ''))


#1
m_da_change_interact <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
                    + DA * investor_da
                  + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
                  + age_notional_months_times + I(age_notional_months_times**2)
                  + I(beds_cur_sale - beds_prev_sale)
                  + I(baths_curr_sale - baths_prev_sale)
                  + I(cars_curr_sale - cars_prev_sale)
                  + as.factor(year(purchase_date_notional)) + as.factor(state) 
                  ,(selfselect2 == 1 | selfselect2 == s) &
                    prev_sale_price >= 40000 & prev_sale_price<=5500000 & #(DA ==0 | months_bet_sales <=24)&
                    contract_price >= 40000 & contract_price <=5500000 & 
                    purchase_date_notional >= as.Date("2004-03-01") & 
                    vacantb==0 & vacant_s==0 & vacant_bs ==0 
                  , data = tr_da_01_h_rs3_times )


#2
m_da_interact <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
             + DA * investor_da
           + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
           + age_notional_months_times + I(age_notional_months_times**2)
           + beds_cur_sale
           + baths_curr_sale  
           + cars_curr_sale
           + beds_prev_sale
           + baths_prev_sale 
           + cars_prev_sale 
           + as.factor(year(purchase_date_notional)) + as.factor(state) 
           ,(selfselect2 == 1 | selfselect2 == s) &
             prev_sale_price >= 40000 & prev_sale_price<=5500000 & #(DA ==0 | months_bet_sales <=24)&
             contract_price >= 40000 & contract_price <=5500000 & 
             purchase_date_notional >= as.Date("2004-03-01") & 
             vacantb==0 & vacant_s==0 & vacant_bs ==0 
           , data = tr_da_01_h_rs3_times)


stargazer(m_da, m_da_change,
          # title = "" , 
          style = "aer", out = file_hp, float = TRUE, float.env = "sidewaystable", align = TRUE, digits = 3,
          omit = c("year","state"), 
          #  omit.labels = c("State"),
          keep.stat = c("n","adj.rsq","f"),
          # omit.stat = c("rsq","f"),
          
          add.lines = list(c("Year Fixed Effects","Yes","Yes"), 
                           c("Location Fixed Effects","Yes","Yes")),
          se=list( summary(m_da,robust=T)$coef[,"Std. Error"],
                   summary(m_da_change,robust=T)$coef[,"Std. Error"]
          ),
          # dep.var.caption = "Model excluding Duplex", 
          dep.var.labels.include = T, dep.var.labels = "Log(Resale Price/Purchase Price (Notional))",
          covariate.labels = c("DA","Investor","MktReturn", 
                               "Months between sales", "Months between sales squared" , 
                               "No. of beds (resale)","No. of baths (resale)","No. of cars (resale)", 
                               "No. of beds (purchase)","No. of baths (purchase)","No. of cars (purchase)", "Change in \\# of beds", "Change in \\# of baths", "Change in \\# of cars", "DA:Investor"),
          #  order = c("DA",":","investor"),
          notes = "Robust standard errors in parentheses. *** 0.1% significance ** 1% significance * 5% significance . 10% significance", notes.append = F)





stargazer(m_ex_cur_prev_age2_age, m_ex_prev_age2_age, m_ex_age2_age, m_ex_age2, m, m_change,m_da_interact,m_da_change_interact,
          # title = "" , 
          style = "aer", out = file_hp, float = TRUE, float.env = "sidewaystable", align = TRUE, digits = 3,
          omit = c("year","state"), 
          #  omit.labels = c("State"),
          keep.stat = c("n","adj.rsq","f"),
          # omit.stat = c("rsq","f"),
          
          add.lines = list(c("Year Fixed Effects","Yes","Yes","Yes","Yes","Yes","Yes"), 
                           c("Location Fixed Effects","Yes","Yes","Yes","Yes","Yes","Yes")),
          se=list( summary(m_ex_cur_prev_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_prev_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_age2_age,robust=T)$coef[,"Std. Error"],
                   summary(m_ex_age2,robust=T)$coef[,"Std. Error"],
                   summary(m,robust=T)$coef[,"Std. Error"],
                   summary(m_change,robust=T)$coef[,"Std. Error"],
                   summary(m_da_interact,robust=T)$coef[,"Std. Error"],
                   summary(m_da_change_interact,robust=T)$coef[,"Std. Error"]
          ),
          # dep.var.caption = "Model excluding Duplex", 
          dep.var.labels.include = T, dep.var.labels = "Log(Resale Price/Purchase Price (Notional))",
          # covariate.labels = c("DA","Carports/Garages/Sheds", "Duplex", "Extension/Alteration", "House/Single Dwelling", 
          #                     "Multiple DA", "Swimming Pool", "Verandahs/Pergolas", "MktReturn", 
          #                    "Months between sales", "Months between sales squared" , 
          #                   "No. of beds (resale)","No. of baths (resale)","No. of cars (resale)", 
          #                  "No. of beds (purchase)","No. of baths (purchase)","No. of cars (purchase)", "Change in \\# of beds", "Change in \\# of baths", "Change in \\# of cars"),
          #  order = c(8),
          notes = "Robust standard errors in parentheses. *** 0.1% significance ** 1% significance * 5% significance . 10% significance", notes.append = F)













# below function just computes all the coefficients
i=1
rm(x)
x = numeric(4)

for (i in 1:14){
  tr_da_01_h_rs3_times$investor = ifelse(tr_da_01_h_rs3_times$months_bet_sales<=i*12,1,0)
  m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
            + investor * DA
          + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
          + age_notional_months_times + I(age_notional_months_times**2) 
          + I(beds_cur_sale-beds_prev_sale) 
          + I(baths_curr_sale-baths_prev_sale) 
          + I(cars_curr_sale-cars_prev_sale) 
          + state + as.factor(year(purchase_date_notional))
          ,(selfselect2 == 1 | selfselect2 ==0)   &
            prev_sale_price >= 40000 & prev_sale_price<=5500000 &
            contract_price >= 40000 & contract_price <=5500000 & 
            purchase_date_notional >= as.Date("2004-03-01") & 
            vacantb==0 & vacant_s==0 & vacant_bs ==0 
          , data = tr_da_01_h_rs3_times )
  
 m_i = m$coefficients[c("(Intercept)", "investor", "DA", "investor:DA")]
 
 x = data.frame(cbind(x,m_i))
}







## above is for investor

















### creating tables..

conditions =  (tr_da_01_h_rs3$selfselect2 == 1 | tr_da_01_h_rs3$selfselect2 ==0) &
              tr_da_01_h_rs3$purchase_date_notional >= as.Date("2004-03-01") & 
              tr_da_01_h_rs3$vacantb==0 & tr_da_01_h_rs3$vacant_s==0 & tr_da_01_h_rs3$vacant_bs==0 &
              tr_da_01_h_rs3$prev_sale_price >= 40000 & tr_da_01_h_rs3$prev_sale_price<=5500000 & 
              tr_da_01_h_rs3$contract_price >= 40000 & tr_da_01_h_rs3$contract_price <=5500000
tr_da_01_h_rs3_cond = subset(tr_da_01_h_rs3_times,conditions)

variable = tr_da_01_h_rs3_cond$age_notional_months

tapply(variable, tr_da_01_h_rs3_cond$DA, summary)
tapply(variable, tr_da_01_h_rs3_cond$DA, sd)
summary(variable)
sd(variable, na.rm = TRUE)


variable = tr_da_01_h_rs3_cond$value_index_2016q3

tapply(variable, tr_da_01_h_rs3_cond$DA, summary)
tapply(variable, tr_da_01_h_rs3_cond$DA, sd)
summary(variable)
sd(variable, na.rm = TRUE)


summary(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$prev_sale_price)
sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$prev_sale_price)

mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$prev_sale_price)
sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$prev_sale_price)

#1
summary(tr_da_01_h_rs3[tr_da_01_h_rs3$DA==0 & (tr_da_01_h_rs3$selfselect2 == 1 | tr_da_01_h_rs3$selfselect2 ==0) &
                           tr_da_01_h_rs3$purchase_date_notional >= as.Date("2004-03-01") & 
                           tr_da_01_h_rs3$vacantb==0 & tr_da_01_h_rs3$vacant_s==0 & tr_da_01_h_rs3$vacant_bs==0 &
                           tr_da_01_h_rs3$prev_sale_price >= 40000 & tr_da_01_h_rs3$prev_sale_price<=5500000 & 
                           tr_da_01_h_rs3$contract_price >= 40000 & tr_da_01_h_rs3$contract_price <=5500000 
                    ,]$const_dollar_prev_price_201609)


sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$const_dollar_prev_price_201609)
#2
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$const_dollar_prev_price_201609)
sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$const_dollar_prev_price_201609)
#3
mean(tr_da_01_h_rs3_cond$const_dollar_prev_price_201609)
sd(tr_da_01_h_rs3_cond$const_dollar_prev_price_201609)


#1
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$prev_sale_indexed_pdate_const_doll_2016, na.rm = TRUE)
sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$const_dollar_prev_price_201609, na.rm = TRUE)
#2
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$prev_sale_indexed_pdate_const_doll_2016, na.rm = TRUE)
sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$prev_sale_indexed_pdate_const_doll_2016, na.rm = TRUE)
#3
mean(tr_da_01_h_rs3_cond$prev_sale_indexed_pdate_const_doll_2016, na.rm = TRUE)
sd(tr_da_01_h_rs3_cond$prev_sale_indexed_pdate_const_doll_2016, na.rm = TRUE)



#1
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$purchase_price_notional_const_doll_2016, na.rm = TRUE)
sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$purchase_price_notional_const_doll_2016, na.rm = TRUE)
#2
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$purchase_price_notional_const_doll_2016, na.rm = TRUE)
sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$purchase_price_notional_const_doll_2016, na.rm = TRUE)
#3
mean(tr_da_01_h_rs3_cond$purchase_price_notional_const_doll_2016, na.rm = TRUE)
sd(tr_da_01_h_rs3_cond$purchase_price_notional_const_doll_2016, na.rm = TRUE)




#1
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$const_dollar_cur_price_201609, na.rm = TRUE)
sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$const_dollar_cur_price_201609, na.rm = TRUE)
#2
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$const_dollar_cur_price_201609, na.rm = TRUE)
sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$const_dollar_cur_price_201609, na.rm = TRUE)
#3
mean(tr_da_01_h_rs3_cond$const_dollar_cur_price_201609, na.rm = TRUE)
sd(tr_da_01_h_rs3_cond$const_dollar_cur_price_201609, na.rm = TRUE)



#1
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$const_dollar_cur_price_201609, na.rm = TRUE)
sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$const_dollar_cur_price_201609, na.rm = TRUE)
#2
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$const_dollar_cur_price_201609, na.rm = TRUE)
sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$const_dollar_cur_price_201609, na.rm = TRUE)
#3
mean(tr_da_01_h_rs3_cond$const_dollar_cur_price_201609, na.rm = TRUE)
sd(tr_da_01_h_rs3_cond$const_dollar_cur_price_201609, na.rm = TRUE)





mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$value_index_2016q3)
sd(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$value_index_2016q3)


beds...

mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1 & !is.na(tr_da_01_h_rs3_cond$beds_prev_sale),]$beds_prev_sale)
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0 & !is.na(tr_da_01_h_rs3_cond$beds_prev_sale),]$beds_prev_sale)
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1 & !is.na(tr_da_01_h_rs3_cond$beds_prev_sale),]$beds_cur_sale)
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0 & !is.na(tr_da_01_h_rs3_cond$beds_prev_sale),]$beds_cur_sale)

mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1 & !is.na(tr_da_01_h_rs3_cond$baths_prev_sale),]$baths_prev_sale)
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0 & !is.na(tr_da_01_h_rs3_cond$baths_prev_sale),]$baths_prev_sale)
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1 & !is.na(tr_da_01_h_rs3_cond$baths_curr_sale),]$baths_curr_sale)
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0 & !is.na(tr_da_01_h_rs3_cond$baths_curr_sale),]$baths_curr_sale)

mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1 & !is.na(tr_da_01_h_rs3_cond$cars_prev_sale),]$cars_prev_sale)
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0 & !is.na(tr_da_01_h_rs3_cond$cars_prev_sale),]$cars_prev_sale)
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1 & !is.na(tr_da_01_h_rs3_cond$cars_curr_sale),]$cars_curr_sale)
mean(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0 & !is.na(tr_da_01_h_rs3_cond$cars_curr_sale),]$cars_curr_sale)

length(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1 & !is.na(tr_da_01_h_rs3_cond$cars_prev_sale),]$cars_prev_sale)
length(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0 & !is.na(tr_da_01_h_rs3_cond$cars_prev_sale),]$cars_prev_sale)
length(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1 & !is.na(tr_da_01_h_rs3_cond$cars_curr_sale),]$cars_curr_sale)
length(tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0 & !is.na(tr_da_01_h_rs3_cond$cars_curr_sale),]$cars_curr_sale)



aggregate(variable, by = list(tr_da_01_h_rs3_cond$DA), FUN = mean, na.rm = TRUE)
aggregate(variable , by = list(tr_da_01_h_rs3_cond$DA), FUN = sd, na.rm = TRUE)

aggregate(variable , by = list(!is.na(tr_da_01_h_rs3_cond$DA)), FUN = mean, na.rm = TRUE)
aggregate(variable , by = list(!is.na(tr_da_01_h_rs3_cond$DA)), FUN = sd, na.rm = TRUE)


par(mfrow=c(1,1))
hist((tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$months_bet_sales)%/%12 , right = FALSE, breaks=c(15) , main = "(a) DA - Time between Sales", xlab = "Years", ylab= "Count" )
hist((tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==0,]$months_bet_sales)%/%12 , right = FALSE, breaks=c(15) , main = "(b) Non-DA - Time between Sales", xlab = "Years", ylab= "Count" )
hist((tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$months_to_da)%/%12 , right = FALSE, breaks=c(15) , main = "(c) DA - Time to DA", xlab = "Years", ylab= "Count" )
hist((tr_da_01_h_rs3_cond[tr_da_01_h_rs3_cond$DA==1,]$age_da)%/%12 , right = FALSE, breaks=c(15) , main = "(d) DA - Time to resale post DA", xlab = "Years", ylab= "Count" )



variable = tr_da_01_h_rs3_cond$value_index_2016q3
aggregate(cbind(value_index_2016q3) ~ building_type_compact2 + state , na.rm = TRUE, data = tr_da_01_h_rs3_cond, FUN=sd)
aggregate(tr_da_01_h_rs3_cond$value_index_2016q3 , by = list(tr_da_01_h_rs3_cond$state), FUN = mean, na.rm = TRUE)

## below is for count
cls <- split(tr_da_01_h_rs3_cond$property_id, list(tr_da_01_h_rs3_cond$investor, tr_da_01_h_rs3_cond$investor_da, tr_da_01_h_rs3_cond$DA))
sapply(cls, function(dat) sum(!is.na(dat)))

tr_da_01_h_rs3_cond$year_sale <- year(tr_da_01_h_rs3_cond$contract_date)
cls <- split(tr_da_01_h_rs3_cond$property_id, list(tr_da_01_h_rs3_cond$year_sale, tr_da_01_h_rs3_cond$DA))
sapply(cls, function(dat) sum(!is.na(dat)))

### below is for estimates profile 12 july 2017...


# below function just computes all the coefficients
i=1
rm(x)
x = numeric(8)

for (i in 1:24){
  tr_da_01_h_rs3_times$investor = ifelse(tr_da_01_h_rs3_times$months_bet_sales<=i*12,1,0)
  m <- lm(log((contract_price_duplex_double/purchase_price_notional)) ~ 
           building_type_compact2
          + log(ssd_index_cur_sale2/ssd_index_purchase_notional) 
          + age_notional_months_times + I(age_notional_months_times**2) 
          + I(beds_cur_sale-beds_prev_sale) 
          + I(baths_curr_sale-baths_prev_sale) 
          + I(cars_curr_sale-cars_prev_sale) 
          + state + as.factor(year(purchase_date_notional))
          ,(selfselect2 == 1 | selfselect2 ==0)   &
            prev_sale_price >= 40000 & prev_sale_price<=5500000 & investor==1 &
            contract_price >= 40000 & contract_price <=5500000 & 
            purchase_date_notional >= as.Date("2004-03-01") & 
            vacantb==0 & vacant_s==0 & vacant_bs ==0 
          , data = tr_da_01_h_rs3_times )
  
  m_i = m$coefficients[c("(Intercept)", "building_type_compact2Carports/Garages/Sheds",
                         "building_type_compact2Duplex",
                         "building_type_compact2Extension/Alteration",
                         "building_type_compact2House/Single Dwelling",
                         "building_type_compact2Multiple_DAs",
                         "building_type_compact2Swimming Pool",
                         "building_type_compact2Verandahs/Pergolas"
                         )]
  
  x = data.frame(cbind(x,m_i))
}

