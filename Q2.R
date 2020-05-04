library(tidyverse)
library(stats)
library(expss)
## Question 1
clean_bene <- PartD17$bene_count[!is.na(PartD17$bene_count)]
avg_bene <- mean(clean_bene)
avg_bene

##Question 2: estimate length of averaqge scrip per provider, then find median of distribution
# clean_claim_count <- PartD17$total_claim_count[!is.na(PartD17$total_claim_count)]
# clean_total_days <- PartD17$total_day_supply[!is.na(PartD17$total_day_supply)]
# q2 <- as.data.frame(clean_claim_count,clean_total_days)
PartD17$avg_script <- PartD17$total_day_supply/PartD17$total_claim_count
avg_claim_median <- median(PartD17$avg_script[!is.na(PartD17$avg_script)])
avg_claim_median
## Question 3

PartD17$specialty_description <- as.character(PartD17$specialty_description)


# spec_sum <- PartD17 %>% 
#   group_by(specialty_description) %>% 
#   summarize()

specs <- PartD17 %>% 
  select(specialty_description, total_claim_count, brand_claim_count)
specs_counts <- specs %>% 
  group_by(specialty_description) %>% 
  summarise(
    total = sum(total_claim_count, na.rm = TRUE),
    brand = sum(brand_claim_count, na.rm = TRUE)
    
  )

specs_counts_trimmed <- specs_counts[specs_counts$total > 1000,]
specs_counts_trimmed <- specs_counts_trimmed %>% 
  mutate(prop = brand/total)

sd(specs_counts_trimmed$prop)


##Question 4
beneficiaries <- PartD17 %>% 
  select(nppes_provider_state,nppes_provider_country, opioid_bene_count, antibiotic_bene_count)


bene_by_state <- beneficiaries %>%
  filter(nppes_provider_country == "US") %>%
  filter(nppes_provider_state == state.abb) %>% 
  group_by(nppes_provider_state) %>% 
  summarise(
    opioids = sum(opioid_bene_count, na.rm = TRUE),
    antibiotics = sum(antibiotic_bene_count, na.rm = TRUE)
  ) %>% 
  mutate(ratio = opioids/antibiotics)
ratio_diff = max(bene_by_state$ratio) - min(bene_by_state$ratio)
ratio_diff

##Question 5 
q5 <- PartD17 %>% 
  select(nppes_provider_state, specialty_description, opioid_day_supply, opioid_claim_count) %>% 
  mutate(avg_opioid_script = opioid_day_supply/opioid_claim_count)
q5 <- q5[!is.na(q5$avg_opioid_script),] %>% 
  filter(nppes_provider_state == state.abb)
#q5table <- table(q5$nppes_provider_state, q5$specialty_description)
q5df <- as.data.frame(table(q5$nppes_provider_state, q5$specialty_description))
q5df_100 <- q5df[q5df$Freq >= 100,]

q5$nppes_provider_state <- as.character(q5$nppes_provider_state)
q5$specialty_description <- as.character((q5$specialty_description))
q5trim1 <- q5 %>% 
  filter(str_detect(nppes_provider_state, "CA") & str_detect(specialty_description, "Family Practice")) 
q5trim2 <- q5 %>% 
  filter(str_detect(nppes_provider_state, "CA") & str_detect(specialty_description, "Internal Medicine")) 
q5trim3 <- q5 %>%   
  filter(str_detect(nppes_provider_state, "FL") & str_detect(specialty_description, "Internal Medicine")) 
q5trim4 <- q5 %>%   
  filter(str_detect(nppes_provider_state, "NY") & str_detect(specialty_description, "Internal Medicine")) 
q5trim5 <- q5 %>%   
  filter(str_detect(nppes_provider_state, "TX") & str_detect(specialty_description, "Family Practice"))
trims <- c(q5trim1, q5trim2, q5trim3, q5trim4, q5trim5)

q5trim <- as.data.frame(matrix(trims))

##question 6
q6 <- PartD17 %>% 
  select(total_claim_count,total_claim_count_ge65, lis_claim_count) %>% 
  mutate(six_five_up = total_claim_count_ge65/total_claim_count) %>% 
  mutate(lis = lis_claim_count/total_claim_count)
q6dropped <- q6[!is.na(q6$six_five_up) & !is.na(q6$lis),]

cor(q6dropped$six_five_up,q6dropped$lis)

##Question 7
q7_2017 <- PartD17 %>% 
  select(npi,total_drug_cost,total_day_supply) %>% 
  mutate(avg_price17 = total_drug_cost/total_day_supply)
q7_2016 <- PartD16 %>% 
  select(npi, total_drug_cost, total_day_supply) %>% 
  mutate(avg_price16 = total_drug_cost/total_day_supply)
q7both <- left_join(q7_2016,q7_2017, by = "npi") %>% 
  mutate(Inflation = ((avg_price17 - avg_price16)/avg_price17))
mean(q7both$Inflation[!is.na(q7both$Inflation)& !is.infinite(q7both$Inflation)])
# length(q7both$Inflation[!is.na(q7both$Inflation)])
# sum(q7both$Inflation[!is.na(q7both$Inflation)])
# rou