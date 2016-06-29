#setwd("AEMP/rental_ownership/data") # adjust to your data path as necessary

SfData <- TRUE

if (SfData) {
  OwnershipData <- read.csv("building ownership SF.csv", stringsAsFactors = F)
} else {
  OwnershipData <- read.csv("Oakland_Parcels2014.csv", stringsAsFactors = F)
  names(OwnershipData)[names(OwnershipData)=="OwnersName"] <- "owner_name"
  names(OwnershipData)[names(OwnershipData)=="MailingA_2"] <- "owner_mailing_address"
}

Ownership_noempties <- OwnershipData[OwnershipData$owner_name != "",]

#filter out the properties owned by the gov
if (SfData) {
  gov_names <- c("CITY PROPERTY", "STATE PROPERTY", "UNITED STATES OF AMERICA", 
                 "RECREATION AND PARK DEPARTMENT")
} else {
  gov_names <- c("CITY OF OAKLAND", "HOUSING AUTHORITY OF THE CITY OF OAKLAND", "STATE OF CALIFORNIA",
                 "ALAMEDA COUNTY FLOOD CONTROL", "EAST BAY REGIONAL PARK DISTRICT", "SAN FRANCISCO BAY AREA RAPID TRANSIT DISTRICT",
                 "EAST BAY MUNICIPAL UTILITY DISTRICT", "OAKLAND UNIFIED SCHOOL DISTRICT", "CITY OAKLAND", "COUNTY OF ALAMEDA" )
}

Ownership_noempties <- Ownership_noempties[!(Ownership_noempties$owner_name %in% gov_names),]


## Studying ownership inequality:

# number of owned properties vs unique owners
n_owned_properties <- length((Ownership_noempties$owner_name))
n_unique_owners <- length(unique(Ownership_noempties$owner_name))

owner_names <- Ownership_noempties$owner_name

owner_freq_table <- table(owner_names)
owner_freq_table_sorted <- sort(owner_freq_table, decreasing = T)

# companies/people who own the most properties
head(owner_freq_table_sorted, n = 20)

# percentage of properties owned by the top N owners
top_owner_cnts <- seq(1000, n_unique_owners, by = 1000)
top_owner_cnts_property_pct <- sapply(top_owner_cnts, function(top_owner_cnt) {
  sum(head(owner_freq_table_sorted, n = top_owner_cnt)) / n_owned_properties
})
top_owner_cnts_owner_pct <- sapply(top_owner_cnts, function(top_owner_cnt) {
  top_owner_cnt / n_unique_owners
})

# plot relationship between percent of owners and percent of properties owned
# plot(top_owner_cnts_owner_pct, top_owner_cnts_property_pct)

# SF:
# 1.5% of owners own 23% of properties!! and 10% own about 37.5%


# Oakland:
# Not as stark.
# 7.3% own 24% of properties.


## Studying where owners are located:

# NOTE: number of unique owner names < number of unique owner addresses.
# This is likely due to inconsistency in listed address. We ignore this for
# now and select some address for each unique owner effectively at random.
# May need to be careful with doing that in future analysis.

#deduplication of original data set with no regard for which address is used:
Ownership_uniqueOwners <- Ownership_noempties[!duplicated(Ownership_noempties$owner_name),]

if (SfData) {
  city_str <- "SAN FRANCISCO CA"
} else {
  city_str <- "OAKLAND CA"
}

# quick and dirty stats:
owner_address <- Ownership_uniqueOwners$owner_mailing_address
CA_owners <- grepl(" CA", owner_address)
city_owners <- grepl(city_str, owner_address)

SF_owners <- grepl("SAN FRANCISCO", owner_address)

pct_CA_owners <- sum(CA_owners) / length(owner_address)
pct_city_owners <- sum(city_owners) / length(owner_address)

# SF properties:
# 98% of owners are located in CA
# 87% of owners are located in SF

# OAK properties:
# 98% owners are in CA
# 78% are in OAK


# add a column to indicate whether the owner is from CA or from city
Ownership_noempties$from_CA <- grepl(" CA", Ownership_noempties$owner_mailing_address)
Ownership_noempties$from_city <- grepl(city_str, Ownership_noempties$owner_mailing_address)

pct_props_from_CA <- sum(Ownership_noempties$from_CA) / length(Ownership_noempties$from_CA)
pct_props_from_city <- sum(Ownership_noempties$from_city) / length(Ownership_noempties$from_city)

# OAK: no major discrepancy between % out of town owners and % out of town properties
# 75% vs. 78% for city, 97 vs 98 for state

# SF: similar results
# 82 vs 87 for city, 95 vs 98 for state

# these discrepancies might just be due to random fluctuation or noise.

Ownership_outoftown <- Ownership_noempties[!Ownership_noempties$from_city,]
outoftown_freq_sorted <- sort(table(Ownership_outoftown$owner_name), decreasing = T)

# SF: Top out of town addresses seem to match up with top owners overall.
# OAK: Same trend


# attempt to quantify trend:

top_Ns <- seq(100, length(owner_freq_table_sorted), by = 1000)

rand_owners <- names(sample(owner_freq_table_sorted, 
                      size = length(outoftown_freq_sorted)))


pct_outoftowns <- 
  sapply(top_Ns, function(top_N) {
  top_N_owners <- names(head(owner_freq_table_sorted, n = top_N))
  top_N_outoftown <- names(head(outoftown_freq_sorted, n = top_N))
  
  pct_outoftown_in_topN <- sum(top_N_owners %in% top_N_outoftown) / top_N
  pct_outoftown_in_topN
})


plot(top_Ns, pct_outoftowns)

# SF: of the top 10000 about 20% are out of town compared to only 12.5% out of town owners
# probability of this kind of result occurring by chance:
1 - pbinom((0.2*10000), 10000, 0.12)


#OAK:of top 10k, about 38% are out of town compared to 22% out of town owners
#probability of this occurring by chance:
1 - pbinom((0.38*10000), 10000, 0.22)



# significant difference between ownership count for intown and out of town?
Ownership_intown <- Ownership_noempties[Ownership_noempties$from_city,]
intown_freq_sorted <- sort(table(Ownership_intown$owner_name), decreasing = T)


t.test(outoftown_freq_sorted, intown_freq_sorted)
# not really lol


# Create a new table for owner frequency
owner_frequency <- as.data.frame(owner_freq_table,stringsAsFactors = F)
names(owner_frequency)[names(owner_frequency)=="owner_names"] <- "owner_name"
names(owner_frequency)[names(owner_frequency)=="Freq"] <- "owner_num_properties_in_city"

# merge with no empties table
Ownership_merged <- merge(Ownership_noempties, owner_frequency, by = "owner_name")

if (!SfData) {
  names(Ownership_merged)[names(Ownership_merged)=="owner_name"] <- "OwnersName"
  names(Ownership_merged)[names(Ownership_merged)=="owner_mailing_address"]  <- "MailingA_2"
}


output_name <- if (SfData) "Ownership_SF_extra_owner_info.csv" else "Ownership_Oakland_extra_owner_info.csv"
frequency_output_name <- if (SfData) "SfOwnerFrequency.csv" else "OaklandOwnerFrequency.csv"

write.csv(Ownership_merged, file = output_name, row.names=FALSE)
write.csv(owner_frequency, file = frequency_output_name, row.names = FALSE)

# Next steps
# How far away are the owner addresses?