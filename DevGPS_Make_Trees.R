library(stringr)

## From DevGPS, after Calc1

# Pull Data for number of employees and Calc1 for RA
Tree <- Data
colnames(Tree) <- c("Region","Code","Emp")
Tree$RA <- Calc1$RA

# Add Region names THIS IS DIFFERENT FOR MSA AND COUNTY - CHECK THIS!!!!
Tree <- Tree %>% left_join(Regions, by = "Region")

# Pull the state abbreviation and transform it into the full name
Tree$StateAbbr <- str_sub(Tree$Name, start = -2)
stateConversion <- function(x, faclevs = 'selected') {
 
    st.codes <- data.frame(state = as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI",
                                            "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN",
                                            "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH",
                                            "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT",
                                            "WA", "WI", "WV", "WY")),
                            full = as.factor(c("Alaska","Alabama" ,  "Arkansas", "Arizona","California" , "Colorado" ,
                                            "Connecticut", "District of Columbia","Delaware" ,  "Florida" , "Georgia" ,
                                            "Hawaii","Iowa" ,"Idaho" , "Illinois" , "Indiana" ,  "Kansas" ,
                                            "Kentucky" , "Louisiana" , "Massachusetts", "Maryland" ,"Maine" ,
                                            "Michigan" , "Minnesota" , "Missouri" ,"Mississippi" ,  "Montana" ,
                                            "North Carolina","North Dakota", "Nebraska" , "New Hampshire" , "New Jersey" ,  "New Mexico" ,
                                            "Nevada" ,"New York" , "Ohio" , "Oklahoma" ,
                                            "Oregon" , "Pennsylvania" , "Puerto Rico", "Rhode Island" , "South Carolina", "South Dakota" ,
                                            "Tennessee" , "Texas" , "Utah" ,  "Virginia","Vermont" ,
                                            "Washington" , "Wisconsin", "West Virginia" , "Wyoming"))
    )
 
    if (nchar(x[1]) == 2) { st.x <- data.frame(state = x); refac.x <- st.codes$full[match(tolower(st.x$state), tolower(st.codes$state))] }
    else { st.x <- data.frame(full = x); refac.x <- st.codes$state[match(tolower(st.x$full), tolower(st.codes$full))] }
 
    if(faclevs == 'all') {return(refac.x)}
    else {return(factor(refac.x))}
 
}
Tree$State <- stateConversion(Tree$StateAbbr)
Tree$StateAbbr <- NULL

# Remove the state name from city name
Tree$Name <- substr(Tree$Name,1,nchar(Tree$Name)-4)

# convert Code to numeric in order to join with NAICS codes descriptions
Tree$Code <- as.numeric(levels(Tree$Code))[Tree$Code] ## Gives NA warning. None created.
sum(is.na(Tree$Code))

# add NAICS code descriptions
Tree <- left_join(Tree, Industries, by = c("Code" = "NAICS"))

# add NAICS code categories
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "11")] <- "Agriculture, Forestry, Fishing and Hunting"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "21")] <- "Mining"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "22")] <- "Utilities"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "23")] <- "Construction"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "31")] <- "Manufacturing"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "32")] <- "Manufacturing"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "33")] <- "Manufacturing"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "42")] <- "Wholesale Trade"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "44")] <- "Retail Trade"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "45")] <- "Retail Trade"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "48")] <- "Transportation and Warehousing"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "49")] <- "Transportation and Warehousing"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "51")] <- "Information"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "52")] <-	"Finance and Insurance"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "53")] <-	"Real Estate Rental and Leasing"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "54")] <-	"Professional, Scientific, and Technical Services"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "55")] <- "Management of Companies and Enterprises"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "56")] <-	"Administrative and Support and Waste Management and Remediation Services"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "61")] <-	"Educational Services"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "62")] <-	"Health Care and Social Assistance"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "71")] <-	"Arts, Entertainment, and Recreation"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "72")] <-	"Accommodation and Food Services"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "81")] <- "Other Services"
Tree$Category[with(Tree, (str_sub(Tree$Code, end = 2)) == "92")] <-	"Public Administration"

## Re-organize to match Morgan treemap format

Tree$Region <- NULL
Tree <- Tree[,c(3,6,1,5,2,7,4)]
colnames(Tree) <- c("market", "category", "code", "description", "emp", "ra", "state")

# Export
write.csv(Tree, "MS_County_Tree.csv", row.names = FALSE)