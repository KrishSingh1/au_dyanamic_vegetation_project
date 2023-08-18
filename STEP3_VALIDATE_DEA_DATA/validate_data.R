original_method <- read.csv("/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/csv_files/SASMDD0002.csv")
new_method <- read.csv("/Users/krish/Desktop/DYNAMIC MODEL VEGETATION PROJECT/DataExtraction/BACKUP_DATA/new_script_test_c/SASMDD0002.csv")


## Test 0: Check for consistent x-y coord ranges between the two datasets

# TRUE = methods exhibit consistent spatial extent 
# FALSE = methods exhibit inconsistent spatial extent 

if (all(range(new_method$x) == range(original_method$x)) & 
    all(range(new_method$y) == range(original_method$y))) {
  print(T)
} else {
  print(F)
}

## Results for Test 0:
# WAANUL0007: TRUE
# NTAFIN0030: TRUE
# QDAMUL0002: TRUE
# SASMDD0016: TRUE 
# SASMDD0002: TRUE

## Test 1: Check if the intersect is equal 

# TRUE = The methods produced identical results for the common time slices
# FALSE = The methods did not produce identical results for the common time slices 

common.timeslices <- intersect(unique(original_method$time),(unique(new_method$time)))

new.method.common <- subset(new_method,subset = (time %in% common.timeslices))
origin.method.common <- subset(original_method, subset = (time %in% common.timeslices))

for(col in colnames(new.method.common)){
  print(all(new.method.common[[col]] == origin.method.common[[col]],na.rm = T))
}

## Test 1 Results:
# WAANUL0007: TRUE
# NTAFIN0030: TRUE
# QDAMUL0002: TRUE
# SASMDD0016: TRUE 
# SASMDD0002: TRUE


## Test 2: Check if the set difference is NA 

# TRUE = The uncommon time silces does not have missing data across the methods 
# FALSE =  The uncommon time silces did have missing data across the methods

diff.timeslices <- setdiff(unique(original_method$time),(unique(new_method$time)))
origin.method.diff <- subset(original_method, subset = (time %in% diff.timeslices))
new.method.diff <- subset(new_method, subset = (time %in% diff.timeslices))


check.na <- function(df){
  all.na <- T
  len <- nrow(df)
  
  if(len > 0){
    
    for(col in c('bs','pv','npv','ue')){
      na.count <- sum(is.na(df[[col]]))
      if(na.count != len){
        all.na <- F
      }
      
    }
    
  }
  return(all.na)
}

check.na(new.method.diff) & check.na(origin.method.diff)

## Test 2 Results:
# WAANUL0007: TRUE
# NTAFIN0030: TRUE
# QDAMUL0002: TRUE 
# SASMDD0016: TRUE
# SASMDD0002: TRUE

## CONCLUSION:
# The methods does not pose inconsistencies with the data it retrieves. 


## Others (not recorded):

## Check for duplication between the two datasets

original_method[duplicated(original_method),]
new_method[duplicated(new_method),]


