# Search for matches between BBTI and Parsed Imprints
# Begin by loading the bbti.RData

# load("~/Desktop/Parsing the Imprint/estc_dates.rda")
# load("~/Desktop/Parsing the Imprint/estc_imps.rda")

# source("imprint cleanup script.R")
# source("Trim BBTI.R")
people = tcpPersons[12376:15582,]

range = 1:length(imps)
#source("parse script.R")
date = c()
for(i in 1:length(people$ID)){
  date[i] = substr(people$ID[i], nchar(as.character(people$ID[i]))-3,nchar(as.character(people$ID[i])))
}

fName = c()
lName = c()
lName = gsub(",.*$", "", people$ID)
fName = gsub(".*,\\s", "", people$ID)
fName = gsub(" .*$", "", fName)

df = cbind(fName, lName, date)

### ===== How estc dates were created, NAs removed ===== ####
# Prep dates vector for dates of each estc file
dates = as.character(estc$pub_year)
dates = as.integer(dates)
# Will generate some NA values. With date range, just default to early date
nullDates = estc$pub_year[which(is.na(dates) == T)]
nullDates = gsub("u", "0", nullDates)
nullDates = gsub("-", "0", nullDates)
nullDates = substring(nullDates, 1, 4)
nullDates = as.integer(nullDates)
dates[which(is.na(dates) == T)] = nullDates
### ============================================= ###

df = as.data.frame(df)
df$date = as.numeric(as.character(df$dates))


matchesList = list()

multiples = c()
for (i in range) {
  print(i)
  matches = c()

  # Limit BBTI to plausible date range
  rightTimes = which(df$date < dates[i] + 2 & df$date > dates[i] - 60)
  df.r = df[rightTimes,]

  # Select the imprint
  imp = imps[[i]]
  if (length(imp) == 0) {next()}

  # Don't select initials
  names = which(nchar(imp) > 1)
  if (length(names) == 0) {next()}

  # The 'names' object is an integer vector that points to words longer than 1 character in 'imp'
  for (j in 1:length(names)) {

    # If names[j] is the first word in 'imp', skip it. (B/c it's a first name.)
    if (names[j] == 1) {next()}

    # First, search for matching last name
    possibles = grep(toupper(imp[names[j]]),df.r$lName)
#     if (length(possibles) == 1) {
#       # If you find only one, do nothing and move on.
#     }
#
    # If you find none, broaden to "agrep" search
    if (length(possibles) == 0) {
      possibles = agrep(toupper(imp[names[j]]),df.r$lName)
      # But restrict results to possibles that share the same first letter.
      possibles = possibles[substring(df.r$lName[possibles],1,1) == substring(imp[names[j]],1,1)]
    }

    # If you find none at this point, you don't have a name. Move on.
    if (length(possibles) == 0) {
      next()
    }

    # If you find exactly one, grab it and move on
#     if (length(possibles) == 1) {
#       matches = c(matches,possibles)
#       next()
#     }

    # If you have more than one possibility, time to search by first name (the first word in 'imps' doesn't run this test)
    if (names[j] > 1 & length(possibles) > 0) {
      prior = imp[names[j]-1]
      firstInitial = substring(prior,1,1)
      possiblesInitials = substring(df.r$fName[possibles],1,1)
      firstNamePass = grep(firstInitial,possiblesInitials)
      }

    # If you find exactly one first name match among the possibles, grab it and move on
    if (length(firstNamePass) == 1) {
      matches = c(matches,possibles[firstNamePass])
      next()
    }

    if (length(firstNamePass) > 1) { # If you find more than one first name with the same first initials
        possibles = possibles[firstNamePass] # Restrict possibles down.
        if (nchar(prior) == 1) { # Are you just working from an initial? If so, just grab the one with the closest date.
          closestByDate = which(dates[i] - df.r$date[possibles] == min(dates[i] - df.r$date[possibles]))
          matches = c(matches,possibles[closestByDate])
          next()
        }
        if (nchar(prior) > 1) { # If it's a whole name, find the closest one by Levenstein distance
          closestByName = which(adist(prior,df.r$fName[possibles]) == min(adist(prior,df.r$fName[possibles]),na.rm=T))
          if (length(closestByName) == 1) {
            matches = c(matches,possibles[closestByName])
            next()
          } else { # If more than one name has the same edit distance, grab the one with the closest date
            possibles = possibles[closestByName]
            closestByNamesAndDate = which(dates[i] - df.r$date[possibles] == min(dates[i] - df.r$date[possibles]))
            matches = c(matches,possibles[closestByNamesAndDate])
            if (length(closestByNamesAndDate) > 1) multiples = c(multiples,i)
          }
        }
      }
    matchesList[[i]] = row.names(df.r)[matches]
  }
}

save(matchesList,file="matches.RData")

namesList = list()
for (i in range) {
  print(i)
  peeps = matchesList[[i]]
  names = c()
  for (j in 1:length(peeps)) {
    if (j > 0) {
      row = which(rownames(people) == peeps[j])
      namej = paste(people[row,], collapse="; ")
    }
    names = c(names,namej)
    names = names[which(names != "character(0); character(0); numeric(0); character(0)")]
  }
  namesList[[i]] = names
}
