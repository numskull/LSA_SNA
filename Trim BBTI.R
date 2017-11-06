# First, trim BBTI
load("~/Desktop/Parsing the Imprint/bbti.RData")

### ===== These transformations were accidentally saved into bbti.RData === ###
#df = df[which(df$occupation != "Member/Apprentice of Stationers' Co"),]
#df = df[-grep("Tanner",df$occupation),]
#df = df[-grep("Leather",df$occupation),]
#df = df[-grep("See ",df$occupation),]
#df = df[-grep("Paper",df$occupation),]
#df = df[-grep("Auctioneer",df$occupation),]
#df = df[-grep("toolmaker",df$occupation),]
### ============================ ###

df = df[-grep("Parchment",df$occupation),]
df = df[-grep("Scrivener",df$occupation),]
df = df[-grep("Hot-presser",df$occupation),]

# REMOVE INDIVIDUAL TROUBLE CASES
pasted_df = apply(df, 1, paste, collapse = "; ")
names(pasted_df) = NULL

df[which(pasted_df == "Abel; SWALL; 1670; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Thomas; COCKERILL; 1666; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Richard; MARRIOTT; 1639; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Andrew; CLARKE; 1670; Stationer ?"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Nathan; BROOKES; 1650; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "John; WILLIAMS; 1670; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "John; NICHOLSON; 1677; Fellmonger"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Dorothy; JAGGARD; 1627; Printer ?"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Moses; PITT; 1641; Bookseller, Printer"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Thomas; SIMMONS; 1676; Bookseller"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "John; BENNET; 1685; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "William; LANGLETON; 1655; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Bernard; ALSOPP; 1601; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Abel; ROPER; 1679; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Ann Mrs; BALDWIN; 1698; Bookseller, Newsagent/vendor/man/news agent, Publisher"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "R; PARKERS; 1696; Bookseller"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Jacob; TONSON; 1670; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "John; DEANE; 1679; Stationer"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Thomas; MARSHE; 1554; Printer"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Charles; RIVINGTON; 1754; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Walter Thomas; CLARKE; 1790; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Francis; RIVINGTON; 1745; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "W; GRIFFIN; 1748; Publisher"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "James; BEWSEY; 1777; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Mary; FLETCHER; 1758; Stationer (apprentice)"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Thomas; CADELL; 1786; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "J & J; MORPHEW & WOODWARD; 1709; Printer ?"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "MARRABLE &; FLACKTON; 1789; Stationer, Bookseller, Bookbinder, Music seller"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "John; COLLAMBELL; 1758; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Alexander; STRAHAN; 1785; Printer, Publisher"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Joseph; MURRAY; 1788; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "John; PARSONS; 1793; Stationer"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "James; WRIGHT; 1732; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Lawrence; GILLIVER; 1721; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "E & R; NUTT; 1735; Publisher, Printer"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Henry Sampson; WOODFALL; 1739; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Henry; WOODFALL; 1739; Printer, Stationer"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "W; MILLER; 1790; Publisher (music)"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Robert; MANNEY; 1704; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Robert; NANNEY; 1704; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "B & W; WILSON; 1795; Printer"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Richard; WHITEHEAD; 1795; Bookbinder, Printer (copperplate)"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Thomas; FAWCETT; 1621; Printer, Stationer"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Io; ROBERTES; 1585; Stationer"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Mathew; LAW; 1595; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Roger; WARDE; 1566; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Thomas; RICKMAN; 1596; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Edward; GRIFFIN; 1636; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Thomas; SCARLETT; 1577; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Thomas; HACKET; 1557; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Daniel; SPEDE; 1603; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Richard 1; WELLINGTON; 1693; Printer, Stationer, Bookseller"),] = c(NA,NA,NA,NA)
df[which(pasted_df == "Thomas; PASSINGER; 1657; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
# df[which(pasted_df == "Roger; WARDE; 1566; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
# df[which(pasted_df == "Roger; WARDE; 1566; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)
# df[which(pasted_df == "Roger; WARDE; 1566; Member/Apprentice of Stationers' Co"),] = c(NA,NA,NA,NA)






# To deal with apprentices

apps = df[grep("Member/Apprentice of Stationers' Co",df$occupation),]
not_apps = df[-grep("Member/Apprentice of Stationers' Co",df$occupation),]
apps$lName = as.character(apps$lName)
apps$fName = as.character(apps$fName)
appNames = apply(apps[1:3], 1, paste, collapse = " ")
not_apps$lName = as.character(not_apps$lName)
not_apps$fName = as.character(not_apps$fName)
not_appNames = apply(not_apps[1:3], 1, paste, collapse = " ")
df = rbind(not_apps, apps[-which(appNames %in% not_appNames),])


# printers = grep("Printer",df$occupation)
# sellers =  grep("Bookseller",df$occupation)
# b.ord = unique(c(printers,sellers))
# df = df[b.ord,]
df = df[row.names(unique(df[,1:3])),]
rounded = 10 * round(df$date / 10)
df$rounded = rounded
df = df[row.names(unique(df[,c(1:2,5)])),]
rounded = 5 * round(df$date / 5)
df$rounded = rounded
df = df[row.names(unique(df[,c(1:2,5)])),]

df = df[!is.na(df$lName),]
