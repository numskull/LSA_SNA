# First, do some initial cleanup. These commands remove
# brackets and make changes to especially problematic names.
evans = grep("N", tcp$TCP)
imps = tcp_imps[-evans]

stopwords = scan("imstops.txt", what = "character", sep = ",")

# Remove brackets around names
imps = gsub("\\[","",imps)
imps = gsub("\\]","",imps)

# Change 'vv's to ws
imps = gsub("vv","w",imps)

# Correct variations of John, Joseph, and Thomas
imps = gsub("iohn","John",imps)
imps = gsub("Iohn","John",imps)
imps = gsub("Ihon","John",imps)
imps = gsub("iohannes","John",imps)
imps = gsub("johannes","John",imps)
imps = gsub("Johannes","John",imps)
imps = gsub("Johannis","John",imps)
imps = gsub("Iames","James",imps)
imps = gsub("ios.","Joseph",imps)
imps = gsub("Ios.","Joseph",imps)
imps = gsub("Iackson","Jackson",imps)
imps = gsub("Iugge","Jugge",imps)
imps = gsub("Iaggard","Jaggard",imps)
imps = gsub("Iudson","Judson",imps)
imps = gsub("Tho.","Thomas",imps)
imps = gsub("R.L. Chiswell", "R. Chiswell", imps)

# Remove "Paul" when it refers to Paul's Churchyard
imps = gsub("Paul's","",imps)
imps = gsub("St. Pauls","",imps)
imps = gsub("Pauls","",imps)

# Remove "Hall" where it involves Stationers or Westminster Hall
imps = gsub("Stationers-Hall","",imps)
imps = gsub("Stationer's-Hall","",imps)
imps = gsub("Stationers Hall","",imps)
imps = gsub("Stationer's Hall","",imps)
imps = gsub("Westminster-Hall","",imps)
imps = gsub("Westminster Hall","",imps)
imps = gsub("Hall-Street","",imps)
imps = gsub("Hall Street","",imps)
imps = gsub("Hall street","",imps)
imps = gsub("-Hall","",imps)
imps = gsub("West-Minster","",imps)
imps = gsub("Grayes-Inne-gate","",imps)
imps = gsub("Snow-hill","",imps)
imps = gsub("Pide-bull","",imps)

# The stationer "John Bill" creates lots of problems.
# Take out the "John Bill" hits, but mark them to add him back in later.
johnBillHits = grep("John Bill",imps)
imps = gsub("John Bill","",imps)
### NOTE: See also "William Lane" and "John Field" and "Richard Field" ###

# Builds list of character vectors (cleaned up imprints)
imprintList = list()
for (i in 1:length(imps)) {
  print(i)
  
  # Break imprint into words
  text = imps[i]
  text = strsplit(text,"\\W")
  text = unlist(text)
  text = text[text!=""]
  
  # Take out Paul next to Saint or St
  pauls = which(text=="Paul")
  if (length(pauls) > 0) {
    for (j in 1:length(pauls)) {
      if (pauls[j] == 1) {next()}
      if (text[pauls[j] - 1] == "Saint" | text[pauls[j] - 1] == "St") {
        text = text[-pauls[j]]
      }
    }
  }
  
  # Take out all words that appear after "the" "in" and "at"
  thes = which(text=="the")
  nexts = thes + 1
  if (length(thes) > 0) {text = text[-nexts]}
  
  ins = which(text=="in")
  nexts = thes + 1
  if (length(nexts) > 0) {text = text[-nexts]}
  
  ats = which(text=="at")
  nexts = ats + 1
  if (length(ats) > 0) {text = text[-nexts]}
  
  # Remove special stopwords
  text = text[text %in% stopwords == FALSE ]
  
  # Remove all words that begin with a lower-case letter
  firstlets = substring(text, 1, 1)
  lowers = grep("[a-z]", firstlets)
  if (length(lowers) > 0) { text = text[-lowers] }
  
  imprintList[[i]] = text
}

imps = imprintList
save(imps, file = "imps.rda")
