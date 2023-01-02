install.packages('ape')
library('ape')
#1
file = 'C:/Users/pdapr/Downloads/sequence.fasta'
dna = read.dna(file, format = 'fasta')

dna = paste(as.character(dna), collapse = '')
typeof(dna)


#2 6-mer
ss = substring(dna, 1:nchar(dna), 6:nchar(dna))
ss

#3 max and min values
t = sort(table(ss))
t
df = as.data.frame(t)
df
#2 max
tail(df, n=2)

#2 min
df[2:3,]
