# hw4 smolkina

#Скачайте геном E.coli в формате fasta (тут -
#https://www.ncbi.nlm.nih.gov/nuccore/NC_011750.1?report=fasta), прочитайте его
#методом read.dna из пакета ape. Получите последовательность генома в виде
#строки (строковой вектор единичной длины). Найдите два самых частых и
#самых редких 6-ти мера в геноме бактерии. Постарайтесь опитимизировать код
#так, чтобы он работала за адекватное время.

#install.packages("ape")
library(ape)
#library(tidyverse)
library(stringi)
library(stringr)

getwd()
setwd('C:/Users/j.smolkina/OneDrive - CRITEO/Desktop/HSE/R/class4')
getwd()

file = "ecoli.fasta"
dna = read.dna(file, format="fasta")
# we have only one sequence
dna = dna[1,]
# as string
dna = paste(as.character(dna), collapse='')
# G C T A
v_a = 'aaaaaa'
v_c = 'cccccc'
v_g = 'gggggg'
v_t = 'tttttt'

v_a_count = 0
v_c_count = 0
v_g_count = 0
v_t_count = 0

new_dna = strsplit(dna,NULL)
length(new_dna)
dna_split = unlist(new_dna, recursive = TRUE, use.names = TRUE)
length(dna_split) #5132068

sub_dna_a = gsub(v_a, 'A', dna, ignore.case = FALSE, perl = FALSE,
        fixed = TRUE, useBytes = FALSE)
sub_dna_a_s = strsplit(sub_dna_a,NULL)
sub_dna_a_split = unlist(sub_dna_a_s, recursive = TRUE, use.names = TRUE)
length(sub_dna_a_split) #5117648

sub_dna_c = gsub(v_c, 'C', dna, ignore.case = FALSE, perl = FALSE,
                 fixed = TRUE, useBytes = FALSE)
sub_dna_c_s = strsplit(sub_dna_c,NULL)
sub_dna_c_split = unlist(sub_dna_c_s, recursive = TRUE, use.names = TRUE)
length(sub_dna_c_split) #5130728

sub_dna_g = gsub(v_g, 'G', dna, ignore.case = FALSE, perl = FALSE,
                 fixed = TRUE, useBytes = FALSE)
sub_dna_g_s = strsplit(sub_dna_g,NULL)
sub_dna_g_split = unlist(sub_dna_g_s, recursive = TRUE, use.names = TRUE)
length(sub_dna_g_split) #5131088

sub_dna_t = gsub(v_t, 'T', dna, ignore.case = FALSE, perl = FALSE,
                 fixed = TRUE, useBytes = FALSE)
sub_dna_t_s = strsplit(sub_dna_t,NULL)
sub_dna_t_split = unlist(sub_dna_t_s, recursive = TRUE, use.names = TRUE)
length(sub_dna_t_split) #5117783


v_a_count = (length(dna_split)-length(sub_dna_a_split))/5
v_c_count = (length(dna_split)-length(sub_dna_c_split))/5
v_g_count = (length(dna_split)-length(sub_dna_g_split))/5
v_t_count = (length(dna_split)-length(sub_dna_t_split))/5
v_a_count #2884
v_c_count #268
v_g_count #196
v_t_count #2857


minimum = min(v_t_count,v_g_count,v_c_count,v_a_count)
minimum #v_g

maximum= max(v_t_count,v_g_count,v_c_count,v_a_count)
maximum #v_a

#v_a_count = grepl(v_a, dna, fixed = TRUE)
#v_c_count = grepl(v_c, dna, fixed = TRUE)
#v_g_count = grepl(v_g, dna, fixed = TRUE)
#v_t_count = grepl(v_t, dna, fixed = TRUE)

#v_a_count = str_count(dna,v_a)
#v_c_count = str_count(dna,v_c)
#v_g_count = str_count(dna,v_g)
#v_t_count = str_count(dna,v_t)



