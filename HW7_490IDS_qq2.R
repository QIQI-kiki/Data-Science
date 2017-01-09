#Q. 1) (3 points)
#Write down a general regular expression to match the following:
#(a) Words with punctuation in them, e.g., h@te or v|c0din
x=c('h@te','v|c0din')
grep(pattern='[[:punct:]]',x)

#(b) An IP address (Four sets of 1 to 3 digits separated by periods, e.g., 100.12.162.0)
x=c('100.12.162.0','124.32.8.120')
grep(pattern='^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$',x)

#(c) An email address that ends with .com, .edu, .net, .org, or .gov
x=c('qq2@illinois.edu','qq2@gmail.com')
grep(pattern='^.*?\\@.*?\\..*?$',x)

#Q. 2) (19 points) Carry out the following exercises on the State of the Union 
#speeches database (available in moodle).
#(a) Use readLines() to read in the speeches where the return value is: character vector
#with one element/character string per line in the file
filetext=readLines('C:\\Users\\QIQI\\Desktop\\stateoftheunion1790-2012.txt')

#(b) Use regular expressions to find ***
grep(pattern='\\*\\*\\*',filetext)

#(c) Use *** to identify the date of the speech.
breaktext=grep(pattern='\\*\\*\\*',filetext)
Dates=as.character(filetext[breaktext+4])
Dates=Dates[grep('[[:digit:]]{4}',Dates)]
head(Dates)

#(d) Use regular expressions to extract the year.
YearPosition=as.numeric((gregexpr(pattern="[[:digit:]]{4}", text=Dates)))
Year=as.numeric(substr(x=Dates, start=YearPosition, stop=YearPosition + 4))
head(Year)

#(e) Use regular expressions to extract the month.
Month=gsub(pattern=paste0(" ", "[[:digit:]]+", ", ", "[[:digit:]]+"), replacement="", x=Dates)
head(Month)

#(f) Use *** to extract the name of the president State of the union speeches.
president=filetext[breaktext+3]
president=president[grep('[[:alpha:]]',president)]
head(president)

#(g) Use regular expressions and R to return the number of speeches in
#the dataset, and the number of presidents that gave speeches.
breaktext=grep(pattern='\\*\\*\\*',filetext)
Dates=as.character(filetext[breaktext+4])
Dates=Dates[grep('[[:digit:]]{4}',Dates)]
num_speech=length(Dates)
president=filetext[breaktext+3]
president=president[grep('[[:alpha:]]',president)]
num_president=length(unique(president))
# The number of speeches in the dataset is 222, the number of presidents is 41.

#(h) Chop the speeches up into a list there is one element for each speech.
#Each element is a character vector. Check: does your number of list
#elements match your answer above?
Speech=list()
for (i in 1:(length(breaktext)-1)){
  Speech[[i]]=paste(filetext[(breaktext[i]+6):breaktext[i+1]-1], sep=" ", collapse="")
}
length(Speech)
#The number of list element is 222, which matches the answer above.

#(i) Eliminate apostrophes, numbers, and the phrase: (Applause.)
for(i in 1:length(Speech))
{
  Speech[[i]]=gsub(pattern="\\'", replacement="", x=Speech[[i]])
  Speech[[i]]=gsub(pattern="[[:digit:]]", replacement="", x=Speech[[i]])
  Speech[[i]]=gsub(pattern="(Applause.)", replacement="", x=Speech[[i]])
}

#(j) Make all the characters lower case.
Speech=tolower(Speech)

#(k) Split the sentences up where there are blanks and punctuation to create ¡°words¡±.
Speech1=strsplit(x=Speech, split="([[:blank:]]|[[:punct:]])")

#(l) Drop any empty words that resulted from this split.
Speech2=lapply(Speech1, function(x) x=x[x != ""])

#(m) Create a word vector for each speech.
word=Speech2
word1=word[[1]]
#this is the word vector for the first speech. we can obtain every speech by [[]].

#(n) Normalize the word vectors to get term frequencies.
wordcount=list()
wordcount=lapply(word,function(x) table(x))
wordfreq=wordcount
for(i in length(wordcount)){
  sum=0
  for(j in length(wordcount[[i]])){
    sum=wordcount[[i]][j]+sum
  }
  for(j in 1:length(wordcount[[i]])){
    wordfreq[[i]][j]=wordcount[[i]][j]/sum
  }
}

#(o) (5 points) Carry out some exploratory analysis of the data and term
#frequencies. For example, find the number of sentences, extract the
#long words, and the political party. Plot and interpret the term
#frequencies. What are your observations?
freetimes=rep(0,length(wordfreq))
for(i in 1:length(wordfreq)){
  freetimes[i]=wordfreq[[i]]['free']
}
plot(freetimes~Year,main='the frequency of word free')

#From the plot, we can see that "free" this word almost occurred for every president's speech.
#free is always an important topic for society.
  
  







