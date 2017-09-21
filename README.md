<a href="https://rvaz.shinyapps.io/english_predictor/" target="_blank">
<b>CLICK HERE</b></a> to try it online

### Natural Language Processing - English Word Predictor

### Content:  

* The R script with the word predicting algorithm which makes suggestions for 
the word being typed and for the next word.   

* The files required by the algorithm  with codified information of over 1 million ngrams.  

* The shinyUI and shinyServer scripts to run the predictor in a Shiny app.  

### Instructions

* To load and try the program in R, source the 
[predict_both.R](predict_both.R) script with all the .csv files.
The function predict.both( ), with a word or phrase as an argument, is what will
produce the suggestions for both, the current word and the following word. 

* To see the predictor at work, visit the app 
<a href="https://rvaz.shinyapps.io/english_predictor/" target="_blank">here</a>.

* To learn more about the app, the algorithms, or the corpus, visit the 
information section of the app.

* For a Spanish version of the predictor, check the
<a href="https://rvaz.shinyapps.io/es_predictor/" target="_blank">Spanish app</a>.

* Or for a bilingual Spanish-English version with language auto-detect, check the 
<a href="https://rvaz.shinyapps.io/en_es_predictor/" target="_blank">EN-ES app</a>.

* See this on [GitHub Pages](https://reyvaz.github.io/NLP-English-Predictor/).  

### More Information

The shiny app takes text as an input and automatically,  

1. Makes suggestions to complete the word being typed.  
2. Makes suggestions for the next word.  

The predictor was built from the ground up using the following, 

1. Three English corpora from SwiftKey containing publicly available news, 
personal blogs, and twitter posts. More information [here](https://web-beta.archive.org/web/20160930083655/http://www.corpora.heliohost.org/aboutcorpus.html). Or [download here](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)  
2. A partial English Wikipedia dump containing updated articles. Downloaded on 
Aug 18, 2017. [Download here](https://dumps.wikimedia.org/enwiki/20170820/enwiki-20170820pages-articles1.xml-p10p30302.bz2).  
3. A collection of recent, popular, and trending topics Twitter posts from Aug 28-30, 2017. Downloaded using the `rtweet` package. No link available, please contact. 
4. The entire May 9, 2017 Memory Alpha Encyclopedia dump. Downloaded on Aug 28, 2017 and available [here](http://memory-alpha.wikia.com/wiki/Memory_Alpha:Database_download).

The corpora were then preprocessed using Python and R. Preprocessing was 
intentionally kept minimal to capture language as it is typed, including 
grammar, slang, contractions, and misspellings. In this way, the program that 
created the predictor files, was allowed to determine the most likely 
correct spelling of words to provide them as suggestions, without discarding the 
information of what the writer meant. Because of this, the predictor is able to 
better understand what the writer means, and also provide more grammatically 
sound suggestions. 

The final corpus used to build the predictor contains ~165.5 million words 
in a ~1 GiB uncompressed file.

Codification of words and phrases, as well as redundancy checks, allows the 
program to store information of over 1 million ngram-predictions lists (and the 
algorithms) in under 24 MiB of uncompressed files.
```r
workspace.size()
```
```
[1] "23.34 MiB"
```
And also make predictions in few milliseconds in a small, personal system. 
```r
measure.prediction.time("President of the")
```
```
[1] "United"   "Republic" "National" "American" "USA"     
Milliseconds elapsed    = 2.0 
Predictions per second = 500 
```
Because of the vast amount of information, the program can be safely scaled-down
without significantly sacrificing accuracy. Whereas the limited footprint 
allows for smooth execution, and easy scale-up without being heavy on resources.

<br>
<p align="center">
<a href="https://reyvaz.github.io/NLP-English-Predictor/" 
rel="see html report">
<img src="eng_cloud4.png" alt="Drawing" width = "500"></a>
</p>
<br>