# Text Mining with R

### Copom, FOMC and International Relations scripts

This repository contains a few text mining projects using tidytext in R. 

While the focus and original intent was to quantify Central Bank comunication sentiment, I found these newly discovered tools 
to be very useful in another application: Discourse Analysis in International Relations. It is not my field of work or research, 
but I turned myself to it to contribute with a Vida (2025) paper. 

All scripts were made with help from AI to initially structure the code, specifically Gemini 3 Pro and Claude Sonnet 4.5. 
All qualitative stuff (like personalized lexicon, categories and weights) were not. 

I hope these scripts can be useful as a starting point for other projects :) 



### About the Copom scripts: 
_they are "download_atas.r" and "copom-txtmining.r"_

First, run the script to download the minutes. it will create an "atas" folder in your directory in which will be saved minutes ranging from a customizable period. 
We use webscrapping to retrieve a customizable amount of minutes. I didn't test going back from the 200th meeting: the layout used by the Central Bank was deeply modified after that one, so the URL could also be different - in which case, my current way of extraction would not work.    

Then, the other script uses tidytext to read, tokenize and evaluate the minutes' stance on monetary policy using a Loughram lexicon mixed with a personalized one. This approach is heavily based in the BIS paper by Chong and Ho (2022) and especially the 2017 book by Julie Silge. 


### About the FOMC scripts:
_they don't exist yet!_


### About the Discourse Analysis scripts
_it's the "analise_pg_eu.r" one_

It's important to note right away that this is very situational. In fact, I didn't do any fancy webscraping to automate the extraction of the texts because there's only two of them, and both are in the repository. They are the Political Guidelines written to inaugurate Von der Leyen's 2019 and 2024 mandates in the European Comission. Vida (2025) believes that the political environment in Europe became much more hostile amid tensions with Russia and a growing far-right, and the script seeks to test that by quantifying a qualitative interpretation of the discourse embedded in each document. 


