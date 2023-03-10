# Overview
[The U.S. Presidential Speech Ngram Viewer](https://sean-bock.shinyapps.io/Presidential_Ngram_viewer/?_ga=2.53193256.1007849256.1676680747-849889320.1676680747) displays frequencies of n-grams (unigrams, bigrams, and trigrams are possible) from U.S. Presidential Campaign speeches given by major party candidates between 1952 and 2020. Simply type an n-gram in the search box, and possible selections will appear. Once an n-gram is selected, an accompanying figure will display below. 

# Which words are included?
The data have been limited to n-grams that are not made up entirely of stopwords (i.e., common words such as 'the', 'in', 'but', etc.).
The frequencies display a given n-gram's usage as a percent of all corresponding n-grams in a given year, either overall or by a given candidate. For example, an overall frequency of .05% in the year 2016 for the unigram 'country' indicates that .05% of all unigrams used in 2016 campaign speeches were 'country'. As another example: if the frequency of the bigram 'the people' is .05% for the Democrat in 2008 (i.e., Barack Obama), that would indicate that .05% of all bigrams used by Barack Obama in 2008 were 'the people'. 

# Graph options
There are several options for viewing frequencies of n-grams. The 'Trends by party' option displays ngram trends disaggregated by party (unchecked displays overall frequencies in each election). If multiple n-grams are entered, the 'Separate by n-gram' option will display each n-gram in a separate panel. Likewise, if 'Trends by party' is selected, the 'Separate by party' will display the results of each party in a separate panel. Finally, users may control the year range in which trends are plotted by adjusting the year slider.
