## kNN and Decision Tree Classification on News Articles in R

1. Download the news articles dataset named “20news-18828.tar.gz”, from an online textual dataset repository: http://qwone.com/%7Ejason/20Newsgroups/. The 20 Newsgroups data set is a collection of approximately 20,000 newsgroup articles, partitioned (nearly) evenly across 20 different newsgroups.

2. Convert them to a term frequency (TF) matrix. (each row is an article, each column is a unique term, and each entry of this TF matrix is term frequency).

3. Run kNN and Decision Tree with 5-folder cross validation. (label each new article with the category).

4. Compare the overall accuracies and f-measures of kNN and decision tress before feature selection and after feature selection. Can feature selection help improve the accuracies of classifiers and why?

5. Evaluate how K impacts the overall accuracy and f-measure of kNN on the dataset. Use histogram plots to visualize the results and identify the best K.

6. Compare the overall accuracies and f-measures of kNN with the best K and decision trees using histograms. Which classifier is better and why?

7. Compare the accuracies of each article category of kNN with the best K and decision trees using histograms. For a particular article category, which classifier is better?
