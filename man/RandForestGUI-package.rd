\docType{package}
\name{RandForestGUI}
\alias{RandForestGUI}
\alias{RandforestGUI}
\alias{randforestGUI}
\alias{randForestGUI}
\alias{Randforestgui}
\alias{randforestgui}
\alias{randForestgui}
\alias{RandforestGui}
\alias{randforestGui}
\alias{randForestGui}
\alias{RF}
\alias{rf}
\alias{Rf}

\title{Graphical user interface of RandForestGUI package, a program to analyze microbial profile (taxonomic sequences, fingerprints and microarrays) based on conditional inference random forest}
\description{A Tcl/Tk GUI to use functions of the \code{RandForestGUI} package.}

\section{Menu bar}{
The menu bar at the top of the window is used to launch all the functions of the package.\cr

The \code{Management} menu is used to import microbial profile and their corresponding categorical variables (ordinal and nominal), save and load a \code{RandForestGUI} project, and select microbial profiles :

- \code{Import microbial profiles} is used to import microbial profiles table present either in an external ASCII file (.txt, .csv etc) or in R environment as an object.

- \code{Import categorical variables} is used to import categorical variables corresponding to the microbial profiles. These categorical variables can be present either in an external ASCII file (.txt, .csv etc) or in R environment as an object.

- \code{Save} is used to save or save as a \code{RandForestGUI} project in a specific directory.

- \code{Load} is used to import a \code{RandForestGUI} project previously saved.

- \code{Selection of microbial profiles} is used to select specific microbial profiles to perform random forest.


The \code{Unsupervised random forest} menu is used to perform conditional inference unsupervised random forest and determine clusters with K-means and PAM (Partitioning Around Medoids):

- \code{Choose unsupervised mode} permits to choose between the Addcl1 sampling method (synthetic data are added by randomly sampling from the product of empirical marginal distributions of the variables) and Addcl2 sampling method (synthetic data are added by randomly sampling from the hyperrectangle that contains the observed data, that is, the variables of synthetic observations have a uniform distribution with range determined by the minimum and maximum of the corresponding observed variable).

- \code{Determine the best mtry} permits to determine the best mtry for the unsupervised random forest analysis on your data set. The accuracy of random forest depends of the mtry value.

- \code{Calculate and plot unsupervised random forest} is a generic function to calculate and/or to plot unsupervised random forest.

- \code{Determine microbial cluster using K-means or PAM and Davies Bouldin index} determines microbial clusters with the k-means or PAM procedures and the best number of microbial clusters using Davies Bouldin index.\cr


The \code{Supervised random forest} menu is used to perform conditional inference supervised random forest and calculate taxonomic units importances:

- \code{Determine the best mtry} permits to determine the best mtry for the supervised random forest analysis on your data set. The accuracy of random forest depends of the mtry value.

- \code{Calculate and plot unsupervised random forest} is a generic function to calculate and/or to plot supervised random forest.

- \code{taxonomic unit importance} calculates the mean decrease accuracy of taxonomic units.

- \code{Significance of taxonomic units importance} calculates p values of taxonomic units mean decrease accuracy using Monte Carlo permutations. 

- \code{Backward taxonomic units selection} performed a backward selection method by iteratively deleting the less important taxonomic units according to their mean decrease in accuracy and at each step calculating a new random forest. This calculation is needed to choice the top sufficient taxonomic units of the next menu 'Supervised random forest with probes selection'"


The \code{Supervised random forest with probes selection} menu is used to perform conditional inference supervised random forest with the top sufficient taxonomic units:

- \code{Top sufficient taxonomic units} determines a small set of taxonomic units sufficient to discriminate microbial clusters. It leads to identify taxonomic units specifics to each cluster. The random forest model with the lowest number of taxonomic units and the lowest out of bag was retained. To comfort the robustness of this approach, two subsequent analyses were performed. A Pearson correlation was calculated between the initial random forest distances (all taxonomic units) and the new random forest distances and a filtered method using with the Kruskal Wallis test performed for each selected taxonomic unit. The choice of the number of taxonomic units is determined according to the lowest out of bag and the best correlation.

- \code{Best mtry determination} permits to determine the best mtry for the supervised random forest analysis with the to sufficient taxonomic units. The accuracy of random forest depends of the mtry value.

- \code{Calculate/plot supervised random forest} is a generic function to calculate and/or to plot supervised random forest with the top sufficient taxonomic units.

- \code{information about top sufficient taxonomic units} is a generic function usefull to obtain mean, standard deviation and dominance index of top sufficient taxonomic units in each microbial clusters.



The \code{Supplementary analysis} menu provides helpfull supplementary analysis.

- \code{Diversity} calculates several diversity index (Richness, Number of individual, Dominance, Shanon, Simpson, Eveness, Equitability and berger parker dominance)

- \code{Decision tree} calculates a conditional inference decision based on the top sufficient taxonomic units (see ctree function from party package).



The \code{Miscellaneous} menu is composed of various tools to improve the use of \code{RandForestGUI} package.

- \code{Language} allows languages to be changed. For instance, only USA english, french, german and Italian can be selected.

- \code{Memory increase} permits to increase the RAM memory allocated to R. If RandForestGUI frequently bugs, this function is most of time the solution!

- \code{Background plot colors} permits to change the background color of RandForestGUI plots.





}
\author{ Rory Michelland \email{rory.michelland@gmail.com}\cr
}
\examples{
\dontrun{
## One of these lines allows the StatFingerprints program to be started
RandForestGUI()
RandforestGUI()
randforestGUI()
randForestGUI()
Randforestgui()
randforestgui()
randForestgui()
RandforestGui()
randforestGui()
randForestGui()
RF()
rf()
Rf()
}
}
