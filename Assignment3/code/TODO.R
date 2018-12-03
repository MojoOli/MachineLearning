
#BEGIN OLI
##preprocessing
#+ low-pass filter
#+ filter out gravity

##feature derivation
#- frequency magnitude
#- frequency phase (mean /max/min/median/mad)
#- lenght of signal
#END OLI
#BEGIN ANA
#+ integral - area under the signal
#+ sliding window  (mean/max/min/median/mad)
#END ANA

#BEGIN ROLI
#+ wavelet components

#plots
#+ correlation of signals (x/y/z/m)
#+ filtered vs non signal plot
#+ model parameter boxplots
#+ interpolated vs normal signal plot
#END ROLI

#BEGIN BOBS
#Dimensionality reduction
#+ PCA

#model tarining
#+ train with raw_data vs features vs magnitude_raw_data (svm,lda,lda2,knn,rf)
#END BOBS
#BEGIN ANA
#+ gallery dependent data partitioning
#+ gallery independent data partitioning (LSOCV)
#END ANA
