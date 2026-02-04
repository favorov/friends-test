friends.test R implementation
===

This is R implementation of the friends.test.
The idea is: we have T rows and C columns and 
column-to-row attention described as a |T|x|C| matrix.
Attention is a metaphor for any relation, weight, load, etc.
First, we rank all attentions of a column to all rows. 
The ranks are referred to as importance of the rows to the column.

If a row is important for a column more than by chance, 
the column is row's friend and the row is the column's marker.
"By chance" (null model)" implies uniform distribution of the row's 
importance in different columns. 

0.0.1 - initial version.  
0.1.1 - p-value calculated.  
0.2.1 - cpp based p-value, first working version.  
0.2.2 - process NAs in relation; use frankv order parameter for the direction.  
0.2.3 - returns names for the feature and the friend.  
0.2.4 - we added the calculation for n top entities - friends of the feature.  
Possibly, n is the number of the features we know, so we test each for being
the worst of best friends.  
0.2.5 - documentation updated, vignette added.  
0.3.0 - names changed, documentation updated.  
0.99.0 - we changed the terminology to elements+communities, added the friends test, prepared a vignette.  
0.99.1 - devtools::check passed with one note.  
0.99.3 - devtools::check passed with no notes or errors.   
0.99.4 - the vignette is fixed and improved.  
0.99.5 - trigger re-check, the mail list error fixed.  
0.99.6 - Documentation updated; non-diagonal options added.   
0.99.61 - Changing rank normalisation scheme.  
0.99.62 - The math in rd is \eqn{}. NOTES from BiocCheck::BiocCheck.  
0.99.63 - Switched to tag + colllection terminology.  
0.99.64 - friends.test output is dimensions is |T|x|C| for ranks, |T|x|C-1| for p-values and putative friends.  
0.99.65 - create separate function for the first ranking, friends.test does not return the ranks any more; documentation is updated again.  
0.99.7 - new functions appear for ks test of uniformity  of ranks of a tag in different collection and for the likelihood of a step in the ranks (thanks to A. Kroshnin and A. Suvorikova).  
0.99.8 - added fields about the best step in the return.  
0.99.9 - all the old tests (friends, best.friends) are removed.  
0.99.10 - unit tests started; There are only NOTES in BiocCheck::BiocCheck again.  
0.99.11 - major bugfix (any uniform part is never empty now); the function that fit models and the function that finds the best are separated.  
0.99.12 - best.friends function added that put it altogether; jitter amplitude lowered to make KS more stable; best.friends now returns data frame even when the return is empty, tests added, docs improved.   
0.99.13 - ks on ranks mapped to 0..1 rather than on raw ranks  
0.99.14 - the Bayesian (bic) version of the functions added.  
0.99.15 - the "all" best_no parameter behaviour fixed. The vingnette is rewritten.  
0.99.16 - the name changed to "friends.test". Parameter best.no renamed to friends.no . Documentation is rewritten.
0.99.17 - code linted and polished; 2 columns with indices of the marker row and the friendly column in the input matrix are added to the output of the main calls (friends.test and frinds.test.bic).
0.99.18 - we now retirn list of lists of 3-elemant vectors in both main functions. All the slow inner loops are now purrr::map - family based. .progress paraameter added. Bigfixes. 