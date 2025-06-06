friends.test R implementation
===

This is R implementation of the friends.test.
The idea is: we have T tags and C collections and 
collection-to-tag attention described as a |T|x|C| matrix.
Attention is a metaphor for any relation, weight, load, etc.
First, we rank all attentions of a collection to all tags. 
The ranks are referred to as importance of the tags to the collection.

If a tag is important for a collection more than by chance, 
the collection is tag's friend and the tag is the collection's marker.
"By chance" (null model)" implies uniform distribution of the tag's 
importance in different collections. 

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
0.99.63 - Switched to tag + collection terminology.  
0.99.64 - friends.test output is dimensions is |T|x|C| for ranks, |T|x|C-1| for p-values and putative friends.  
0.99.65 - create separate function for the first ranking, friends.test does not return the ranks any more; documentation is updated again.  
0.99.7 - new functions appear for ks test of uniformity  of ranks of a tag in different collections and for the likelihood of a step in the ranks (thanks to A. Kroshnin and A. Suvorikova).  
0.99.8 - added fields about the best step in the return.  
0.99.9 - all the old tests (friends, best.friends) are removed.  
0.99.10 - unit tests started; There are only NOTES in BiocCheck::BiocCheck again.  
0.99.11 - major bugfix (any uniform part is never empty now); the function that fit models and the function that finds the best are separated.  
0.99.12 - best.friends function added that put it altogether; jitter amplitude lowered to make KS more stable; best.friends now returns data frame even when the return is empty, tests added, docs improved.   
0.99.13 - ks on ranks mapped to 0..1 rather than on raw ranks  
0.99.14 - the Bayesian (bic) version of the functions added.  
0.99.15 - the "all" best_no parameter behaviour fixed. The vingnette is rewritten.  
0.99.16 - the name changed to "friends.test". Parameter best.no renamed to friends.no . Documentation is rewritten.
