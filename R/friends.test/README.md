friends.test R implementation
===

This is R implementation of the friends.test .

The idea is: we have T rows and C columns and column-to-row relation described as a full |T|x|C| matrix. Relation is a comparable value, *e.g.* weight, load, correlation. First, we rank all relations of a column to all the rows inside the column. The ranks are referred to as importance of the rows to the column.

If a row is more important for a column than for other column(s), and the difference is more than by chance, the column is the row's friend and the row is the column's marker. "By chance" (null model)" implies uniform distribution of the row's importance for different columns.

**Preprint:** Suvorikova A, Kroshnin A, Lvovs D, Mukhina V, Mironov A, Fertig EJ, Danilova L, Favorov A (2025). *friends.test: identifying specificity by detecting structural breaks in entity interactions.* arXiv:2512.24843. <https://arxiv.org/abs/2512.24843>

See [NEWS.md](R/friends.test/NEWS.md) for the full version history.

