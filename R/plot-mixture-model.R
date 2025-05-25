#nM<-M/rowSums(M)
Morig<-M
scale<-1/2
scaleMask<-mask
scaleMask[selected_rows,]=!mask[selected_rows]
M[scaleMask]=M[scaleMask]*scale


hM<-Heatmap(M,
  name = "Mixture test", # Name of the heatmap legend
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  show_row_names = FALSE,
  show_column_names = FALSE,
  heatmap_legend_param = list(title = "Value"), # Customize legend title
  col = colorRamp2::colorRamp2(c(0, 0, max(M)),
  c("white", "white", "red")), # Optional: custom color scheme
  column_title = "mixture"
)

falsecolor<-"gray98"

friends<-friends.test::friends.test.bic(M,prior.to.have.friends = 0.001)


hmask<-Heatmap(mask,
             name = "mask", # Name of the heatmap legend
             cluster_rows = FALSE,
             cluster_columns = FALSE,
             show_row_names = FALSE,
             show_column_names = FALSE,
             col = colorRamp2::colorRamp2(c(0, 0, 1),
                                          c(falsecolor, falsecolor, "red")), # Optional: custom color scheme
             column_title = "mask",
             show_heatmap_legend = FALSE
)


friends.mat<-matrix(0,nrow=nrow(M),ncol=ncol(M))
rownames(friends.mat)<-rownames(M)
colnames(friends.mat)<-colnames(M)

for(r in seq(nrow(friends))){
  friends.mat[friends[r,"marker"],friends[r,"friend"]]=1
}

hfb<-Heatmap(friends.mat,
            name = "Friends", # Name of the heatmap legend
            cluster_rows = FALSE,
            cluster_columns = FALSE,
            show_row_names = FALSE,
            show_column_names = FALSE,
            col = colorRamp2::colorRamp2(c(0, 0, 1),
                          c(falsecolor, falsecolor, "orange")), # Optional: custom color scheme
            column_title = "friends.test.bic",
            show_heatmap_legend = FALSE
)

pdf("mixture_and_friends_bic.pdf")
plot(hM+hmask+hfb)
#dev.off()

friends<-friends.test::friends.test(M)


hmask<-Heatmap(mask,
               name = "mask", # Name of the heatmap legend
               cluster_rows = FALSE,
               cluster_columns = FALSE,
               show_row_names = FALSE,
               show_column_names = FALSE,
               col = colorRamp2::colorRamp2(c(0, 0, 1),
                                            c(falsecolor, falsecolor, "red")), # Optional: custom color scheme
               column_title = "mask",
               show_heatmap_legend = FALSE
)


friends.mat<-matrix(0,nrow=nrow(M),ncol=ncol(M))
rownames(friends.mat)<-rownames(M)
colnames(friends.mat)<-colnames(M)

for(r in seq(nrow(friends))){
  friends.mat[friends[r,"marker"],friends[r,"friend"]]=1
}

hfb<-Heatmap(friends.mat,
             name = "Friends", # Name of the heatmap legend
             cluster_rows = FALSE,
             cluster_columns = FALSE,
             show_row_names = FALSE,
             show_column_names = FALSE,
             col = colorRamp2::colorRamp2(c(0, 0, 1),
                                          c(falsecolor, falsecolor, "orange")), # Optional: custom color scheme
             column_title = "friends.test",
             show_heatmap_legend = FALSE
)

plot(hM+hmask+hfb)
dev.off()

