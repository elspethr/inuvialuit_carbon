# Figure 2
cols = (viridis(4))[c(2,3,4,1)]
cols[3] = "chocolate4"

#note phylopic 2.0 launched, rphylopic doesn't work now
caribou = image_data("e6e864fd-8e3d-435f-9db3-dc6869c589f1", size = 128)[[1]]
char = image_data("68304786-f291-4115-9ccd-ead068ba6f19", size = "thumb")[[1]]
goose = image_data("7b8fb3d4-0cac-4552-8cd1-bd493b7de679", size="thumb")[[1]]

png("Figure2.png", height=4, width=6, 
    units="in", res=480, pointsize=10)
par(mar=c(1,4,3,1), cex.main=0.99)
layout(matrix(c(1,2,3,4), ncol=4, byrow=TRUE), 
  widths=c(1.75,1.75,3,1))
nsamps = 100
alp = 0.15
pullasamp = sample(1:6000, nsamps) 
plot(density(samps$harvest_est[,7,1]/1000)$y, 
     density(samps$harvest_est[,7,1]/1000)$x, xlab="", type="n", xaxt="n",
     main="(a) Edible weight", ylab="Tonnes", xlim=c(0,1), 
     ylim=c(5, 140))
for (i in 1:4) {
    abline(h=samps$harvest_est[pullasamp,7,i]/1000, 
           col=alpha(cols[i], alp))
}
box()
plot(density(samps$market_cost_est[,7,1]/1000000)$y, 
     density(samps$market_cost_est[,7,1]/1000000)$x, xlab="", type="n", xaxt="n",
     main="(b) Substitution value", ylab="Million $", xlim=c(0,100), 
     ylim=c(0.1, 3.4))
for (i in 1:4) {
  abline(h=samps$market_cost_est[pullasamp,7,i]/1000000, 
         col=alpha(cols[i], alp))
}
box()
plot(density(samps$carbon_cost_est[,7,1,1]/1000)$y, 
     density(samps$carbon_cost_est[,7,1,1]/1000)$x, xlab="", 
     type="n",  xaxt="n",
     main=expression(bold(paste("(c) ", CO[2], " emitted by substitutes"))), 
     ylab="Tonnes", xlim=c(0.15,3.85), ylim=c(20, 1200))
for (i in 1:4) {
    for (j in 1:4) {
      segments(x0=rep(j-1,nsamps), 
               y0=samps$carbon_cost_est[pullasamp,7,i,j]/1000,
               x1=rep(j-0.01, nsamps), 
               y1=samps$carbon_cost_est[pullasamp,7,i,j]/1000, 
               col=alpha(cols[i], alp))
  }
}
abline(v=1:3, lwd=1)
text(x=c(0.5,1.5,2.5,3.5), y=c(0,0,0,0),
    labels=c("Low Barge", "High Barge", "Low Food Mail", "High Food Mail"),
    cex=0.7)
box()
par(mar=c(1,0,3,0))
plot(1:1000/5, 1:1000, type="n", axes=FALSE, ylab="", xlab="")
add_phylopic_base(goose, x=23, y = 50, ysize=60,
                  alpha = 1, col = cols[1])
text(80, 50, "Birds", adj=0)
add_phylopic_base(char, x=23, y = 120, ysize=60,
                  alpha = 1, col = cols[2])
text(80, 120, "Fish", adj=0)
add_phylopic_base(caribou, x=30, y = 200, ysize=72,
                  alpha = 1, col = cols[3])
text(80, 200, "Mammals", adj=0)
add_phylopic_base(goose, x=45, y = 275, ysize=38,
                  alpha = 1, col = cols[4])
add_phylopic_base(caribou, x=15, y = 300, ysize=42,
                  alpha = 1, col = cols[4])
add_phylopic_base(char, x=10, y = 260, ysize=34,
                  alpha = 1, col = cols[4])
text(80, 275, "Total", adj=0)
dev.off()