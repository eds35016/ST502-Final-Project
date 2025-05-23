# load('./results/lineups_PIR_sigmas.RData')
# load('./results/lineups_EFF_sigmas.RData')
# load('./results/lineups_Winscore_sigmas.RData')
# load('./results/lineups_TS_sigmas.RData')

load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_PIR_sigmas.RData')
load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_EFF_sigmas.RData')
load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_Winscore_sigmas.RData')
load('/Users/maddybursell/Documents/ST 502/ST502-Final-Project/Doneck_Dolphins_analysis/results/lineups_TS_sigmas.RData')

# 1. PIR line-ups---------
sort(summary_PIR)
sum(summary_PIR)

barplot(c(500/3000, 307/3000, 296/3000, 237/3000, 144/3000, 134/3000, 130/3000), 
        names=c(expression(L[1]), expression(L[2]), expression(L[3]), 
                expression(L[4]), expression(L[5]), expression(L[6]), 
                expression(L[7])),
        col = rgb(0.8,0,0, .6), ylim = c(0, 0.2), cex.names = 1.5, 
        cex.axis = 1.5, ylab = "Relative frequency",
        cex.lab=1.5, cex.main=1.5)


# 2. EFF line-ups---------
sort(summary_EFF)
sum(summary_EFF)

barplot(c(582/3000, 323/3000, 310/3000, 233/3000, 156/3000, 144/3000, 117/3000), 
        names=c(expression(L[1]), expression(L[2]), expression(L[3]), 
                expression(L[4]), expression(L[5]), expression(L[6]), 
                expression(L[7])),
        col = rgb(0.8,0,0, .6), ylim = c(0, 0.2), cex.names = 1.5, 
        cex.axis = 1.5, ylab = "Relative frequency",
        cex.lab=1.5, cex.main=1.5)


# 3. Win Score line-ups---------
sort(summary_Winscore)
sum(summary_Winscore)

barplot(c(523/3000, 416/3000, 338/3000, 315/3000, 202/3000, 158/3000, 157/3000), 
        names=c(expression(L[1]), expression(L[2]), expression(L[3]), 
                expression(L[4]), expression(L[5]), expression(L[6]), 
                expression(L[7])),
        col = rgb(0.8,0,0, .6), ylim = c(0, 0.2), cex.names = 1.5, 
        cex.axis = 1.5, ylab = "Relative frequency",
        cex.lab=1.5, cex.main=1.5)

# 4. True Shooting % line-ups---------
sort(summary_TS)
sum(summary_TS)

# Not positive that I am usually the correct counts
barplot(c(126/3000, 81/3000, 79/3000, 72/3000, 71/3000, 70/3000, 69/3000), 
        names=c(expression(L[1]), expression(L[2]), expression(L[3]), 
                expression(L[4]), expression(L[5]), expression(L[6]), 
                expression(L[7])),
        col = rgb(0.8,0,0, .6), ylim = c(0, 0.2), cex.names = 1.5, 
        cex.axis = 1.5, ylab = "Relative frequency",
        cex.lab=1.5, cex.main=1.5)

# PIR
# l1: annabel, rossi, dirk, patrik, walter    L1
# l2: annabel, rossi, dirk, natalie, walter   L3
# l3: annabel, dirk, patrik, svenja, walter   L2
# l4: annabel, rossi, dejon, dirk, walter     L4
# l5: annabel, dejon, dirk, svenja, walter    L7
# l6: annabel, rossi, dirk, svenja, walter    L5
# l7: annabel, dejon, dirk, natalie, patrik   L6
# 
# EFF
# l1: annabel, rossi, dirk, patrik, walter    L1
# l3: annabel, dirk, patrik, svenja, walter   L2
# l2: annabel, rossi, dirk, natalie, walter   L3
# l4: annabel, rossi, dejon, dirk, walter     L4
# l6: annabel, rossi, dirk, svenja, walter    L5
# l7: annabel, dejon, dirk, natalie, patrik   L6
# l5: annabel, dejon, dirk, svenja, walter    L7

# Win score
# l1: annabel, rossi, dirk, patrik, walter   L1
# l3: annabel, dirk, patrik, svenja, walter  L2
# l2: annabel, rossi, dirk, natalie, walter  L3
# l6: annabel, rossi, dirk, svenja, walter   L5
# l4: annabel, rossi, dejon, dirk, walter    L4
# l8: rossi, dirk, lucas, svenja, walter
# l5: annabel, dejon, dirk, svenja, walter   L7
# l7: annabel, dejon, dirk, natalie, patrik  L6