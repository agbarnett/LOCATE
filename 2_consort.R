# 2_consort.R
# CONSORT flow diagram
# called by 2_descriptive_stats.Rmd
library(diagram)


# check of those without reason but not randomised
filter(for_consort, is.na(reason), is.na(randomised))

# get numbers
n_approached = nrow(for_consort)
n_randomised = sum(is.na(for_consort$randomised)==FALSE)
n_uc = sum(for_consort$randomised == 'Usual care', na.rm = TRUE)
n_nm = sum(for_consort$randomised == 'New model of care', na.rm = TRUE)
# exclusion numbers prior to randomisation
exclusion_n = filter(for_consort, 
                     is.na(randomised), # must be prior to randomisation
                     !is.na(reason)) %>%
  group_by(reason) %>%
  tally() %>%
  ungroup()
# follow-up (dummy for now - use 12 month numbers once available)
lost.fu.uc = nrow(filter(for_consort, !is.na(randomised), randomised == 'Usual care', !is.na(reason)))
died.uc = nrow(filter(for_consort, !is.na(randomised), randomised == 'Usual care', reason == 'Died'))
lost.fu.nm = nrow(filter(for_consort, !is.na(randomised), randomised == 'New model of care', !is.na(reason)))
died.nm = nrow(filter(for_consort, !is.na(randomised), randomised == 'New model of care', reason == 'Died'))
analysed.uc = n_uc - lost.fu.uc
analysed.nm = n_nm - lost.fu.nm

## Add per protocol definition
# patients that attended their Fibroscan appointment and were able to be scanned
pp.uc = analysed.uc # include all as PP does not apply
pp.nm = nrow(filter(for_consort, randomised=='New model of care', pp==TRUE)) # to do, need scan data

# labels
b = c('Enrollment', 'Allocation', 'Follow-up', 'Analysis')
l1 = paste('Approached\n(n=', n_approached, ')', sep='')
l3 = paste('Randomised\n(n=', n_randomised, ')', sep='')
l4 = paste('Usual care\n(n=', n_uc, ')', sep='')
l5 = paste('New model\n(n=', n_nm, ')', sep='')
l6 = paste('Lost to follow-up (n=', lost.fu.uc, ')\n', # usual care lost to fu
           '- Died (n=', died.uc,')', sep='')
l7 = paste('Lost to follow-up (n=', lost.fu.nm, ')\n', # new mode lost to fu
           '- Died (n=', died.nm,')', sep='')
l8 = paste('Analysed (n=', analysed.uc, ')\n', 
           '- Per protocol (n=', pp.uc,')', sep='')
l9 = paste('Analysed (n=', analysed.nm, ')\n', 
           '- Per protocol (n=', pp.nm,')', sep='')
# exclusion labels
l2 = paste('Excluded (n=', sum(exclusion_n$n), ')\n',
           '- Could not be contacted (n=', filter(exclusion_n, reason == 'Could not be contacted')$n, ')\n',
           '- Risky alcohol or liver problem (n=', filter(exclusion_n, reason == 'AUDIT (risky alcohol) or liver problem')$n, ')\n',
           '- Seen other specialist (n=', filter(exclusion_n, reason == 'Have been evaluated in a specialist hepatology clinic in the previous 12 months')$n, ')\n',
           '- Plans to move (n=', filter(exclusion_n, reason == 'Plans to move')$n, ')\n',
           '- Terminal illness (n=', filter(exclusion_n, reason == 'Terminal illness')$n, ')\n',
           '- No time (n=', filter(exclusion_n, reason == 'No time to take part')$n, ')\n',
           '- Other reasons (n=', filter(exclusion_n, reason == 'Other')$n, ')', sep='')

labels = c(l1, l2, l3, l4, l5, l6, l7, l8, l9, b)
n.labels = length(labels)
### make data frame of box chars
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.5	0.94	white	square	0.25	0.16
2	0.77	0.79	white	square	0.47	0.23
3	0.5	0.62	white	square	0.25	0.15
4	0.26	0.45	white	square	0.23	0.2
5	0.76	0.45	white	square	0.23	0.2
6	0.26	0.27	white	square	0.2	0.2
7	0.76	0.27	white	square	0.2	0.2
8	0.26	0.10	white	square	0.2	0.2
9	0.76	0.10	white	square	0.2	0.2
10	0.1	0.94	light blue	round	0.72	0.035
11	0.51	0.53	light blue	round	0.7	0.035
12	0.51	0.36	light blue	round	0.7	0.035
13	0.51	0.18	light blue	round	0.7	0.035')
pos = as.matrix(subset(frame, select=c(x, y)))
M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
M[3, 1] = "' '"
M[4, 3] = "' '"
M[5, 3] = "' '"
M[6, 4] = "' '"
M[7, 5] = "' '"
M[8, 6] = "' '"
M[9, 7] = "' '"
tcol = rep('black', n.labels)
to.blank = c(2,4:9)
tcol[to.blank] = 'transparent' # blank some boxes to add text as right aligned
# function to repeat figure
make_figure = function(){
  par(mai=c(0,0.04,0.04,0.04))
  plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0,
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = tcol)
  # add left-aligned text; -0.185 controls the horizontal indent
  for (i in to.blank){
    text(x=pos[i,1] - 0.185, y=pos[i,2], adj=c(0,0.5), labels=labels[i]) # minus controls text position
  }
  # extra arrow to excluded
  shape::Arrows(x0=0.5, x1=0.53, y0=0.82, y1=0.82, arr.width = 0.25, arr.length=0.19, arr.typ='triangle')
}

#
jpeg('figures/consort_flow.jpg', width=7.5, height=8, units='in', res=300, quality=100)
make_figure()
dev.off()
