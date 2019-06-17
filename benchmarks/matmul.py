import matplotlib.pyplot as plt
import numpy as np

avgStd = [ [6203061, 3206544, 4879213, 3209149, 3260515],
           [1349308, 1358943, 1359098, 1349140, 1352781] 
         ]
avgOur = [ [3241344, 3234315, 3221410, 3239985, 3215943],
           [1346043, 1356790, 1360259, 1361760, 1356191]
         ]
avgOld = [ [3235383, 3229923, 3235401, 3233851, 3281149],
           [1351552, 1355432, 1354889, 1355565, 1352287]
         ]

std = [np.sum(avgStd[0])/5.0, 
       np.sum(avgStd[1])/5.0]

our = (std[0]/(np.sum(avgOur[0])/5.0),
       std[1]/(np.sum(avgOur[1])/5.0)) 
old = (std[0]/(np.sum(avgOld[0])/5.0),
       std[1]/(np.sum(avgOld[1])/5.0))
# data to plot

n_groups = 2

labels = [our[0], old[0], our[1], old[1]]

# create plot
fig, ax = plt.subplots()
index = np.arange(n_groups)
bar_width = 0.2
opacity = 0.8

rects1 = plt.bar(index, our, bar_width,
alpha=opacity,
color='r',
label='Our tuner')

rects2 = plt.bar(index + bar_width, old, bar_width,
alpha=opacity,
color='g',
label='Exsisting tuner')

plt.xlabel('Dataset')
plt.ylabel('Speedup')
plt.xticks(index+bar_width/2, ('F1', 'F2'))
ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05), shadow=True, ncol    =2)

for i, v in enumerate(our):
    ax.text(i, v+v/50, str(round(v,2)), color='black', va='center', fontweight='bold')
for i, v in enumerate(old):
    ax.text(i+bar_width, v+v/50, str(round(v,2)), color='black', va='center', fontweight='bold')
plt.tight_layout()
plt.savefig("matmul.png")
plt.show()
