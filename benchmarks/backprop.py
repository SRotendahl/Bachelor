import matplotlib.pyplot as plt
import numpy as np

avgStd = [ [315, 288, 295, 287, 287],
           [14639, 13476, 13742, 13610, 13495] 
         ]
avgOur = [ [287, 314, 289, 294, 288],
           [13761, 14467, 13544, 13506, 13698]
         ]
avgOld = [ [316, 288, 289, 316, 290],
           [14481, 14297, 14279, 15804, 13870]
         ]

std = [np.sum(avgStd[0])/5.0, 
       np.sum(avgStd[1])/5.0]

our = (std[0]/(np.sum(avgOur[0])/5.0),
       std[1]/(np.sum(avgOur[1])/5.0)) 
old = (std[0]/(np.sum(avgOld[0])/5.0),
       std[1]/(np.sum(avgOld[1])/5.0))
# data to plot

n_groups = 2

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
plt.title('Runtime for a dataset, after being autotuned')
plt.xticks(index+bar_width/2, ('D1', 'D2'))
plt.legend()

plt.tight_layout()
plt.savefig("backprop.png")
plt.show()
