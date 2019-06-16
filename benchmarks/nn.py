import matplotlib.pyplot as plt
import numpy as np

avgStd = [ [7775, 7789, 7980, 7784, 7910],
           [33783, 34569, 33974, 34237, 33676] 
         ]
avgOur = [ [8706, 8495, 7769, 7789, 8488],
           [1287, 1286, 1228, 1229, 1283]
         ]
avgOld = [ [7844, 8698, 7762, 8970, 7761],
           [1236, 1373, 1248, 1293, 1295]
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
bar_width = 0.4
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
plt.xticks(index+bar_width/2, ('D1', 'D2'))
plt.legend()
for i, v in enumerate(our):
  ax.text(i, v+0.8, str(round(v,2)), color='black', va='center', fontweight='bold')
for i, v in enumerate(old):
  ax.text(i+bar_width, v+0.8, str(round(v,2)), color='black', va='center', fontweight='bold')

plt.tight_layout()
plt.savefig("nn.png")
plt.show()
