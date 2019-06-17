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

our = std[1]/(np.sum(avgOur[1])/5.0) 
old = std[1]/(np.sum(avgOld[1])/5.0)
# data to plot

n_groups = 1

# create plot
fig, ax = plt.subplots()
index = np.arange(n_groups)
bar_width = 0.1
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
plt.xticks(index+bar_width/2, ('D2'))
ax.text(0, our+0.8, str(round(our,2)), color='black', va='center', 
        fontweight='bold')
ax.text(bar_width, old+0.8, str(round(old,2)), color='black', 
        va='center', fontweight='bold')
ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05), shadow=True, ncol=2)
plt.tight_layout()
plt.savefig("nn.png")
plt.show()
