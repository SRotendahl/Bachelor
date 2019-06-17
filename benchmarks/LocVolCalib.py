import matplotlib.pyplot as plt
import numpy as np

avgStdCalib = [ [240290, 239479, 244412, 239221, 239722],
                [298248, 239479, 299696, 301184, 298872], 
                [4745410, 4768022, 4763633, 4800159, 4768614]
              ]
avgOurCalib = [ [98349, 98583, 99478, 98912, 98906],
                [158210, 158106, 159141, 158292, 158410],
                [2529428, 2530433, 2548801, 2544966, 2556094]
              ]
avgOldCalib = [ [237392, 238268, 238268, 238817, 239878],
                [237392, 157959, 158088, 158865, 159119],
                [2535680, 2537991, 2544248, 2545626, 2556094]
              ]

stdCalib = [np.sum(avgStdCalib[0])/5.0, 
            np.sum(avgStdCalib[1])/5.0,
            np.sum(avgStdCalib[2])/5.0]

ourCalib = (stdCalib[0]/(np.sum(avgOurCalib[0])/5.0),
            stdCalib[1]/(np.sum(avgOurCalib[1])/5.0), 
            stdCalib[2]/(np.sum(avgOurCalib[2])/5.0))
oldCalib = (stdCalib[0]/(np.sum(avgOldCalib[0])/5.0),
            stdCalib[1]/(np.sum(avgOldCalib[1])/5.0),
            stdCalib[2]/(np.sum(avgOldCalib[2])/5.0))
# data to plot
n_groups = 3

# create plot
fig, ax = plt.subplots()
index = np.arange(n_groups)
bar_width = 0.2
opacity = 0.8

rects1 = plt.bar(index, ourCalib, bar_width,
alpha=opacity,
color='r',
label='Our tuner')

rects2 = plt.bar(index + bar_width, oldCalib, bar_width,
alpha=opacity,
color='g',
label='Exsisting tuner')

plt.xlabel('Dataset')
plt.ylabel('Speedup')
plt.xticks(index + bar_width/2, ('Small', 'Medium', 'Large'))
ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05), shadow=True, ncol    =2)


for i, v in enumerate(ourCalib):
    ax.text(i-0.05, v+0.05, str(round(v,2)), color='black', va='center', fontweight='bold')
for i, v in enumerate(oldCalib):
    ax.text(i+bar_width-0.05, v+0.05, str(round(v,2)), color='black', va='center',
            fontweight='bold')

plt.tight_layout()
plt.savefig("LocVolCalib.png")
plt.show()
