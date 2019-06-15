import matplotlib.pyplot as plt
import numpy as np

avgStd = [ [],
           [] 
              ]
avgOur = [ [67857, 64202, 61656, 64463, 61698],
           [184, 184, 190, 183, 185]
              ]
avgOld = [ [237392, 238268, 238268, 238817, 239878],
           [237392, 157959, 158088, 158865, 159119]
              ]

stdCalib = [np.sum(avgStdCalib[0])/5.0, 
            np.sum(avgStdCalib[1])/5.0,

our = (stdCalib[0]/(np.sum(avgOurCalib[0])/5.0),
            stdCalib[1]/(np.sum(avgOurCalib[1])/5.0), 
old = (stdCalib[0]/(np.sum(avgOldCalib[0])/5.0),
            stdCalib[1]/(np.sum(avgOldCalib[1])/5.0),
# data to plot
n_groups = 2

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
plt.title('Runtime for a dataset, after being autotuned')
plt.xticks(index + bar_width/2, ('D1', 'D2'))
plt.legend()

plt.tight_layout()
plt.savefig("LocVolCalibSML.png")
plt.show()
