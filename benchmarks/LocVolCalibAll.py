import matplotlib.pyplot as plt
import numpy as np

avgStdCalib = [ [240290, 239479, 244412, 239221, 239722],
                [298248, 239479, 299696, 301184, 298872], 
                [4745410, 4768022, 4763633, 4800159, 4768614]
              ]
avgMCalib = [  [239424, 237995, 240378, 240361, 241050],
               [157382, 157398, 158831, 158831, 158102],
               [25040263, 24979413, 25157968, 25088769, 25318874]
            ]
avgSMCalib = [ [101424, 99248, 99177, 101288, 102347],
               [157815, 158033, 158510, 158650, 157161],
               [25024178, 25222631, 25263261, 25110657, 24977593]
             ]
avgMLCalib = [ [507779, 664339, 674933, 666679, 668026],
               [416656, 431973, 450056, 435412, 508915],
               [6945645, 6884476, 7045431, 7080790, 7175057]
             ]
avgSMLCalib = [ [98349, 98583, 99478, 98912, 98906],
                [158210, 158106, 159141, 158292, 158410],
                [2529428, 2530433, 2548801, 2544966, 2556094]
              ]
stdCalib = [np.sum(avgStdCalib[0])/5.0, 
            np.sum(avgStdCalib[1])/5.0,
            np.sum(avgStdCalib[2])/5.0]

mCalib =  (stdCalib[0]/(np.sum(avgMCalib[0])/5.0),
            stdCalib[1]/(np.sum(avgMCalib[1])/5.0), 
            stdCalib[2]/(np.sum(avgMCalib[2])/5.0))
smCalib = (stdCalib[0]/(np.sum(avgSMCalib[0])/5.0),
            stdCalib[1]/(np.sum(avgSMCalib[1])/5.0),
            stdCalib[2]/(np.sum(avgSMCalib[2])/5.0))
mlCalib = (stdCalib[0]/(np.sum(avgMLCalib[0])/5.0),
            stdCalib[1]/(np.sum(avgMLCalib[1])/5.0),
            stdCalib[2]/(np.sum(avgMLCalib[2])/5.0))
smlCalib = (stdCalib[0]/(np.sum(avgSMLCalib[0])/5.0),
            stdCalib[1]/(np.sum(avgSMLCalib[1])/5.0),
            stdCalib[2]/(np.sum(avgSMLCalib[2])/5.0))
# data to plot
n_groups = 3

# create plot
fig, ax = plt.subplots()
index = np.arange(n_groups)
bar_width = 0.2
opacity = 0.8

rects1 = plt.bar(index, mCalib, bar_width,
alpha=opacity,
color='r',
label='Trained on Medium')

rects2 = plt.bar(index + bar_width, smCalib, bar_width,
alpha=opacity,
color='g',
label='Trained on Small-Medium')

rects3 = plt.bar(index + bar_width*2, mlCalib, bar_width,
alpha=opacity,
color='b',
label='Trained on Medium-Large')

rects4 = plt.bar(index + bar_width*3, smlCalib, bar_width,
alpha=opacity,
color='y',
label='Trained on Small-Medium-Large')

for i, v in enumerate(mCalib):
   ax.text(i-0.1, v+0.05, str(round(v,2)), color='black', va='center', fontweight='bold'    )
for i, v in enumerate(smCalib):
   ax.text(i+bar_width-0.1, v+0.05, str(round(v,2)), color='black', va='center', fontweight='bold')

for i, v in enumerate(mlCalib):
   ax.text(i+bar_width*2-0.1, v+0.05, str(round(v,2)), color='black', va='center', fontweight='bold')

for i, v in enumerate(smlCalib):
  ax.text(i+bar_width*3-0.1, v+0.05, str(round(v,2)), color='black', va='center', fontweight='bold')

plt.xticks(index + bar_width*1.5, ('Small', 'Medium', 'Large'))
ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05), shadow=True, ncol    =2)


plt.tight_layout()
plt.savefig("LocVolCalibAll.png")
plt.show()
