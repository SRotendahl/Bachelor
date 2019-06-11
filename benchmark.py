import matplotlib.pyplot as plt
import numpy as np

avgStdCalib = [[]]
avgOurCalib = [[523905], [389276], [
    ]]

avgOldCalib = [[]]

stdCalib = ()
ourCalib = ()
oldCalib = ()

# data to plot
n_groups = 3
means_frank = (90, 55, 40)
means_guido = (85, 62, 54)

# create plot
fig, ax = plt.subplots()
index = np.arange(n_groups)
bar_width = 0.2
opacity = 0.8

rects0 = plt.bar(index, means_frank, bar_width,
alpha=opacity,
color='r',
label='No tuning')

rects1 = plt.bar(index + bar_width, means_frank, bar_width,
alpha=opacity,
color='g',
label='Exsisting tuner')

rects2 = plt.bar(index + bar_width*2, means_guido, bar_width,
alpha=opacity,
color='b',
label='Our tuner')

plt.xlabel('Dataset')
plt.ylabel('Time (microseconds)')
plt.title('Runtime for a dataset, after being autotuned')
plt.xticks(index + bar_width*2, ('Small', 'Medium', 'Large'))
plt.legend()

plt.tight_layout()
plt.show()
