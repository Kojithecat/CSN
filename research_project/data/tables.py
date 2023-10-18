import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as col
import os

files = os.scandir(path = "./npy")
x_ticks = ["6","8","10","15"]
#y_ticks = reversed(["0.0","0.1","0.5","0.67","0.9","1.0"])
y_ticks = ["1.0","0.9","0.67", "0.5", "0.1","0.0"]


for f in files:
    matrix = np.load(f)
    print(os.path.basename(f))

    maxim = matrix.max()
    minim = abs(matrix.min())

    print(maxim, minim)

    matrix2 = np.where(matrix < 0, matrix/minim, matrix/maxim)

    print(y_ticks)

    #cmap = col.ListedColormap([(85/255, 146/255, 225/255, 0.8), (225/255, 85/255, 85/255, 0.8)])
    #cmap = col.ListedColormap([(225/255, 85/255, 85/255, 0.8)])
    cmap = col.ListedColormap([(85/255, 146/255, 225/255, 0.8)])
    plt.xticks(ticks=[0,1,2,3], labels = x_ticks, fontsize = 22)
    plt.yticks(ticks=[0,1,2,3,4,5], labels=y_ticks, fontsize = 22)

    plt.imshow(-100-(matrix2), aspect='auto', cmap=cmap)

    title = "Communities"
    laby = "Proportion, p"

    plt.title(title, fontsize = 46)

    plt.ylabel(laby, fontsize = 46)



    for (i, j), value in np.ndenumerate(matrix):
        plt.text(j, i, "%.1f"%value, va='center', ha='center', fontsize = 20)
    #plt.axis('off')

    plt.show()
    plt.cla()
