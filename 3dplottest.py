import numpy as np
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import scipy.interpolate
import code

x = [0, 100]
y = [0, 100]
z = [0, 100]


code.interact(local=locals())
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(x, y, z)
plt.show()
