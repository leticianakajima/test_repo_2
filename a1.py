from PIL import Image
import numpy as np
import math
from scipy import signal




blaaaahhhh
#check that n is odd -- if its not throws an error
#returns a box filter of size nxn -- a numpy array
def boxfilter(n):
    if (n%2 != 0): #odd
        try:
            assert False, "you have to input an odd number"
        except AssertionError, e:
            raise Exception(e.args)
    else:
        #make the box filter

        print('make the box')
