import os
import numpy as np
import a, b as c
import b.c as d, e

from b import a
from b import (c, a)
from b import *
from b import a as x, b as y

def print_cwd():
    print(os.getcwd())

def create_array():
    x = np.array([1, 2, 3, 4])
    return x

def sum_array(x):
    x_sum = x.sum()
    return x_sum

def sum_array2(x):
    return x.sum()

if __name__ == '__main__':
    print('-' * 60 + '\nCurrent directory:')
    print_cwd()
    x = create_array()
    x_sum = sum_array(x)
    x_sum2 = sum_array2(x)
    print('-' * 60 + '\nYour sum is {}'.format(x_sum))
    print('-' * 60 + '\nYour sum2 is {}\n'.format(x_sum2) + '-' * 60)
