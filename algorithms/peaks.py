""" 
Script for 2d peak finding. 
A peak is defined as a point that is greater than or equal to its neighbors.
Complexity is O(log(n))
"""

import math

# example 2d array
array = [[1, 1, 1, 4, 5],
        [1, 2, 1, 2, 1],
        [1, 1, 1, 2, 3],
        [1, 1, 1, 2, 3],
        [1, 1, 1, 2, 3],]

# check if the array index is a peak
def is_peak(array, i, j):
    rows = len(array)
    cols = len(array[0])
    
    if i < 0 or i >= rows or j < 0 or j >= cols:
        return False
    
    current_value = array[i][j]
    
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

    for di, dj in directions:
        ni, nj = i + di, j + dj
        if 0 <= ni < rows and 0 <= nj < cols:
            if array[ni][nj] > current_value:
                return False
    
    return True

# find the peak in the array given by the boundaries
def find_peak(array, top, bottom, left, right):

    rows = bottom - top
    cols = right - left
    
    i = math.floor(rows / 2)
    j = math.floor(cols / 2)

    if (is_peak(array, top + i, left + j)):
        return (top + i, left + j, array[top + i][left + j])
    else:
        directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
        for k in range(4):

          if (top + i + directions[k][0] < 0 or top + i + directions[k][0] >= bottom or left + j + directions[k][1] < 0 or left + j + directions[k][1] >= right):
            continue

          if (array[top + i + directions[k][0]][left + j + directions[k][1]] > array[top + i][left + j]):
            if (k == 0): 
              return find_peak(array, top, top + i if rows % 2 == 0 else i + 1, left, right)
            if (k == 1): 
              return find_peak(array, top + i, bottom, left, right)
            if (k == 2):
              return find_peak(array, top, bottom, left, left + j if cols % 2 == 0 else j + 1)
            if (k == 3):
              return find_peak(array, top, bottom, left + j, right)
        
(i, j, peak_value) = find_peak(array, 0, len(array), 0, len(array[0]))
print(f"Peak found at ({i}, {j}) with value {peak_value}")