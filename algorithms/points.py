"""
This is a implementation of the closest pair of points algorithm
using a divide and conquer approach.
Worst case time complexity is O(n^2). This could be improved by optimising
the strip comparison. Average case time complexity is O(n log n).
"""

from math import sqrt
import statistics

def dist(x0, y0, x1, y1):
  return sqrt((x0 - x1) ** 2 + (y0 - y1) ** 2)

# list of xs, ys and the coords of the closest two points
def closestPair(points):

  # base cases for length 1, 2, 3
  if len(points) == 1:
    return(points[0][0], points[0][1], float('inf'), float('inf'))

  elif len(points) == 2:
    return (points[0][0], points[0][1], points[1][0], points[1][1])
  
  elif len(points) == 3:
    d12 = sqrt((points[0][0] - points[1][0]) ** 2 + (points[0][1] - points[1][1]) ** 2)
    d13 = sqrt((points[0][0] - points[2][0]) ** 2 + (points[0][1] - points[2][1]) ** 2)
    d23 = sqrt((points[1][0] - points[2][0]) ** 2 + (points[1][1] - points[2][1]) ** 2)
    if (d12 <= d13 and d12 <= d23) :
      return (points[0][0], points[0][1], points[1][0], points[1][1])
    elif (d13 <= d12 and d13 <= d23) :
      return (points[0][0], points[0][1], points[2][0], points[2][1])
    else :
      return (points[1][0], points[1][1], points[2][0], points[2][1])
    
  # recursive case: divide and conquer
  else :

    # use the median of the coordinates to divide the points
    midpoint = statistics.median([point[0] for point in points])

    fstPoints = []
    sndPoints = []

    for point in points:
      if (point[0] <= midpoint):
        fstPoints.append(point)
      else:
        sndPoints.append(point)

    # get the closest pair of points in the left and right halves
    fstPair = closestPair(fstPoints)
    sndPair = closestPair(sndPoints)
    fstPairDist = dist(fstPair[0], fstPair[1], fstPair[2], fstPair[3])
    sndPairDist = dist(sndPair[0], sndPair[1], sndPair[2], sndPair[3])

    # compare the respective best pairs
    outPair = fstPair
    minDist = fstPairDist
    if (sndPairDist < fstPairDist):
      outPair = sndPair
      minDist = sndPairDist

    # check which points lie within the strip of width 2 * minDist
    compCoords = []
    for point in points:
      if (abs(point[0] - midpoint) <= minDist):
        compCoords.append(point)

    # check if any two points in the strip are closer than minDist
    for i in range(len(compCoords)):
      for j in range(i + 1, len(compCoords)):
        tmpDist = dist(compCoords[i][0], compCoords[i][1], compCoords[j][0], compCoords[j][1])
        if (tmpDist < minDist):
          minDist = tmpDist
          outPair = (compCoords[i][0], compCoords[i][1], compCoords[j][0], compCoords[j][1])

    return outPair


def __main__():
  xCords = [3, -7, 0, 10, -4, 5, -1, 6, -9, 2, -3, 8, -6, 1, -8]
  yCords = [-2, 9, -5, 4, -10, 7, -1, 0, 6, -3, 10, -4, 2, 5, -7]

  points = list(zip(xCords, yCords))

  # Sort points by x coordinate
  points.sort(key=lambda x: x[0])

  x0, y0, x1, y1 = closestPair(points)

  print(f"The closest pair of points are ({x0}, {y0}) and ({x1}, {y1})")

if __name__ == "__main__":
  __main__()