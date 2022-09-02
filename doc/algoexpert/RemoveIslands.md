Imagine a blank and white image that consists of pixels, 
and these pixels are either black or white. The representation 
of this image is in the form of a matrix, which only has 
the numbers zero (black pixel) and one (white pixel)

You are going to be tasked with removing all the white pixels 
that are not connected to the border of the image. Two adjacent 
pixels can be connected horizontally or vertically, but not 
diagonally. 

We call islands to these blocks of white pixels not connected to the borders.

### Sample Input

1,0,0,0,0,0
0,1,0,1,1,1
0,0,1,0,1,0
1,1,0,0,1,0
1,0,1,1,0,0
1,0,0,0,0,1


### Sample Output

1,0,0,0,0,0
0,0,0,1,1,1
0,0,0,0,1,0
1,1,0,0,1,0
1,0,0,0,0,0
1,0,0,0,0,1