Given two non-empty arrays of integers, write a function that determines whether the 
second array is a subsequence of the first one.

A subsequence of an array is a set of numbers that aren't necessarily adjacent in the 
array but that are in the same order as they appear in the array. For instance, 
the numbers \[1, 3, 4\] form a subsequence of the array \[1, 2, 3, 4\], and so do the 
numbers \[2, 4\]. Note that a single number in an array and the array itself are both valid 
subsequences of the array.

### Sample Input

array = [5, 1, 22, 25, 6, -1, 8, 10]

sequence = [1, 6, -1, 10]

### Sample Output

true

### How to solve it

head tail programming!:

 - head is the first element of a sequence
 - tail is the rest element but the head

it takes up more lines the explanation that the code!
 - pick the head of the array and the subsequence
 - Are equals?
    - Yes, repeat the process with the tail of the array 
   and the tail of the subsequence
    - No, we gotta keep trying with the next element of the array, i.e, 
   repeat the process with the subsequence (and not the tail since we didnt 
   find a match for its head) and the tail of the array. 
    - When to stop? if the array and subsequence are both empty,
   we are done, the result is true. If the array is empty and
   the subsequence not (we didnt find a match for the head of the subsequence 
   in the whole array)
   ,the result is false

