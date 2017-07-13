;+
; PROJET:
;	SOHO - LASCO
;
; NAME:
;	WHERE2D
;
; PURPOSE:
;	Performs the same job as the "where" function but for a 2D array
;
; PROCEDURE:
;	Performs the same job as the "where" function, for a 2-dimentional
;	array, but returns explicits a list of 2-D (x and y) subscripts,
;	instead of 1-D subscripts for "where"
;
;	Afterward, a 2-D subscripts array can be subscripted using the
;	resulting subscript array w2d from where2d, with a sequence such as :
;	arr( w2d(0,*), w2d(1,*) )   (instead of arr(w) with the where function)
;
; CATEGORY:
;	Detection
;
; CALLING SEQUENCE:
;	index2d = where2d(array [, count] )
;
; INPUTS:
;	array		An array (or more generally an array expression)
;			where the function will determine the nonzero elements
;
; OUTPUTS:
;	The result is the list of 2-D subscripts of the nonzero elements, i.e.:
;	_ if there are nonzero elements : a 2 x n array
;	  the first and second columns contains respectively the x and y
;	  subscripts of the non-zero elements of the array 
;	_ if there are NO nonzero elements, the result is the scalar -1
;
; Optional OUTPUTS:
;	count		the number of nonzero elements found by where2d
;			(=number of rows of the output list, or 0 if empty)
;
; MODIFICATION HISTORY:
;	v1.0 written by J.More, September 1996
;-


function  where2d, array, count

s = size(array)

;   if not a 2-D array, aborts
if s(0) ne 2 then return, -1

;  otherwise gets the width (number of columns) of the array
width = s(1)

;   calls the classical where function
w = where(array, count)

;   transforms the 1-D subscripts in 2-D subscripts
if count gt 0 then w2d = transpose( [[w mod width], [w / width]] ) $
              else w2d = -1

w2d = fix(w2d)


return, w2d
end