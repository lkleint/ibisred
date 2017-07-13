PRO statist,array
;+
; NAME:
;	STATIST
; PURPOSE:
; Invokes IDL-routine "help",
;	prints minimum, maximum, r.m.s., and arithmetic mean 
;	of data.
;*CATEGORY:            @CAT-# 33@
;	Statistics
; CALLING SEQUENCE:
;	STATIST,data
; INPUTS:
;	data : 1- or more-dimensional array containing numerical
;	       data.
; OUTPUTS:
;	none
; COMMON BLOCKS:
;	none
; SIDE EFFECTS:
;	print line to standard output.
; RESTRICTIONS:
;	none
; PROCEDURE:
;	straight (using IDL-routines help,min,max)
; MODIFICATION HISTORY:
;	nlte, 1990-03-17 
;	nlte, 1992-02-05  on_error
;       nlte, 1995-06-30  no call to former IDL-routine stdev
;-
on_error,1
help,array
n = n_elements(array)	;# of points.
if n le 1 then message, 'Number of data points must be > 1'
;
mean = total(array)/n	;yes.
rms=sqrt(total((array-mean)^2)/(n-1.))
print,'min',min(array),' max',max(array),' rms',rms,' mean',mean
return
end
