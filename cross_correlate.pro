pro cross_correlate,s1,s2,offset,corr,ishift=ishift,width=width,i1=i1,i2=i2
;+
;*NAME:
;			cross_correlate
;
;*PURPOSE:
; Normalized mean and covariance cross correlation offset between two input
; vectors or the same length.
;
; CALLING SEQUENCE
;	cross_correlate,s1,s2,OFFSET,CORR,ishift=n,width=n,i1=n,i2=n
;
; INPUTS:
;	s1 - first spectrum
;	s2 - second spectrum
;
; OUTPUTS:
;	offset - offset of s2 from s1 in data points
;	corr - output correlation vector
; KEYWORD INPUTS:
;	ishift - approximate offset (default = 0)
;	width - search width (default = 15)
;	i1,i2 - region in first spectrum containing the feature(s)
;		(default  i1=0, i2=n_elements(s2)-1)
;
; OPERATIONAL NOTES:
;	If the maximum correlation is found at the edge of the search
;	area, !err is set to -1 and a 0.0 offset returned.  Otherwise,
;	!err is set to 0.
; HISTORY:
;	Version 1  D. Lindler  Sept. 1991
;-
;---------------------------------------------------------------------------

if n_params(0) lt 1 then $
  message,'cross_correlate,s1,s2,offset,corr,ishift=ishift,width=width,i1=i1,i2=i2

	if n_elements(ishift) eq 0 then ishift = 0
	approx = long(ishift+100000.5)-100000		;nearest integer
	if n_elements(width) eq 0 then width = 15
	ns = n_elements(s1)
	if n_elements(i1) eq 0 then i1 = 0		;starting sample
	if n_elements(i2) eq 0 then i2 = ns-1		;ending sample
;
; extract template from spectrum 2
;
	ns2 = ns/2
	width2 = width/2
	it2_start = (i1-approx+width2) > 0
	it2_end   = (i2-approx-width2) < (ns-1)
	nt = it2_end - it2_start+1
	if nt lt 1 then begin
		print,'CROSS_CORRELATE - region too small, or '+ $
			'WIDTH too large, or ISHIFT too large'
		!err = -1
		offset = 0.0
		return
	end
	template2 = s2(it2_start:it2_end)
;
; correlate
;
	corr = fltarr(width)			;correlation matrix
	mean2 = total(template2)/nt
	sig2 = sqrt(total((template2-mean2)^2))
	diff2 = template2 - mean2
		
	for i=0,width-1 do begin
;
; find region in first spectrum
;
		it1_start = it2_start - width2 + approx + i
		it1_end = it1_start + nt -1		
		template1 = s1(it1_start:it1_end)
		mean1 = total(template1)/nt
		sig1 = sqrt(total((template1-mean1)^2))
		diff1 = template1 - mean1
		if (sig1 eq 0) or (sig2 eq 0) then begin
			print,'CROSS_CORRELATE - zero variance computed'
			!err = -1 & offset=0.0
			return
		end
		corr(i) = total(diff1*diff2)/sig1/sig2
	end
;
; Find maximum
;
	maxc = max(corr) & k=!c
	if (!c eq 0) or (!c eq (width-1)) then begin
		print,'CROSS_CORRELATE- maximum on edge of search area'
		!err = -1
		offset = 0.0
		return
	end
;
; USE QUADRATIC REFINEMENT
;
	Kmin=(corr(K-1)-corr(K))/(corr(K-1)+corr(K+1)-2.*corr(K))-0.5
	offset = K + Kmin - width2 + approx
	!err = 0
return
end
