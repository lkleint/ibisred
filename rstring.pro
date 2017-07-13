;+
;PURPOSE
;	to produce a compressed string
;	returns  strcompress(string(var, _extra=_extra), /remove_all)
;SYNTAX
;	str=rstring(arr, _extra=_extra, pad=pad)
;INPUTS
;	arr: array you wish to convert
;	_extra: extra keywords for string()
;	if you want to pad with zeros put the power of 10 you want to pad to
;
;Written by R. da Silva, UCSC, Fall 2009
;-
function rstring, var, _extra=_extra, pad=pad
str=strcompress(string(var, _extra=_extra), /remove_all)
if keyword_set(pad) then str= padzero(str, pad)
return, str
end
