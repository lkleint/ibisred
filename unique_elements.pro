FUNCTION unique_elements, a, COUNT=count

  IF ((size(a))[0] EQ 0) THEN BEGIN
    count = 1
    return, [a]
  ENDIF

  sa = a[sort(a)]
  u  = [sa[0]]

  FOR i=1, n_elements(a)-1 DO $
    IF (sa[i] NE sa[i-1]) THEN u = [u, sa[i]]

  count = n_elements(u)
  return, u
END
