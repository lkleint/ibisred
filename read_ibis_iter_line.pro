FUNCTION read_ibis_iter_line, iter_line, Niter, Nframe, date_string

  ;; Read iteration string from log file.

  substrings = strsplit(iter_line, "-", COUNT=count, /EXTRACT)
  iters = strsplit(strmid(substrings[0], strpos(substrings[0], "#")+1), " ", /EXTRACT)
  iterno = fix(iters[0])
  Niter  = fix(iters[2])
  Nframe = fix(strmid(substrings[1], 20))
  date_string = strtrim(substrings[2], 2)

  return, iterno
END
