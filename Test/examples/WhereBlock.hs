hash ptr len = f len
 where f h = return h
       f p = (p `advancePtr` 1)
