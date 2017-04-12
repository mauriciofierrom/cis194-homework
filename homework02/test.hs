test x = case x of
           [y,' '] -> init x
           _  -> "sdf"

test2 x = case x of
            (t:e:d:e') -> unwords e'
            _          -> "all"
