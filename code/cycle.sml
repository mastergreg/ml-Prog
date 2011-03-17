fun cyclelist(nil) = nil
|   cyclelist (m::ms) = ms @ [m]
and
    cycle (0, m) =  m  
|   cycle (i,m) = cycle( (i-1), cyclelist(m));
val testl=explode("asdfasdf");
cycle (2 ,testl);

