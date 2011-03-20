fun isert a nil = nil
|   isert a (l::ls) = (a::l)::isert a ls;

val mls = [[1,2,3],[2,3,4],[5,5,3],nil];
isert 1 mls;
