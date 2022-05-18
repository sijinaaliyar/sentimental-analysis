if upcase(NAME) = "TEXTCLUSTER_CLUSTER_" then do;
ROLE = "SEGMENT";
LEVEL = "NOMINAL";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_PROB1" then do;
ROLE = "REJECTED";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_PROB10" then do;
ROLE = "REJECTED";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_PROB11" then do;
ROLE = "REJECTED";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_PROB12" then do;
ROLE = "REJECTED";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_PROB2" then do;
ROLE = "REJECTED";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_PROB3" then do;
ROLE = "REJECTED";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_PROB4" then do;
ROLE = "REJECTED";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_PROB5" then do;
ROLE = "REJECTED";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_PROB6" then do;
ROLE = "REJECTED";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_PROB7" then do;
ROLE = "REJECTED";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_PROB8" then do;
ROLE = "REJECTED";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_PROB9" then do;
ROLE = "REJECTED";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_SVD1" then do;
ROLE = "INPUT";
LEVEL = "INTERVAL";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_SVD10" then do;
ROLE = "INPUT";
LEVEL = "INTERVAL";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_SVD11" then do;
ROLE = "INPUT";
LEVEL = "INTERVAL";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_SVD12" then do;
ROLE = "INPUT";
LEVEL = "INTERVAL";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_SVD2" then do;
ROLE = "INPUT";
LEVEL = "INTERVAL";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_SVD3" then do;
ROLE = "INPUT";
LEVEL = "INTERVAL";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_SVD4" then do;
ROLE = "INPUT";
LEVEL = "INTERVAL";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_SVD5" then do;
ROLE = "INPUT";
LEVEL = "INTERVAL";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_SVD6" then do;
ROLE = "INPUT";
LEVEL = "INTERVAL";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_SVD7" then do;
ROLE = "INPUT";
LEVEL = "INTERVAL";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_SVD8" then do;
ROLE = "INPUT";
LEVEL = "INTERVAL";
end;
else 
if upcase(NAME) = "TEXTCLUSTER_SVD9" then do;
ROLE = "INPUT";
LEVEL = "INTERVAL";
end;
