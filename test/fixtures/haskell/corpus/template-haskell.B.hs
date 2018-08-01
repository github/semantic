[|example'|]
[e|example'|]
[p|example'|]
[t|example'|]
[d|example'|]
[str|example'|]

[ | example'| ]
[ e| example'| ]
[ p| example'| ]
[ t| example'| ]
[ d| example'| ]
[ str| example'| ]

[ | example' |  ]
[ e | example' |  ]
[ p | example' |  ]
[ t | example' |  ]
[ d | example' |  ]
[ str | example' |  ]
[str|integer,double,varchar,boolean,date,money,enum
    |12,0.1,a string,true,1929-10-01,12,bar
    |13,3.14159,testing!,false,1900-01-01,$3.99,foo
    |]

[ | [{ "ret_setof_integers": 3 },
     { "ret_setof_integers": 4 },
     { "ret_setof_integers": 5 }] | ]

[|
  [
    { "ret_setof_integers": 3 },
    { "ret_setof_integers": 4 },
    { "ret_setof_integers": 5 }
  ]
|]

g = [|a|] `b` c

g = $x
g = $(a . b $ c)
$(makeEff' ''Embedded)
