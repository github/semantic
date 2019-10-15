{
  type <- \name -> \bases -> \dict ->
    #record { __name: name, __bases: bases, __dict: dict };

  object <- type "object" #unit #record{}

  #record { type: type, object: object }
}
