{
  type <- \name -> \bases -> \dict ->
    #record { __name: name, __bases: bases, __dict: dict }
}
