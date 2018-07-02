self: super: 

{
  haskell.project = (import ./haskell/project) self super;
}