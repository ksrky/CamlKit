module A = Abstract.Syntax

type scope

val empty : scope

val scoping_prog : scope -> A.prog -> A.prog
