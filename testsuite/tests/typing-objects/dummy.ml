(* TEST
   * expect
*)

class virtual child1 parent =
  object
    method private parent = parent
  end

and virtual child2 =
  object(_ : 'self)
    constraint 'parent = < previous: 'self option; .. >
    method private virtual parent: 'parent
  end

[%%expect{|
class virtual child1 : 'a -> object method private parent : 'a end
and virtual child2 :
  object ('a)
    method private virtual parent : < previous : 'a option; .. >
  end
|}]

class virtual child1' parent =
  object
    method private parent = parent
  end

and virtual child2' =
  object(_ : 'self)
    constraint 'parent = < previous: 'self option; .. >
    method private virtual parent: 'parent
  end

and foo = object(self)
  method previous = None
  method child =
    object
      inherit child1' self
      inherit child2'
    end
end;;

[%%expect{|
Line _, characters 22-26:
        inherit child1' self
                        ^^^^
Error: This expression has type < child : 'a; previous : 'b option; .. >
       but an expression was expected of type 'c
       Self type cannot escape its class
|}]

class foo = object(self)
  method previous = None
  method child =
    object
      inherit child1 self
      inherit child2
    end
end;;
[%%expect{|
class foo : object method child : child2 method previous : child2 option end
|}]

class nested = object
  method obj = object(self)
    method previous = None
    method child () =
      object
        inherit child1 self
        inherit child2
      end
  end
end;;
[%%expect{|
class nested :
  object
    method obj : < child : unit -> child2; previous : child2 option >
  end
|}]

class just_to_see = object
  method obj = object(self)
    method previous = None
    method child =
      let o =
        object
          inherit child1 self
          inherit child2
        end
      in
      o
  end
end;;
[%%expect{|
Line _, characters 18-24:
            inherit child2
                    ^^^^^^
Error: The method parent has type < child : 'a; previous : 'b option >
       but is expected to have type < previous : < .. > option; .. >
       Self type cannot escape its class
|}]

type gadt = Not_really_though : gadt

class just_to_see2 = object(self)
  method previous = None
  method child Not_really_though =
    object
      inherit child1 self
      inherit child2
    end
end;;
[%%expect{|
type gadt = Not_really_though : gadt
Line _, characters 14-20:
        inherit child2
                ^^^^^^
Error: The method parent has type
         < child : gadt -> 'a; previous : 'b option; .. >
       but is expected to have type < previous : < .. > option; .. >
       Self type cannot escape its class
|}, Principal{|
type gadt = Not_really_though : gadt
Line _, characters 14-20:
        inherit child2
                ^^^^^^
Error: The method parent has type
         < child : 'a -> 'b; previous : 'c option; .. >
       but is expected to have type < previous : < .. > option; .. >
       Self type cannot escape its class
|}]
