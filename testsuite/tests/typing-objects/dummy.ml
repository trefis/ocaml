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

(* This looks all weird... *)

class foo1 = object(self)
  method previous = None
  method child =
    object
      inherit child1 self
      inherit child2
    end
end;;
[%%expect{|
class foo1 : object method child : child2 method previous : child2 option end
|}]

class foo2 = object(self)
  method previous = None
  method child =
    object
      inherit child1 self
    end
end;;
[%%expect{|
Line _, characters 0-116:
  class foo2 = object(self)
    method previous = None
    method child =
      object
        inherit child1 self
      end
  end..
Error: Some type variables are unbound in this type:
         class foo2 :
           object method child : child1 method previous : 'a option end
       The method previous has type 'a option where 'a is unbound
|}]

class foo3 = object(self : 'a)
  method previous : 'a option = None
  method child =
    object
      inherit child1 self
    end
end;;
[%%expect{|
class foo3 :
  object ('a) method child : child1 method previous : 'a option end
|}]

class foo3' = object(self : 'a)
  method previous : 'a option = None
  method child =
    object
      inherit child1 self
      inherit child2
    end
end;;
[%%expect{|
class foo3' :
  object ('a) method child : child1 method previous : 'a option end
|}]

class foo4 = object(self)
  method previous : foo4 option = None
  method child =
    object
      inherit child1 self
    end
end;;
[%%expect{|
class foo4 : object method child : child1 method previous : foo4 option end
|}]

class foo4' = object(self)
  method previous : foo4 option = None
  method child =
    object
      inherit child1 self
      inherit child2
    end
end;;
[%%expect{|
class foo4 : object method child : child1 method previous : foo4 option end
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
class just_to_see :
  object method obj : < child : child2; previous : child2 option > end
|}]

class just_to_see2 = object
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
class just_to_see2 :
  object method obj : < child : child2; previous : child2 option > end
|}]

type gadt = Not_really_though : gadt

class just_to_see3 = object(self)
  method previous = None
  method child Not_really_though =
    object
      inherit child1 self
      inherit child2
    end
end;;
[%%expect{|
type gadt = Not_really_though : gadt
class just_to_see3 :
  object method child : gadt -> child2 method previous : child2 option end
|}]
