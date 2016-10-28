class Main inherits IO {
  main() : SELF_TYPE {
  	
		(	
			let c : Complex <- (new Complex).init(1, 1) in
			  if c.reflect_X().reflect_Y() = c.reflect_0()
			  	then out_string("=)\n")
			  else out_string("=(\n")
			  fi
			  
		)
  };
};

class Complex inherits IO {
    x : Int;
    (* Missing : *)
    y  Int;
    (* This function has errors in declration *)
    init(a , b : Int) : Complex {
			{
					x = a;
					y = b;
					self;
			}
    };

    print() : Object {
			if y = 0
			(* Missing else *)
			then out_int(x)
			fi
    };

    reflect_0() : Complex {
			{
					x = ~x;
					y = ~y;
					self;
			}
    };

    reflect_X() : Complex {
			{
					y = ~y;
					self;
			}
    };

    reflect_Y() : Complex {
			{
					x = ~x;
					self;
			}
    };
};
