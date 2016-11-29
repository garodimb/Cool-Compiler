
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class D inherits IO{
	call_me() : IO {
		out_string("This is D")
	};
};

class Main inherits D{
	a : Bool;
	main():IO {
  		{
			out_string("Hello world\n");
			(new SELF_TYPE).call_me();
			a <- (new D) = (new D);
			out_string("\n");
		}
  	};
};
