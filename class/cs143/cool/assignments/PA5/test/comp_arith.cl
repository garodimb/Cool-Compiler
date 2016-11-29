
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main inherits IO{
	sum() : Int {
		1 + (5 + (6 + ( 7 + (8 + (9 * (10 + ( 11 + (12+13))))))))
	};

	main():IO {
  		{
			out_string("Hello world\n");
			out_int(sum());
			out_string("\n");
		}
  	};
};
