class Main inherits IO {
	sum(num1 : Int, num2 : Int) : Int {
		(* Wrong indetifier name *)
		let 99result : Int in
		{
			99result <- num1 + num2;
			99result;
		}
	};

  	main() : Int {	-- main() is an atrophied method so we can parse. 
    	{
    		out_string("Sum is: ");
    		out_int(sum(1,2));
    		out_string("\n");
    		0;
    	}
  	};
};