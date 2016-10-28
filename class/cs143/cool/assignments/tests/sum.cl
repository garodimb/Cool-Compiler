class Main inherits IO {
	sum(num1 : Int, num2 : Int) : Int {
		(* Wrong indetifier name *)
		let result : Int in
		{
			result <- num1 + num2;
			result;
		}
	};

	prompt() : Int
	{
		{
		 out_string("Enter number: ");
		 in_int();
		}
	};

  	main() : Int { 
    	{
    		let num1 : Int <- prompt(), num2 : Int <- prompt() in {
	    		out_string("Sum is: ");
	    		out_int(sum(num1,num2));
	    		out_string("\n");
	    		0;
			};
    	}
  	};
};