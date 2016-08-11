-- This program prints hello world as output
class Main{
	--io : IO <- new IO;
	main() : Int{
		--let io : IO <- new IO in
		{
			--io.out_string("Welcome to COOL wolrd!\n");
			(new IO).out_string("Welcome to COOL wolrd!\n");
			1;
		}
	};
};
