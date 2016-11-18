class C {
	a : Int;
	b : Bool;
	c : C <- self;
	init(x : Int, y : Bool) : C {
          {
			b <- y;
			c <- new C;
         }
	};
};

Class Main {
	main():C {
		(new C).init(1,true)
	};
};
