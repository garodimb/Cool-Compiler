

class Main inherits IO {

  main() : Int {	
    0 
  };

  out : Int <-
    {
      out_string("2 is Prime.\n");
      2;
    };

  testee : Int <- out; 

  (* Keyword as identifier *)
  let : Int;

  (* Object id as identifier *)
  Stop : Int <- 500;

  m : Object <-
    while true loop 
      {

        testee <- testee + 1;
        divisor <- 2;

        while 
          if testee < divisor * divisor 
            then false 
	  else if testee - divisor*(testee/divisor) = 0 
            then false
            else true
          fi fi     
        loop 
          divisor <- divisor + 1
        pool;        

        if testee < divisor * divisor	
        then 	-- testee has no factors less than sqrt(testee).
          {
            out <- testee;
            out_int(out); 
            out_string(" is prime.\n");
          }
        else
          0
	fi;   	

        if Stop <= testee then 
          "halt".abort()
        else 
          "continue"
        fi;       

      } 
    pool;

};

