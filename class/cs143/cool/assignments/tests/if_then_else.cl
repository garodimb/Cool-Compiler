class Main inherits IO {
  pal(s : String) : Bool {
			if s.length() = 0
				then true
			else 
				if s.length() = 1
					then true
				else 
					if s.substr(0, 1) = s.substr(s.length() - 1, 1)
						then pal(s.substr(1, s.length() -2))
					fi
				(* Error due to missing fi. This is syntax error and should
					get caught in parser phase and not lexical phase *)
				fi 
			fi
	  };

  i : Int;
  main() : SELF_TYPE {
		{
		    i <- ~1;
			  out_string("Enter a string: ");
			  if pal(in_string())
			  	then out_string("That was a palindrome\n")
			(* Error due to missing fi. This is syntax error and should
					get caught in parser phase and not lexical phase *)
			  fi;
		}
  };
};
