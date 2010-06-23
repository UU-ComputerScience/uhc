while (<>) {
   s/%%<([^%]*)%%>/sh($1)/ge;
   print;
}

sub sh {
   my $cmd = shift;
   my $repl = `$cmd` ;
   return "" . $repl . "" ;
}


