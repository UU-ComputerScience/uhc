while (<>) {
   s/%%([1-9]*)([^1-9<]*)<([^%]*)%%>/ehc($1,$2,$3)/ge;
   print;
}

sub ehc {
   my $ehcVersion = shift;
   my $opt = shift;
   my $src = shift;
   my $repl;
   if ( $ehcVersion eq "" && $opt eq "" ) {
     $repl = `$src` ;
   } elsif ( $opt eq "srcfile" ) {
     $repl = `cat '$src'` ;
   } elsif ( $opt eq "ppinline" ) {
     $repl = `echo '$src' | $ehcVersion/ehc --pretty=pp` ;
   } elsif ( $opt eq "astinline" ) {
     $repl = `echo '$src' | $ehcVersion/ehc --pretty=ast` ;
   } elsif ( $opt eq "ppfile" ) {
     $repl = `$ehcVersion/ehc '$src' --pretty=pp` ;
   } elsif ( $opt eq "astfile" ) {
     $repl = `$ehcVersion/ehc '$src' --pretty=ast` ;
   } elsif ( $opt eq "file" ) {
     $repl = `$ehcVersion/ehc '$src' --pretty=no --show-top-ty=yes` ;
   } else {
     $repl = `echo '$src' | $ehcVersion/ehc --pretty=no --show-top-ty=yes` ;
   }
   return "" . $repl . "" ;
}


