while (<>) {
   s/%%([0-9_]*)([^1-9\(]*)\(([^%]*)%%\)/ehc($1,$2,$3)/ge;
   print;
}

sub ehc {
   my $ehcVersion = shift;
   my $opt = shift;
   my $src = shift;
   my $repl;
   my $pre = "bin/" ;
   if ( $ehcVersion eq "" && $opt eq "" ) {
     $repl = `$src` ;
   } elsif ( $opt eq "srcfile" ) {
     $repl = `cat '$src'` ;
   } elsif ( $opt eq "ppinline" ) {
     $repl = `echo '$src' | $pre$ehcVersion/ehc --pretty=pp` ;
   } elsif ( $opt eq "astinline" ) {
     $repl = `echo '$src' | $pre$ehcVersion/ehc --pretty=ast` ;
   } elsif ( $opt eq "ppfile" ) {
     $repl = `$pre$ehcVersion/ehc '$src' --pretty=pp` ;
   } elsif ( $opt eq "astfile" ) {
     $repl = `$pre$ehcVersion/ehc '$src' --pretty=ast` ;
   } elsif ( $opt eq "grinfile" ) {
     $repl = `$pre$ehcVersion/ehc '$src' --pretty=grin` ;
   } elsif ( $opt eq "file" ) {
     $repl = `$pre$ehcVersion/ehc '$src' --pretty=no --show-top-ty=yes` ;
   } elsif ( $opt eq "exec" ) {
     $repl = `$src` ;
   } else {
     $repl = `echo '$src' | $pre$ehcVersion/ehc --pretty=no --show-top-ty=yes` ;
   }
   return "" . $repl . "" ;
}


