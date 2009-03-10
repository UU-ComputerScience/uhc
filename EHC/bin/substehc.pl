while (<>) {
   s/%%([0-9_]*)([^1-9\(]*)\(([^%]*)%%\)/ehc($1,$2,$3)/ge;
   print;
}

sub ehc {
   my $ehcVersion = shift;
   my $opt = shift;
   my $src = shift;
   my $repl;
   my $prev = "install/" ;
   my $postv = "/bin" ;
   if ( $ehcVersion eq "" && $opt eq "" ) {
     $repl = `$src` ;
   } elsif ( $opt eq "srcfile" ) {
     $repl = `cat '$src'` ;
   } elsif ( $opt eq "ppinline" ) {
     $repl = `echo '$src' | $prev$ehcVersion$postv/ehc --pretty=eh` ;
   } elsif ( $opt eq "astinline" ) {
     $repl = `echo '$src' | $prev$ehcVersion$postv/ehc --pretty=ast` ;
   } elsif ( $opt eq "ppfile" ) {
     $repl = `$prev$ehcVersion$postv/ehc '$src' --pretty=eh` ;
   } elsif ( $opt eq "astfile" ) {
     $repl = `$prev$ehcVersion$postv/ehc '$src' --pretty=ast` ;
   } elsif ( $opt eq "grinfile" ) {
     $repl = `$prev$ehcVersion$postv/ehc '$src' --pretty=grin` ;
   } elsif ( $opt eq "file" ) {
     $repl = `$prev$ehcVersion$postv/ehc '$src' --pretty=no --show-top-ty=yes` ;
   } elsif ( $opt eq "exec" ) {
     $repl = `$src` ;
   } else {
     $repl = `echo '$src' | $prev$ehcVersion$postv/ehc --pretty=no --show-top-ty=yes` ;
   }
   return "" . $repl . "" ;
}


