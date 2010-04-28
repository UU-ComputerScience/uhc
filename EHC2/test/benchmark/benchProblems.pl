use Time::HiRes qw(gettimeofday tv_interval);

# @dirs = ("nofib/imag/exp3_8");
@dirs = ("nofib/imag/exp3_8", "nofib/imag/gen-regexps", "nofib/imag/integrate", "nofib/imag/paraffins", "nofib/imag/primes", "nofib/imag/queens", "nofib/imag/rfib-int", "nofib/imag/rfib-integer", "nofib/imag/rfib-double", "nofib/imag/tak" );


@compilernames = ("ehc99 -tC");


for $dir (@dirs) {

    @compilers = ( "../../install/99/bin/ehc --cpp -tC --no-hi-check --dump-grin-stages=1 --import-path=$dir");

    
    for $j (0) {
    
        $compiler = $compilers[$j];
    
        printf("%-24s %-15s", $dir, $compilernames[$j]);
    
        #system ("rm $dir/*.grin")
        system ("rm $dir/*.hi $dir/*.core $dir/*.o $dir/Main $dir/Main 2>/dev/null");
        
        $tc0 = [gettimeofday];
        system ("$compiler $dir/Main.hs 1> $dir/comp-output$j 2> $dir/comp-errors$j");
        $tc1 = [gettimeofday];
    
        my $tc = tv_interval $tc0, $tc1;
        printf("Compile %-8g  ", $tc);
    
    
        if (-e "$dir/Main") {
    
            for $n (1, 2, 3) {
            
                $invoer = "in$n";
                $uitvoer = "out$n";
            
                open (INVOER, "$dir/$invoer");
                $args = <INVOER>;
                chomp $args;
                close INVOER;
                
                system ("rm $dir/run-output$j-$n $dir/run-errors$j-$n 2>/dev/null");

                $tr0 = [gettimeofday];
                if (-e "$dir/stdin$n" ) {
                    system ("$dir/Main $args  < $dir/stdin$n  1> $dir/run-output$j-$n 2> $dir/run-errors$j-$n");
                } else {
                    system ("$dir/Main $args  1> $dir/run-output$j-$n 2> $dir/run-errors$j-$n");
                }
                $tr1 = [gettimeofday];
            
                my $tr = tv_interval $tr0, $tr1;
            
                printf("Run%d %-8g ", $n, $tr);
                
                open (INVOER, "$dir/run-errors$j-$n");
                if (<INVOER>) {
                    print "ERROR ";
                }
                else
                {
                    open UITVOER, "$dir/run-output$j-$n";
                    open EXPECT,  "$dir/$uitvoer";
                    $uit = <UITVOER>;
                    $ex  = <EXPECT>;
                    close UITVOER;
                    close EXPECT;
                    
                    if ($uit!=$ex) { print "WRONG "; }
                    else { print "OK    "; }
                }
                close INVOER;
                
            }
        }
        else
        {
                print "Compilation error";
        }
            
        print "\n";
    }
    print "\n";
}
