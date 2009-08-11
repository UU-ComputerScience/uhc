use Time::HiRes qw(gettimeofday tv_interval);

# @dirs = ("nofib/imag/exp3_8");

@dirs = ("nofib/imag/bernouilli", "nofib/imag/digits-of-e1", "nofib/imag/digits-of-e2", "nofib/imag/exp3_8", "nofib/imag/gen-regexps", "nofib/imag/integrate", "nofib/imag/paraffins", "nofib/imag/primes", "nofib/imag/queens", "nofib/imag/rfib-int", "nofib/imag/rfib-integer", "nofib/imag/rfib-double", "nofib/imag/tak", "nofib/imag/wheel-sieve1", "nofib/imag/wheel-sieve2");


@compilers = ("uhc -tbc --cpp", "uhc -tC --cpp", "ghc --make -cpp");
@compilernames = ("uhc -tbc", "uhc -tC", "ghc");


for $dir (@dirs) {
    
    for $j (0,1,2) {
    
        $compiler = $compilers[$j];
    
        printf("%-24s %-10s", $dir, $compilernames[$j]);
    
        system ("rm $dir/Main.exe $dir/Main 2>/dev/null");
        
        $tc0 = [gettimeofday];
        system ("$compiler $dir/Main.hs 1>comp-output 2>comp-errors");
        $tc1 = [gettimeofday];
    
        my $tc = tv_interval $tc0, $tc1;
        printf("Compile %-8g  ", $tc);
    
    
        if (-e "$dir/Main.exe" || -e "$dir/Main") {
    
            for $n (1, 2, 3) {
            
                $invoer = "in$n";
                $uitvoer = "out$n";
            
                open (INVOER, "$dir/$invoer");
                $args = <INVOER>;
                chomp $args;
                close INVOER;
                
                system ("rm run-output run-errors 2>/dev/null");

                $tr0 = [gettimeofday];
                system ("$dir/Main.exe $args  1> run-output 2> run-errors");
                $tr1 = [gettimeofday];
            
                my $tr = tv_interval $tr0, $tr1;
            
                printf("Run%d %-8g ", $n, $tr);
                
                open (INVOER, "run-errors");
                if (<INVOER>) {
                    print "ERROR ";
                }
                else
                {
                    open UITVOER, "run-output";
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


