test-expect test-regress: test-lists
	@how=`echo $@ | sed -e 's/.*expect.*/exp/' -e 's/.*regress.*/reg/'` ; \
	cd test ; \
	for v in $(VERSIONS) ; \
	do \
	  echo "== version $$v ==" ; \
	  ehc=../$$v/$(EHC) ; \
	  if test -x $$ehc ; \
	  then \
	    for t in `cat $$v.lst` ; \
	    do \
	      tb=`basename $$t .eh` ; \
	      if test -r $$t -a -x $$ehc ; \
	      then \
	        te=$${t}.exp$${v} ; tr=$${t}.reg$${v} ; th=$${t}.$${how}$${v} ; \
	        tc=$${tb}.core ; \
	        rm -f $${tc} ; \
	        $$ehc $$t > $$th 2>&1 ; \
	        if test -r $${tc} ; \
	        then \
	          echo "== code ==" >> $${th} ; \
	          cat $${tc} >> $${th} ; \
	          if test -x ../bin/jc -a -x ../bin/jr ; \
	          then \
	            cp $$t t.eh ; \
	            $$ehc --code=java t.eh > /dev/null ; \
	            rm -f t.class ; \
	            ../bin/jc t &> /dev/null ; \
	            if test -r t.class ; \
	            then \
	              echo "== execution ==" >> $${th} ; \
	              ../bin/jr t >> $${th} 2>&1 ; \
	            fi \
	          fi \
	        fi ; \
	        if test $$tr = $$th -a -r $$te ; \
	        then \
	          echo "-- $$te -- $$th --" | $(INDENT2) ; \
	          if cmp $$te $$th > /dev/null ; \
	          then \
	            echo -n ; \
	          else \
	            diff $$te $$th | $(INDENT4) ; \
	          fi \
	        elif test ! -r $$te ; \
	        then \
	          echo "-- no $$te to compare to" | $(INDENT2) ; \
	        fi \
	      fi \
	    done \
	  else \
	    echo "-- no $$ehc to compile with" | $(INDENT2) ; \
	  fi \
	done
