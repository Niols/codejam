.PHONY: all

all:
	find -name '*.ml' | \
	    while read file; do \
	        echo -n "Compiling $$file... "; \
	        if ocamlopt -o /tmp/a.out "$$file"; then \
	            echo "Success!"; \
	        else \
	            echo "Failure :-("; \
	        fi; \
	    done

clean:
	find -name '*~' -print -delete
	find '(' -regex '.*\.cm[iox]' -o -name '*.o' ')' -print -delete
