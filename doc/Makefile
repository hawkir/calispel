all: doc.html doc.txt

clean:
	rm -f doc.html doc.txt

doc.html: doc.xml
#	When the stylesheets warn, they print a message but exit without error.
	if [ "$$(xsltproc --output doc.html \
	                  --stringparam section.autolabel 1 \
	                  --stringparam toc.section.depth 4 \
	                  html.xsl doc.xml 2>&1 \
	         | tee /dev/stderr)" ]; then \
		rm -f doc.html; \
		exit 1; \
	fi

doc.txt: doc.html
	w3m -dump doc.html > doc.txt
