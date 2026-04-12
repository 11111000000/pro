all: build

build:
	mkdir -p site
	emacs -Q --batch --eval "(progn (package-initialize) (require 'org) (find-file \"src/index.org\") (org-html-export-to-html) (move-file \"src/index.html\" \"site/index.html\"))"
	
clean:
	rm -rf site
	
deploy: build
	git add site
	git commit -m "docs: update site"
	git subtree push --prefix site origin gh-pages