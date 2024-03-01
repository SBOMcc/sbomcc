sbomcc: sbomcc.rkt \
		parser.rkt \
		strings.rkt
	raco exe sbomcc.rkt
	raco distribute build sbomcc

# bogu-darwin-arm64: bogu.rkt \
# 				   local.rkt \
# 				   parser.rkt \
# 				   rules.rkt \
# 				   scanner.rkt \
# 				   strings.rkt \
# 				   walk.rkt \
# 				   github.rkt \
# 				   github-api.rkt \
# 				   utils.rkt \
# 	  			   format.rkt
# 	raco exe -o bogu bogu.rkt
# 	raco distribute bogu-$(BOGU_VERSION) bogu
# 	# cp scripts/darwin/install.sh install.sh
# 	zip -r9 bogu-$(BOGU_VERSION)-darwin-arm64.zip bogu-$(BOGU_VERSION)

# bogu-linux-x64: bogu.rkt \
# 				local.rkt \
# 				parser.rkt \
# 				rules.rkt \
# 				scanner.rkt \
# 				strings.rkt \
# 				walk.rkt \
# 				github.rkt \
# 				github-api.rkt \
# 				utils.rkt \
# 	  			format.rkt
# 	raco exe -o bogu bogu.rkt
# 	raco distribute bogu-$(BOGU_VERSION) bogu
# 	# cp scripts/linux/install.sh install.sh
# 	zip -r9 bogu-$(BOGU_VERSION)-linux-x64.zip bogu-$(BOGU_VERSION)

clean:
	rm sbomcc || true
	rm -rf sbomcc-* || true
	rm -rf build* || true
	# rm install.sh || true
