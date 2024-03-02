sbomcc: sbomcc.rkt \
		parser.rkt \
		strings.rkt \
		local.rkt \
		utils.rkt
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

sbomcc-linux-x64: sbomcc.rkt \
				  parser.rkt \
				  strings.rkt \
				  local.rkt \
				  utils.rkt
	raco exe -o sbomcc bogu.rkt
	raco distribute sbomcc-$(SBOMCC_VERSION) sbomcc
	# cp scripts/linux/install.sh install.sh
	zip -r9 sbomcc-$(SBOMCC_VERSION)-linux-x64.zip sbomcc-$(SBOMCC_VERSION)

clean:
	rm sbomcc || true
	rm -rf sbomcc-* || true
	rm -rf build* || true
	# rm install.sh || true
