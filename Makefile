sbomcc: sbomcc.rkt \
		parser.rkt \
		strings.rkt \
		local.rkt \
		utils.rkt
	raco exe sbomcc.rkt
	raco distribute build sbomcc

sbomcc-darwin-arm64: sbomcc.rkt \
				   	 parser.rkt \
				  	 strings.rkt \
				  	 local.rkt \
				  	 utils.rkt
	raco exe -o sbomcc sbomcc.rkt
	raco distribute sbomcc-$(SBOMCC_VERSION) bogu
	zip -r9 sbomcc-$(SBOMCC_VERSION)-darwin-arm64.zip sbomcc-$(SBOMCC_VERSION)

sbomcc-linux-x64: sbomcc.rkt \
				  parser.rkt \
				  strings.rkt \
				  local.rkt \
				  utils.rkt
	raco exe -o sbomcc bogu.rkt
	raco distribute sbomcc-$(SBOMCC_VERSION) sbomcc
	zip -r9 sbomcc-$(SBOMCC_VERSION)-linux-x64.zip sbomcc-$(SBOMCC_VERSION)

clean:
	rm sbomcc || true
	rm -rf sbomcc-* || true
	rm -rf build* || true
	# rm install.sh || true
