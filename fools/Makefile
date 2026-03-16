
FORTH=gforth

check:
	prove --shuffle -e "$(FORTH)" \
	$$(find test -type f -name "*.fth")

check-more:
	$(MAKE) --quiet FORTH=gforth check
	$(MAKE) --quiet FORTH=gforth-0.7.3 check
	$(MAKE) --quiet FORTH=swiftforth check
	$(MAKE) --quiet FORTH='vfxforth include' check
	$(MAKE) --quiet FORTH='vfxforth64 include' check

tags:
	find . -name '*.fth' -exec etags {} +
