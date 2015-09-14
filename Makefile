
.PHONY: backend
backend:
	stack build

.PHONY: frontend
frontend:
	cd dashboard && npm install && npm run build

.PHONY: release
release: backend frontend
	-@ rm -rf pkgroot ai-challenger-grid
	mkdir -p pkgroot/dashboard/js
	cp -r dashboard/index.html pkgroot/dashboard/index.html
	cp -r dashboard/images pkgroot/dashboard
	cp -r dashboard/css pkgroot/dashboard
	cp dashboard/js/bundle.min.js pkgroot/dashboard/js/bundle.min.js
	cp `stack path --local-install-root`/bin/ai-challenger-grid pkgroot/ai-challenger-grid
	mv pkgroot ai-challenger-grid
	tar czvf ai-challenger-grid.tar.gz ai-challenger-grid


