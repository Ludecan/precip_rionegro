SHELL := /bin/bash

# This must be called:
# - `make docker-run` (defaults to current date)
# - `make docker-run "run_args=dt_fin=2022-03-01"` (single date)
# - `make docker-run "run_args=dt_fin=2022-03-01\;dt_ini=2022-03-31"` (date range)
# container runs as the user and group owning the datos folder.
# We set HOME explicitly here so we can know where to place .netrc regardless of the user inside the container
docker-run $(run_args):
		docker run -u `stat -c "%u:%g" $(PWD)/datos` -v $(PWD)/datos:/precip_rionegro/datos -v $(PWD)/Resultados:/precip_rionegro/Resultados -v $(HOME)/.netrc:/precip_rionegro/.netrc:ro -v $(PWD)/RCache_unix:/precip_rionegro/RCache_unix --env-file .env --env HOME=/precip_rionegro precip_rionegro $(run_args)

docker-build:
	docker build -t precip_rionegro . 2>&1 | tee build.log
