SHELL := /bin/bash

# This must be called:
# - `make docker-run` (defaults to current date)
# - `make docker-run "run_args=dt_fin=2022-03-01"` (single date)
# - `make docker-run "run_args=dt_fin=2022-03-01\;dt_ini=2022-03-31"` (date range)
# container runs as the user and group owning the datos folder.
# We set HOME explicitly here so we can know where to place .netrc regardless of the user inside the container
docker-run $(run_args):
	docker run -u `stat -c "%u:%g" $(PWD)/datos` -v $(PWD)/datos:/datos -v $(PWD)/Resultados:/Resultados -v $(HOME)/.netrc:/.netrc:ro --env-file .env -env HOME=/ precip_rionegro $(run_args)

docker-build:
	docker build -t precip_rionegro .
