#!/usr/bin/env bash
# Make a temporary directory to deploy the app, copy essential files there
mkdir deploy_temp
cp -R R/ deploy_temp/
cp -R inst/ deploy_temp/
cp app.R DESCRIPTION NAMESPACE deploy.R deploy_temp/

# Move to the temporary folder and deploy
cd deploy_temp/
Rscript deploy.R

# Clean up
cd ..
rm -rf deploy_temp/
